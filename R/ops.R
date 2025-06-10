#' Functions for data operations in imongr
#'
#' @param pool Database connection pool object
#' @param pool_prod A database pool object
#' @param pool_verify A database pool object
#' @param df Data frame of relevant data
#' @param indicator Character vector of indicator ids
#' @param registry_delivery_ids Integer delivery ids
#' @param update Character string of format YYYY-MM-DD providing date until data
#'   are regarded as updated. Default value is NA.
#' @param affirm Character string of format YYYY-MM-DD providing date until data
#' are regarded as final. Default value is NA.
#' @param terms_version Character string providing version stamp of of the terms
#'   accepted when data are published. Default value is NA that will normally
#'   apply for all uploads prior to publishing.
#' @return Relevant values from the current environment and database
#' @name ops
#' @aliases delete_indicator_data delete_registry_data delete_agg_data insert_data
#'   insert_agg_data update_aggdata_delivery
#'   agg_all_data clean_agg_data create_imongr_user update_registry_medfield
#'   update_registry_user update_ind_text update_ind_val
NULL


#' @rdname ops
#' @export
delete_indicator_data <- function(pool, df) {
  if (!"ind_id" %in% names(df)) {
    stop("Data frame has no notion of 'indicator' (id). Cannot delete!")
  }

  ind_context_year <- df |> dplyr::select("ind_id", "context", "year") |> unique()

  for (i in seq_len(nrow(ind_context_year))) {
    query <- paste0("
  DELETE FROM
    data
  WHERE
    ind_id = '", ind_context_year$ind_id[i], "' AND
    context = '", ind_context_year$context[i], "' AND
    year = '", ind_context_year$year[i], "';")

    pool::dbExecute(pool, query)
  }

}


#' @rdname ops
#' @export
delete_agg_data <- function(pool, df) {
  query <- "
DELETE FROM
  agg_data
WHERE
"

  ind <- unique(df$ind_id)
  context <- unique(df$context)
  ic <- paste(paste0("'", ind, "'"), collapse = ", ")
  cc <- paste(paste0("'", context, "'"), collapse = ", ")
  condition <- paste0("
  ind_id IN (", ic, ") AND
  context IN (", cc, ");")

  query <- paste0(query, condition)

  pool::dbExecute(pool, query)
}

#' @rdname ops
#' @export
insert_data_verify <- function(pool, df, update = NA, affirm = NA) {
  delivery <- data.frame(
    md5_checksum = md5_checksum(df),
    latest_update = update,
    latest_affirm = affirm,
    user_id = get_user_id(pool),
    publish_id = NA,
    published = 0
  )

  delete_indicator_data(pool, df)

  insert_table(pool, "delivery", delivery)

  df_id <- pool::dbGetQuery(pool, "SELECT MAX(id) from delivery")

  colnames(df_id) <- "delivery_id"

  df <- dplyr::left_join(df, get_all_orgnr(pool), by = "orgnr")

  insert_table(pool, "data", cbind(df, df_id))
}

#' @rdname ops
#' @export
insert_data_prod <- function(pool_verify, pool_prod, df, registry_delivery_ids, terms_version = NA) {
  # Indicator data for the checksum
  message("Publiserer data")

  ind <- get_table(pool_prod, table = "ind") |>
    dplyr::filter(.data$id %in% unique(df$ind_id))

  # Find the registry id
  reg_id <- ind$registry_id[1]

  # Deliveries to be published
  # Get all deliveries with published = 0 that are referenced
  # in the data table for the current registry
  filter_values <- pool::dbGetQuery(
    pool_verify,
    paste(
      "SELECT DISTINCT data.delivery_id, data.ind_id, data.context, data.year
     FROM data LEFT JOIN ind ON data.ind_id = ind.id
     LEFT JOIN delivery ON data.delivery_id = delivery.id
     WHERE ind.registry_id =", reg_id,
      "AND delivery.published = 0
     ORDER BY data.delivery_id;"
    )
  )

  # To iterate over
  new_delivery_ids <- unique(filter_values$delivery_id)

  message(paste0("Det er ", length(new_delivery_ids), " leveranse(r) klar til publisering"))

  # New row in the publish table in prod
  new_publish <- data.frame(
    md5_checksum = md5_checksum(df, ind),
    terms_version = terms_version,
    user_id = get_user_id(pool_prod),
    registry_id = reg_id
  )

  # Orgnr for the data
  df <- dplyr::left_join(df, get_all_orgnr(pool_prod), by = "orgnr")

  message("Oppdaterer publiseringstabeller")
  # Insert new row in publish and get the row id
  insert_table(pool_prod, "publish", new_publish)
  new_publish_id_prod <- pool::dbGetQuery(pool_prod, "SELECT MAX(id) FROM publish")
  new_publish_id_prod <- new_publish_id_prod$`MAX(id)`

  insert_table(pool_verify, "publish", new_publish)
  new_publish_id_verify <- pool::dbGetQuery(pool_verify, "SELECT MAX(id) FROM publish")
  new_publish_id_verify <- new_publish_id_verify$`MAX(id)`

  # Iterate over deliveries and insert data into prod
  for (delivery_id_i in new_delivery_ids) {
    message("")
    message(paste0("Publiserer leveranse nummer ", delivery_id_i))

    # Filter data to include only the lines uploaded in the delivery
    filter_values_i <- filter_values[filter_values$delivery_id == delivery_id_i, ]

    # Indicator id, context and year are pasted into strings and compared
    filter_vector_i <- paste0(filter_values_i$ind_id, filter_values_i$context, filter_values_i$year)

    keep_inds <- sapply(paste0(df$ind_id, df$context, df$year),
                        FUN = function(x) {
                          x %in% filter_vector_i
                        })

    df_i <- df[keep_inds, ]

    ##### In production #####
    delivery_i <- pool::dbGetQuery(
      pool_verify,
      paste0("SELECT * FROM delivery WHERE id =", delivery_id_i, ";")
    )

    delivery <- data.frame(
      md5_checksum = delivery_i$md5_checksum,
      latest_update = delivery_i$latest_update,
      latest_affirm = delivery_i$latest_affirm,
      user_id = delivery_i$user_id,
      publish_id = new_publish_id_prod,
      published = 1
    )

    message("  Sletter tidligere indikatordata")
    delete_indicator_data(pool_prod, df_i)

    message("  Oppdaterer leveransetabell i prod")
    insert_table(pool_prod, "delivery", delivery)

    df_id <- pool::dbGetQuery(pool_prod, "SELECT MAX(id) from delivery")

    colnames(df_id) <- "delivery_id"

    message("  Setter inn ny data i datatabellen")
    insert_table(pool_prod, "data", cbind(df_i, df_id))

    ##### In verify #####
    query <- paste(
      "UPDATE delivery
        SET published = 1, publish_id =", new_publish_id_verify,
      "WHERE id =", delivery_i$id
    )

    message("  Oppdaterer publiseringstabell i verify")
    pool::dbExecute(pool_verify, query)
  }

  message("Ferdig\n")

}

#' @rdname ops
#' @export
insert_agg_data <- function(pool, df) {
  # data for re-use
  org <- get_flat_org(pool)
  ind <- get_table(pool, "ind")
  all_orgnr <- get_all_orgnr(pool)
  conf <- get_config()
  ## obtain unit name-number mapping needed for none-fraction data when added to
  ## the aggregate
  orgnr_name_map <- get_all_orgnr(pool, include_short_name = TRUE) |>
    dplyr::select("orgnr", "short_name") |>
    dplyr::rename(unit_name = "short_name")


  # make sure we have unit_levels
  if (!"unit_level" %in% names(df)) {
    df <- dplyr::left_join(df, all_orgnr, by = "orgnr")
  }

  # add registry_id to data
  ind_reg <- dplyr::select(get_table(pool, "ind"), "id", "registry_id")
  df <- df |>
    dplyr::left_join(ind_reg, by = c("ind_id" = "id"))

  reg <- unique(df$registry_id)
  for (i in seq_len(length(reg))) {
    message(paste("Register:", get_registry_name(pool, reg[i])))
    dat <- dplyr::filter(df, .data$registry_id == reg[i]) |>
      dplyr::select(-c("registry_id"))
    # if delivery is a subset of registry indicators AND dg is part of subset,
    # agg_data for all indicators of the current registry must be updated.
    if (!setequal(
      get_registry_indicators(pool, reg[i])$id,
      unique(dat$ind_id)
    )) {
      message("  Henter alle indikatorer")
      dat <- get_registry_data(pool, reg[i])
      if (!"unit_level" %in% names(dat)) {
        dat <- dplyr::left_join(dat, all_orgnr, by = "orgnr")
      }
    }
    # identify none-fraction indicators (no aggregation)
    ind_noagg <- indicator_is_fraction(pool, dat, conf, return_ind = TRUE)
    ind_noagg <- ind_noagg$ind[!ind_noagg$is_fraction]

    message("  Aggregerer")
    dat <- agg(dat, org, ind, ind_noagg, orgnr_name_map)
    message("  Sletter gammel aggregert data")
    delete_agg_data(pool, dat)
    message("  Henter ny aggregert data")
    insert_table(pool, "agg_data", dat)
  }
  message("\nOppdaterer leveransetidspunkt")
  # Get all indicator ids from delivered registries,
  # since all dates are removed from agg_data in the loop above.
  all_ind_id <- dplyr::filter(ind_reg, .data$registry_id %in% reg)$id
  update_aggdata_delivery(pool, all_ind_id)
  message("Ferdig\n")
}

#' @rdname ops
#' @export
update_aggdata_delivery <- function(pool, indicator) {
  delivery <- get_aggdata_delivery(pool, indicator)

  pool::dbWriteTable(pool,
    name = "temp_agg_data", value = delivery,
    temporary = TRUE
  )

  query <- paste0("
UPDATE
  agg_data a, temp_agg_data t
SET
  a.delivery_time = t.delivery_time,
  a.delivery_latest_update = t.delivery_latest_update,
  a.delivery_latest_affirm = t.delivery_latest_affirm
WHERE
  a.id = t.id;")

  pool::dbExecute(pool, query)
  pool::dbRemoveTable(pool, name = "temp_agg_data", temporary = TRUE)
}

#' @rdname ops
#' @export
agg_all_data <- function(pool) {
  insert_agg_data(pool, get_table(pool, "data"))
}

#' @rdname ops
#' @export
clean_agg_data <- function(pool) {
  message("Start cleaning agg_data")

  # remove data for indicators not present in ind table
  global_indicator <- get_table(pool, "ind")$id
  agg_data_indicator <- unique(get_table(pool, "agg_data")$ind_id)
  orphant_indicator <- setdiff(agg_data_indicator, global_indicator)
  if (length(orphant_indicator > 0)) {
    orphant <- paste0("'", orphant_indicator, "'")
    orphant <- paste0(orphant, collapse = ", ")
    message(paste("... found orphant indicators:", orphant))
    query <- paste0("
DELETE FROM
  agg_data
WHERE
  ind_id IN (", orphant, ");")
    message("...deleting orphant indicators from agg_data table")
    pool::dbExecute(pool, query)
  } else {
    message("No mess, nothing to do")
  }
  message("Done!")
}


#' @rdname ops
#' @export
create_imongr_user <- function(pool, df) {
  user <- get_user(pool)
  if (df$user_name %in% user$user_name) {
    return("User already exists. Nothing to do!")
  } else {
    msg <- insert_table(pool, "user", df)
    return(msg)
  }
}

#' @rdname ops
#' @param pool Database pool object
#' @param registry_id The numeric id of the selected registry
#' @param df A data frame with new user medfield for overwriting the old medfield list
#' @export
update_registry_medfield <- function(pool, registry_id, df) {
  # Delete all medfields from the selected registry
  query <- paste0("
DELETE FROM
  registry_medfield
WHERE
  registry_id=", registry_id, ";")

  pool::dbExecute(pool, query)

  # Add medfields to the registry if available
  if (nrow(df) > 0) {
    pool::dbWriteTable(pool, "registry_medfield", df,
      append = TRUE,
      row.names = FALSE
    )
  }
}

#' @rdname ops
#' @param pool Database pool object
#' @param registry_id The numeric id of the selected registry
#' @param df A data frame with new user data for overwriting the old user list
#' @export
update_registry_user <- function(pool, registry_id, df) {
  # Delete all users from the selected registry
  query <- paste0("
DELETE FROM
  user_registry
WHERE
  registry_id=", registry_id, ";")

  pool::dbExecute(pool, query)

  # Add new users if available
  if (nrow(df) > 0) {
    pool::dbWriteTable(pool, "user_registry", df,
      append = TRUE,
      row.names = FALSE
    )
  }
}

#' @rdname ops
#' @param pool Database pool object
#' @param pool_verify Database pool object
#' @param conf The data from the get_config function
#' @param rv A shiny::reactiveValues object
#' @noRd
add_project <- function(input, rv, pool, pool_verify) {
  query <- paste0("INSERT INTO project (id, registry_id, start_year) VALUES ( '",
    rv$new_project_name,
    "', '",
    input$project_registry,
    "', '",
    input$new_project_start_year,
    "');"
  )

  pool::dbExecute(pool, query)
  pool::dbExecute(pool_verify, query)
}

#' @rdname ops
#' @param pool Database pool object
#' @param project_id String
#' @param indicator_id String
#' @noRd
add_project_to_indicator <- function(pool, project_id, indicator_id) {
  query <- paste0("
    INSERT INTO
      project_ind
      (project_id, ind_id)
    VALUES ('",
    project_id,
    "','",
    indicator_id,
    "');"
  )

  pool::dbExecute(pool, query)
}

#' @rdname ops
#' @param pool Database pool object
#' @param registry_id The numeric id of the selected project
#' @param df A data frame with new hospital data for overwriting the old hospital list
#' @noRd
update_project_hospitals <- function(pool, project_id, new_data) {
  # Delete all hospitals from the selected project
  query <- paste0("
DELETE FROM
  project_hospital
WHERE
  project_id='", project_id, "';")

  pool::dbExecute(pool, query)

  # Add new hospitals if available
  if (nrow(new_data) > 0) {
    pool::dbWriteTable(pool, "project_hospital", new_data,
      append = TRUE,
      row.names = FALSE
    )
  }
}

#' @rdname ops
#' @param pool Database pool object
#' @param ind_id The id of the selected indicator
#' @param df A data frame with new unit data for overwriting the old rows
#'
#' This function is intended for the selected indicators tab,
#'
#' @noRd
update_ind_units <- function(pool, ind_id, new_data) {
  # Delete all hospitals from the selected project
  query <- paste0("
DELETE FROM
  unit_ind
WHERE
  ind_id='", ind_id, "';")

  pool::dbExecute(pool, query)

  # Add new rows if available
  if (nrow(new_data) > 0) {
    pool::dbWriteTable(pool, "unit_ind", new_data,
      append = TRUE,
      row.names = FALSE
    )
  }
}

#' Update the description text from the main panel input
#' in the indicator tab
#'
#' @rdname ops
#' @noRd
update_ind_text <- function(pool, df) {

  message("Oppdaterer indikatortekst")

  query <- paste0("
UPDATE
  ind
SET
  title = ?,
  short_description = ?,
  long_description = ?
WHERE
  id = ?;")

  params <- list(
    df$title,
    df$short_description,
    df$long_description,
    df$id
  )

  con <- pool::poolCheckout(pool)
  rs <- DBI::dbSendQuery(con, query)
  DBI::dbBind(rs, params)
  DBI::dbClearResult(rs)
  pool::poolReturn(con)

  message("Ferdig\n")
}

#' Updatet the description text from the main panel input
#' in the project tab
#'
#' @rdname ops
#' @noRd
update_project_text <- function(pool, new_data) {

  message("Oppdaterer prosjekttekst")

  query <- paste0("
UPDATE
  project
SET
  title = ?,
  short_description = ?,
  long_description = ?
WHERE
  id = ?;")

  params <- list(
    new_data$title,
    new_data$short_description,
    new_data$long_description,
    new_data$id
  )

  con <- pool::poolCheckout(pool)
  rs <- DBI::dbSendQuery(con, query)
  DBI::dbBind(rs, params)
  DBI::dbClearResult(rs)
  pool::poolReturn(con)

  message("Ferdig\n")
}

#' Update indicator parameters from in the indicator tab sidebar
#'
#' @rdname ops
#' @noRd
update_ind_val <- function(pool, df) {

  message("Oppdaterer indikatorverdier")

  query <- paste0("
UPDATE
  ind
SET
  include = ?,
  dg_id = ?,
  level_direction = ?,
  level_green = ?,
  level_yellow = ?,
  min_denominator = ?,
  type = ?,
  sformat = ?
WHERE
  id = ?;")

  params <- list(
    df$include,
    df$dg_id,
    df$level_direction,
    df$level_green,
    df$level_yellow,
    df$min_denominator,
    df$type,
    df$sformat,
    df$id
  )

  con <- pool::poolCheckout(pool)
  rs <- DBI::dbSendQuery(con, query)
  DBI::dbBind(rs, params)
  DBI::dbClearResult(rs)
  pool::poolReturn(con)

  message("Ferdig\n")
}

#' Update the project info with input from the sidebar menu
#' in the project tab
#' @rdname ops
#' @noRd
update_project_val <- function(pool, new_data) {

  message("Oppdaterer prosjektverdier")

  query <- paste0("
UPDATE
  project
SET
  start_year = ?,
  end_year = ?
WHERE
  id = ?;")

  params <- list(
    new_data$start_year,
    new_data$end_year,
    new_data$id
  )

  con <- pool::poolCheckout(pool)
  rs <- DBI::dbSendQuery(con, query)
  DBI::dbBind(rs, params)
  DBI::dbClearResult(rs)
  pool::poolReturn(con)

  message("Ferdig\n")
}

#' Update the review for a given registry and year with input from the checkbox form
#' in the review tab
#'
#' @rdname ops
#' @noRd
update_review <- function(pool, df, registry_id, year) {

  message("Oppdaterer vurdering")

  query <- paste0("
    DELETE FROM
      evaluation
    WHERE
      registry_id = ", registry_id, " AND year = ", year, ";")

  pool::dbExecute(pool, query)

  pool::dbWriteTable(pool, "evaluation", df,
    append = TRUE,
    row.names = FALSE
  )

  message("Ferdig\n")
}

#' Get notice id for a given registry and year
#'
#' @rdname ops
#' @noRd
get_notice_id <- function(pool, registry_id, year) {

  query <- paste0("
    SELECT notice FROM evaluation
    WHERE registry_id = ", registry_id, "
    AND year = ", year, ";
  ")

  pool::dbGetQuery(pool, query)
}

#' Get notice for a given registry and year
#'
#' @rdname ops
#' @noRd
get_notice <- function(pool, id) {
  query <- paste0("
    SELECT * FROM notice
    WHERE id = ", id, ";
  ")

  pool::dbGetQuery(pool, query)
}

#' Update notice for a given registry and year
#'
#' @rdname ops
#' @noRd
update_notice <- function(pool, id, text) {
  query <- paste0("
    UPDATE notice
    set text = '", text, "' WHERE id = ", id, ";
  ")

  pool::dbGetQuery(pool, query)
}