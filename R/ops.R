#' Functions for data operations in imongr
#'
#' @param pool Database connection pool object
#' @param df Data frame of relevant data
#' @param ind Data frame of relevant indicator data
#' @param registry Integer registry id
#' @param update Character string of format YYYY-MM-DD providing date until data
#'   are regarded as updated. Default value is NA.
#' @param affirm Character string of format YYYY-MM-DD providing date until data
#' are regarded as final. Default value is NA.
#' @param terms_version Character string providing version stamp of of the terms
#'   accepted when data are published. Default value is NA that will normally
#'   apply for all uploads prior to publishing.
#' @return Relevant values from the current environment and database
#' @name ops
#' @aliases duplicate_delivery retire_user_deliveries
#'   delete_indicator_data delete_registry_data delete_agg_data insert_data
#'   insert_agg_data update_aggdata_delivery update_aggdata_delivery_time
#'   agg_all_data clean_agg_data create_imongr_user update_registry_medfield
#'   update_registry_user update_ind_text update_ind_val
NULL


#' @rdname ops
#' @export
duplicate_delivery <- function(pool, df, ind, registry) {

  query <- "
SELECT
  md5_checksum
FROM
  delivery
WHERE
  latest=1;"

  dat <- pool::dbGetQuery(pool, query)

  if (md5_checksum(df, ind) %in% dat$md5_checksum) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @rdname ops
#' @export
retire_user_deliveries <- function(pool) {

  query <- paste0("
UPDATE
  delivery
SET
  latest=0
WHERE
  user_id=", get_user_id(pool), ";")

  pool::dbExecute(pool, query)
}

#' @rdname ops
#' @export
delete_indicator_data <- function(pool, df) {

  if (!"ind_id" %in% names(df)) {
    stop("Data frame has no notion of 'indicator' (id). Cannot delete!")
  }
  ind <- unique(df$ind_id)
  context <- unique(df$context)

  query <- paste0("
DELETE FROM
  data
WHERE
  ind_id IN ('", paste0(ind, collapse = "', '"), "') AND
  context IN ('", paste0(context, collapse = "', '"), "');")

  pool::dbExecute(pool, query)
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
  context IN (", cc, ");"
  )

  query <- paste0(query, condition)

  pool::dbExecute(pool, query)
}


#' @rdname ops
#' @export
insert_data <- function(pool, df, update = NA, affirm = NA,
                        terms_version = NA) {

  ind <- get_table(pool, table = "ind") %>%
    dplyr::filter(.data$id %in% unique(df$ind_id))

  delivery <- data.frame(latest = 1,
                         md5_checksum = md5_checksum(df, ind),
                         latest_update = update,
                         latest_affirm = affirm,
                         terms_version = terms_version,
                         user_id = get_user_id(pool))

  delete_indicator_data(pool, df)
  retire_user_deliveries(pool)

  insert_table(pool, "delivery", delivery)
  did <- get_user_latest_delivery_id(pool)
  df_id <- data.frame(delivery_id = did)

  df <- dplyr::left_join(df, get_all_orgnr(pool), by = "orgnr")

  insert_table(pool, "data", cbind(df, df_id))

}

#' @rdname ops
#' @export
get_registry_data <- function(pool, registry) {

  conf <- get_config()
  fields <- paste(conf$db$tab$data$insert[conf$upload$data_var_ind],
                  collapse = ",\n  ")

  query <- paste0("
SELECT
  d.", fields, "
FROM
  data d
LEFT JOIN
  ind i
ON
  d.ind_id = i.id
WHERE
  i.registry_id=", registry, ";")

  pool::dbGetQuery(pool, query)

}

#' @rdname ops
#' @export
get_delivery_data <- function(pool, delivery) {

  conf <- get_config()
  fields <- paste(conf$db$tab$data$insert[conf$upload$data_var_ind],
                  collapse = ",\n  ")

  query <- paste0("
SELECT
  d.", fields, "
FROM
  data d
LEFT JOIN
  ind i
ON
  d.ind_id = i.id
WHERE
  d.delivery_id = '", delivery, "';")

  pool::dbGetQuery(pool, query)

}

#' @rdname ops
#' @export
insert_data_prod <- function(pool, pool_verify, publish_deliveries, previous_deliveries, terms_version) {
  # df may contain more than one delivery
  current_delivery_ids <- sort(unique(publish_deliveries$delivery_id))
  previous_delivery_ids <- sort(unique(previous_deliveries$delivery_id))

  delivery_ids = current_delivery_ids[!current_delivery_ids %in% previous_delivery_ids]

  for (i in delivery_ids) {
    publish_data = get_delivery_data(pool_verify, i)
    publish_data_reorder = publish_data[,c(6, 3, 2, 4, 5, 1)]

    ind <- get_table(pool, table = "ind") %>%
      dplyr::filter(.data$id %in% unique(publish_data$ind_id))

    csum <- md5_checksum(publish_data_reorder, ind)

    # Fetch delivery from imongr-verify
    delivery <- pool::dbGetQuery(pool_verify, paste0("SELECT * FROM delivery WHERE id = '", i, "';"))

    # TODO:
    # Here the csum value should be compared with delivery$md5_checksum
    # For now, assume that they are equal

    delete_indicator_data(pool, publish_data)

    # The "latest" column doesn't come into play here, so skip updating its value

    insert_table(pool, "delivery", delivery[, c(-1, -3)]) # Strip off the id and time columns

    # TODO: append delivery_id and unit_level
    df_id <- data.frame(delivery_id = rep(i, nrow(publish_data)))

    publish_data <- dplyr::left_join(publish_data, get_all_orgnr(pool), by = "orgnr")
    publish_data = cbind(publish_data, df_id)

    insert_table(pool, "data", publish_data)
    insert_agg_data(pool, publish_data)
  }

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
  orgnr_name_map <- get_all_orgnr(pool, include_short_name = TRUE) %>%
    dplyr::select("orgnr", "short_name") %>%
    dplyr::rename(unit_name = .data$short_name)


  # make sure we have unit_levels
  if (!"unit_level" %in% names(df)) {
    df <- dplyr::left_join(df, all_orgnr, by = "orgnr")
  }

  # add registry_id to data
  ind_reg <- dplyr::select(get_table(pool, "ind"), "id", "registry_id")
  df <- df %>%
    dplyr::left_join(ind_reg, by = c("ind_id" = "id"))

  reg <- unique(df$registry_id)
  for (i in seq_len(length(reg))) {
    message(paste("Processing", get_registry_name(pool, reg[i])))
    dat <- dplyr::filter(df, .data$registry_id == reg[i]) %>%
      dplyr::select(-c("registry_id"))
    # if delivery is a subset of registry indicators AND dg is part of subset,
    # agg_data for all indicators of the current registry must be updated.
    if (!setequal(get_registry_indicators(pool, reg[i])$id,
                  unique(dat$ind_id))) {
      message("  subset of indicators provided, fetching a compleete data set")
      dat <- get_registry_data(pool, reg[i])
      if (!"unit_level" %in% names(dat)) {
        dat <- dplyr::left_join(dat, all_orgnr, by = "orgnr")
      }
    }
    # identify none-fraction indicators (no aggregation)
    ind_noagg <- indicator_is_fraction(pool, dat, conf, return_ind = TRUE)
    ind_noagg <- ind_noagg$ind[!ind_noagg$is_fraction]

    message("  aggregating")
    dat <- agg(dat, org, ind, ind_noagg, orgnr_name_map)
    message("  delete old agg data")
    delete_agg_data(pool, dat)
    message("  inserting fresh agg data")
    insert_table(pool, "agg_data", dat)
  }
  message("\nUpdating delivery timings")
  update_aggdata_delivery(pool)
  message("Done!")
}

#' @rdname ops
#' @export
update_aggdata_delivery <- function(pool) {
  delivery <- get_aggdata_delivery(pool)

  pool::dbWriteTable(pool, name = "temp_agg_data", value = delivery,
                     temporary = TRUE)

  query <- paste0("
UPDATE
  agg_data a, temp_agg_data t
SET
  a.delivery_time = t.delivery_time,
  a.delivery_latest_update = t.delivery_latest_update,
  a.delivery_latest_affirm = t.delivery_latest_affirm
WHERE
  a.id = t.id;"
  )

  pool::dbExecute(pool, query)
  pool::dbRemoveTable(pool, name = "temp_agg_data", temporary = TRUE)
}


#' @rdname ops
#' @export
update_aggdata_delivery_time <- function(pool) {
  lifecycle::deprecate_stop(
    "0.27.0", "imongr::update_aggdata_delivery_time()",
    "imongr::update_aggdata_delivery()"
  )

  delivery_time <- get_aggdata_delivery_time(pool)

  pool::dbWriteTable(pool, name = "temp_agg_data", value = delivery_time,
                      temporary = TRUE)

  query <- paste0("
UPDATE
  agg_data a, temp_agg_data t
SET
  a.delivery_time = t.delivery_time
WHERE
  a.id = t.id;"
  )

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
  ind_id IN (", orphant, ");"
    )
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
#' @export
update_registry_medfield <- function(pool, df) {

  query <- paste0("
DELETE FROM
  registry_medfield
WHERE
  registry_id IN (", paste0(df$registry_id, collapse = ", "), ");")

  pool::dbExecute(pool, query)

  pool::dbWriteTable(pool, "registry_medfield", df, append = TRUE,
                     row.names = FALSE)
}

#' @rdname ops
#' @export
update_registry_user <- function(pool, df) {

  query <- paste0("
DELETE FROM
  user_registry
WHERE
  registry_id IN (", paste0(df$registry_id, collapse = ", "), ");")

  pool::dbExecute(pool, query)

  pool::dbWriteTable(pool, "user_registry", df, append = TRUE,
                     row.names = FALSE)
}

#' @rdname ops
#' @export
update_ind_text <- function(pool, df) {

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

}

#' @rdname ops
#' @export
update_ind_val <- function(pool, df) {

  query <- paste0("
UPDATE
  ind
SET
  include = ?,
  level_direction = ?,
  level_green = ?,
  level_yellow = ?,
  min_denominator = ?,
  type = ?
WHERE
  id = ?;")

  params <- list(
    df$include,
    df$level_direction,
    df$level_green,
    df$level_yellow,
    df$min_denominator,
    df$type,
    df$id
  )

  con <- pool::poolCheckout(pool)
  rs <- DBI::dbSendQuery(con, query)
  DBI::dbBind(rs, params)
  DBI::dbClearResult(rs)
  pool::poolReturn(con)

}
