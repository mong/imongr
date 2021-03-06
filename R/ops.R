#' Functions for data operations in imongr
#'
#' @param pool Database connection pool object
#' @param df Data frame of relevant data
#' @return Relevant values from the current environment and database
#' @name ops
#' @aliases delivery_exist_in_db duplicate_delivery retire_user_deliveries
#' delete_indicator_data delete_registry_data delete_agg_data insert_data
#' insert_agg_data update_aggdata_delivery_time agg_all_data clean_agg_data
#' create_imongr_user update_registry_medfield update_registry_user
NULL


#' @rdname ops
#' @export
delivery_exist_in_db <- function(pool, df) {

  query <- "
SELECT
  md5_checksum
FROM
  delivery;"

  dat <- pool::dbGetQuery(pool, query)

  if (md5_checksum(df) %in% dat$md5_checksum) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @rdname ops
#' @export
duplicate_delivery <- function(pool, df) {

  query <- "
SELECT
  md5_checksum
FROM
  delivery
WHERE
  latest=1;"

  dat <- pool::dbGetQuery(pool, query)

  if (md5_checksum(df) %in% dat$md5_checksum) {
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
insert_data <- function(pool, df) {

  delivery <- data.frame(latest = 1,
                         md5_checksum = md5_checksum(df),
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
insert_agg_data <- function(pool, df) {

  # data for re-use
  org <- get_flat_org(pool)
  ind <- get_table(pool, "ind")
  all_orgnr <- get_all_orgnr(pool)

  #make sure we have unit_names
  if (!"unit_level" %in% names(df)) {
    df <- dplyr::left_join(df, all_orgnr, by = "orgnr")
  }

  # add registry_id to data
  ind_reg <- dplyr::select(get_table(pool, "ind"), .data$id, .data$registry_id)
  df <- df %>%
    dplyr::left_join(ind_reg, by = c("ind_id" = "id"))

  reg <- unique(df$registry_id)
  for (i in seq_len(length(reg))) {
    message(paste("Aggregating", get_registry_name(pool, reg[i])))
    dat <- dplyr::filter(df, .data$registry_id == reg[i]) %>%
      dplyr::select(!.data$registry_id)
    # if delivery is a subset of registry indicators AND dg is part of subset,
    # agg_data for all indicators of the current registry must be updated.
    if (!setequal(get_registry_indicators(pool, reg[i])$id,
                  unique(dat$ind_id))) {
      message("...subset provided, fetching a compleete data set")
      dat <- get_registry_data(pool, reg[i])
      if (!"unit_level" %in% names(dat)) {
        dat <- dplyr::left_join(dat, all_orgnr, by = "orgnr")
      }
    }
    message("...aggregating")
    dat <- agg(dat, org, ind)
    message("...delete old agg data")
    delete_agg_data(pool, dat)
    message("...inserting fresh agg data")
    insert_table(pool, "agg_data", dat)
  }
  message("\nUpdating time of delivery")
  update_aggdata_delivery_time(pool)
  message("Done!")
}

#' @rdname ops
#' @export
update_aggdata_delivery_time <- function(pool) {

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
