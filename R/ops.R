#' Functions for data operations in imongr
#'
#' @param pool Database connection pool object
#' @param df Data frame of relevant data
#' @param registry Integer registry id
#' @param indicator Character vector of indicator ids
#' @return Relevant values from the current environment and database
#' @name ops
#' @aliases get_user_data get_user_id get_user_registries
#' get_user_registry_select
#' get_user_latest_delivery_id get_registry_data get_indicators_registry
#' md5_checksum delivery_exist_in_db duplicate_delivery retire_user_deliveries
#' delete_indicator_data delete_registry_data
#' delete_agg_data insert_data insert_agg_data agg_all_data
NULL


#' @rdname ops
#' @export
get_all_user_data <- function(pool) {

  query <- paste0("
SELECT
  *
FROM
  user
WHERE
  user_name='", get_user_name(), "';")

  pool::dbGetQuery(pool, query)
}


#' @rdname ops
#' @export
get_user_data <- function(pool) {

  query <- paste0("
SELECT
  *
FROM
  user
WHERE
  valid = 1 AND
  user_name='", get_user_name(), "';")

  pool::dbGetQuery(pool, query)
}


#' @rdname ops
#' @export
get_user_id <- function(pool) {

  df <- get_user_data(pool)

  if (dim(df)[1] == 0) {
    stop("No data on the current user!")
  }

  df$id
}


#' @rdname ops
#' @export
get_user_registries <- function(pool) {

  valid_user <- nrow(get_user_data(pool)) > 0

  if (valid_user) {
    query <- paste0("
SELECT
  r.name
FROM
  user_registry ur
LEFT JOIN
  registry r
ON
  ur.registry_id=r.id
WHERE
  ur.user_id=", get_user_id(pool), ";")

    pool::dbGetQuery(pool, query)[, 1]
  } else {
    NULL
  }
}


#' @rdname ops
#' @export
get_user_registry_select <- function(pool) {

  query <- paste0("
SELECT
  r.name AS name,
  r.id AS value
FROM
  user_registry ur
LEFT JOIN
  registry r
ON
  ur.registry_id=r.id
WHERE
  ur.user_id=", get_user_id(pool), "
ORDER BY name;"
  )

  tibble::deframe(pool::dbGetQuery(pool, query))
}

#' @rdname ops
#' @export
get_user_deliveries <- function(pool) {

  valid_user <- nrow(get_user_data(pool)) > 0

  if (valid_user) {
    conf <- get_config()

    query <- paste0("
SELECT
  delivery.time AS Dato,
  delivery.time AS Tid,
  SUBSTRING(delivery.md5_checksum, 1, 7) as Referanse,
  GROUP_CONCAT(DISTINCT data.ind_id SEPARATOR ',\n') AS Indikatorer
FROM
  data
LEFT JOIN
  delivery
ON
  data.delivery_id=delivery.id
WHERE
  delivery.user_id=", get_user_id(pool), "
GROUP BY
  data.delivery_id
ORDER BY
  delivery.time DESC;")

    df <- pool::dbGetQuery(pool, query)

    # timestamp in db is UTC, convert back to "our" time zone
    df$Dato <- format(df$Dato, format = conf$app_text$format$date, # nolint
                      tz = Sys.getenv("TZ"))
    df$Tid <- format(df$Tid, format = conf$app_text$format$time, # nolint
                     tz = Sys.getenv("TZ"))

    df
  } else {
    NULL
  }
}

#' @rdname ops
#' @export
get_user_latest_delivery_id <- function(pool) {

  query <- paste0("
SELECT
  id
FROM
  delivery
WHERE
  latest=1 AND
  user_id=", get_user_id(pool), ";")

  df <- pool::dbGetQuery(pool, query)
  df$id
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
get_indicators_registry <- function(pool, indicator) {

  query <- paste0("
SELECT
  DISTINCT registry_id AS rid
FROM
  ind
WHERE
  id IN ('", paste0(indicator, collapse = "', '"), "');"
  )

  pool::dbGetQuery(pool, query)$rid
}

#' @rdname ops
#' @export
md5_checksum <- function(df) {

  fn <- tempfile()
  utils::write.csv(df, file = fn)
  fc <- file(fn, "r")
  t <- readLines(fc)
  close(fc)
  digest::digest(paste0(t, collapse = ""), algo = "md5", serialize = FALSE)

}


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

  query <- paste0("
DELETE FROM
  data
WHERE
  ind_id IN ('", paste0(ind, collapse = "', '"), "');")

  pool::dbExecute(pool, query)
}


#' @rdname ops
#' @export
delete_registry_data <- function(pool, df) {

  if (!"registry_id" %in% names(df)) {
    stop("Data frame has no notion of 'registry' (id). Cannot go on!")
  }
  reg <- unique(df$registry_id)
  if (length(reg) > 1) {
    stop("Data can only represent one registry. Cannot go on!")
  }

  query <- paste0("
DELETE FROM
  data
WHERE
  registry_id=", reg, ";")

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

  ind <- levels(as.factor(df$ind_id))
  condition <- paste(paste0("'", ind, "'"), collapse = ", ")
  condition <- paste0("ind_id IN (", condition, ");")

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

  insert_tab(pool, "delivery", delivery)
  did <- get_user_latest_delivery_id(pool)
  df_id <- data.frame(delivery_id = did)

  df <- dplyr::left_join(df, get_all_orgnr(pool), by = "orgnr")

  insert_tab(pool, "data", cbind(df, df_id))

}


#' @rdname ops
#' @export
insert_agg_data <- function(pool, df) {

  # data for re-use
  org <- get_flat_org(pool)
  ind <- get_table(pool, "indicator")
  all_orgnr <- get_all_orgnr(pool)

  #make sure we have unit_names
  if (!"unit_level" %in% names(df)) {
    df <- dplyr::left_join(df, all_orgnr, by = "orgnr")
  }

  # add registry_id to data
  ind_reg <- dplyr::select(get_indicator(pool), .data$id, .data$registry_id)
  df <- df %>%
    dplyr::left_join(ind_reg, by = c("ind_id" = "id"))

  reg <- unique(df$registry_id)
  for (i in seq_len(length(reg))) {
    message(paste("Aggregating", get_registry_name(pool, reg[i])))
    dat <- dplyr::filter(df, .data$registry_id == reg[i]) %>%
      dplyr::select(!.data$registry_id)
    # if delivery is a subset of registry indicators AND dg is part of subset,
    # agg_data for all indicators of the current registry must be updated.
    if (!setequal(get_registry_indicators(pool, reg[i]), dat$ind_id)) {
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
    insert_tab(pool, "agg_data", dat)
  }
  message("Done!")
}

#' @rdname ops
#' @export
agg_all_data <- function(pool) {

  insert_agg_data(pool, get_all_data(pool))
}
