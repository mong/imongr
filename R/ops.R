#' Functions for data operations in imongr
#'
#' @param pool Database connection pool object
#' @param df Data frame of relevant data
#' @param registry Integer registry id
#' @return Relevant values from the current environment and database
#' @name ops
#' @aliases get_user_data get_user_id
#' get_user_latest_delivery_id get_registry_data md5_checksum
#' delivery_exist_in_db retire_user_deliveries delete_registry_data
#' delete_agg_data insert_data insert_agg_data
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
}


#' @rdname ops
#' @export
get_user_deliveries <- function(pool) {

  conf <- get_config()

  query <- paste0("
SELECT
  del.time AS Dato,
  del.time AS Tid,
  reg.name AS Register,
  SUBSTRING(del.md5_checksum, 1, 7) as Referanse
FROM
  (SELECT DISTINCT delivery_id, registry_id FROM data) AS dat
LEFT JOIN
  delivery del
ON
  del.id=dat.delivery_id
LEFT JOIN
  registry reg
ON
  dat.registry_id=reg.id
WHERE
  del.user_id=", get_user_id(pool), "
ORDER BY
  del.time DESC;")

  df <- pool::dbGetQuery(pool, query)

  # timestamp in db is UTC, convert back to "our" time zone
  df$Dato <- format(df$Dato, format = conf$app_text$format$date, # nolint
                    tz = Sys.getenv("TZ"))
  df$Tid <- format(df$Tid, format = conf$app_text$format$time, # nolint
                   tz = Sys.getenv("TZ"))

  df
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
  ", fields, "
FROM
  data
WHERE
  registry_id=", registry, ";")

  pool::dbGetQuery(pool, query)

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

  if (delivery_exist_in_db(pool, df)) {
    stop("This delivery has already been made, data exist in database!")
  }

  delivery <- data.frame(latest = 1,
                         md5_checksum = md5_checksum(df),
                         user_id = get_user_id(pool))

  delete_registry_data(pool, df)
  retire_user_deliveries(pool)

  insert_tab(pool, "delivery", delivery)
  did <- get_user_latest_delivery_id(pool)
  df_id <- data.frame(delivery_id = did)
  insert_tab(pool, "data", cbind(df, df_id))

}


#' @rdname ops
#' @export
insert_agg_data <- function(pool, df) {

  df <- agg(df, get_flat_org(pool), get_table(pool, "indicator"))
  delete_agg_data(pool, df)
  insert_tab(pool, "agg_data", df)
}
