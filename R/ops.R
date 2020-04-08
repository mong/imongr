#' Functions for data operations in imongr
#'
#' @param pool Database connection pool object
#' @param df Data frame of relevant data
#' @return Relevant values from the current environment and database
#' @name ops
#' @aliases get_user_name get_user_groups get_user_data get_user_id
#' get_user_latest_delivery_id md5_checksum delivery_exist_in_db
#' retire_user_deliveries insert_data
NULL



#' @rdname ops
#' @export
get_user_name <- function() {

  user_name <- Sys.getenv("SHINYPROXY_USERNAME")
  if (user_name == "") {
    stop("No user defined!")
  }

  user_name

}


#' @rdname ops
#' @export
get_user_groups <- function() {

  user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")
  if (user_groups == "") {
    stop("No groups defined")
  }

  user_groups
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
insert_data <- function(pool, df) {

  if (delivery_exist_in_db(pool, df)) {
    stop("This delivery has already been made, data exist in database!")
  }

  delivery <- data.frame(latest = 1,
                         md5_checksum = md5_checksum(df),
                         user_id = get_user_id(pool))

  retire_user_deliveries(pool)

  insert_tab(pool, "delivery", delivery)
  did <- get_user_latest_delivery_id(pool)
  df_id <- data.frame(delivery_id = did)
  insert_tab(pool, "data", cbind(df, df_id))

}
