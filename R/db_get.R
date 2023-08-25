#' Retreiv data from imongr database
#'
#' Database metadata are read from config. If one or more of these are
#' defined 'env' corresponding values will be fetched from the environmental
#' variables IMONGR_DB_HOST, IMONGR_DB_NAME, IMONGR_DB_USER and IMONGR_DB_PASS.
#' If these are undefined the function providing connection handles will exit
#' with an error. What the function expects from table names and variable
#' names in the data is also defined by the configuration. Thus, functions can
#' be left unchanged when the data model is altered as long as such changes
#' are reflected by configuration.
#'
#' \code{get_aggdata_delivery_time()} provides a data.frame with the variables
#' \emph{id} and \emph{delivery_time} where the first one corresponds to the id
#' field in the agg_data table. The latter is a db TIMESTAMP represented as a
#' POSIX(c)t object in R
#'
#' @param pool a database connection pool object
#' @param registry Integer defining registry id
#' @param medfield Integer defining medfield id
#' @param user Integer defining user id
#' @param valid Logical if to select valid user only. TRUE by default
#' @param orgnr Integer id of organization
#' @param full_name Logical defining if full names is to be returned
#' @param indicator Character vector of indicator ids
#' @param include_short_name Logical if variable 'short_name' is to be returned
#' @param sample Integer in range \[0, 1\] defining data set subsample size.
#' Defaults to NA in which case all data is returned
#' @return Data object from database
#' @name db_get
#' @aliases get_indicator get_user_data get_user_id get_user_registries
#' get_user_registry_select get_registry_data
#' get_indicators_registryget_registry_ind get_registry_name get_org_name
#' get_flat_org get_all_orgnr get_user get_users
#' get_registry_indicators get_registry_medfield get_medfield_registry
#' get_registry_user get_user_registry get_aggdata_delivery
#' get_aggdata_delivery_time
NULL


#' @rdname db_get
#' @export
get_indicator <- function(pool) {

  lifecycle::deprecate_warn("0.12.0", "imongr::get_indicator()",
                            "imongr::get_table()")

  get_table(pool, "ind")
}


#' @rdname db_get
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


#' @rdname db_get
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


#' @rdname db_get
#' @export
get_user_id <- function(pool) {

  df <- get_user_data(pool)

  if (dim(df)[1] == 0) {
    stop("No data on the current user!")
  }

  df$id
}


#' @rdname db_get
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


#' @rdname db_get
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


#' @rdname db_get
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
                      tz = conf$app_text$format$tz)
    df$Tid <- format(df$Tid, format = conf$app_text$format$time, # nolint
                     tz = conf$app_text$format$tz)

    df
  } else {
    NULL
  }
}

#' @rdname db_get
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


#' @rdname db_get
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


#' @rdname db_get
#' @export
get_registry_ind <- function(pool, registry) {

  conf <- get_config()

  query <- paste0("
SELECT\n  ",
  paste(conf$db$tab$ind$insert, collapse = ",\n  "), "
FROM
  ind
WHERE
  registry_id=", registry, ";")

  pool::dbGetQuery(pool, query)
}


#' @rdname db_get
#' @export
get_registry_name <- function(pool, registry, full_name = FALSE) {

  if (missing(registry) || paste(registry, collapse = "") == "") {
    return(character())
  }

  query <- paste0("
SELECT
  name,
  full_name
FROM
  registry
WHERE
  id IN (", paste(registry, collapse = ", "), ");")

  if (full_name) {
    pool::dbGetQuery(pool, query)$full_name
  } else {
    pool::dbGetQuery(pool, query)$name
  }
}


#' @rdname db_get
#' @export
get_org_name <- function(pool, orgnr) {

  query <- paste0("
SELECT
  orgnr,
  short_name
FROM
  hospital
UNION
SELECT
  orgnr,
  short_name
FROM
  hf
UNION
SELECT
  orgnr,
  short_name
FROM
  rhf
UNION
SELECT
  orgnr,
  short_name
FROM
  nation;")

  orgs <- pool::dbGetQuery(pool, query)

  orgnr <- tibble::tibble(orgnr = orgnr)
  dplyr::left_join(orgnr, orgs, by = "orgnr")$short_name

}


#' @rdname db_get
#' @export
get_flat_org <- function(pool) {

  conf <- get_config()
  prefix <- conf$aggregate$orgnr$prefix
  query <- paste0("
SELECT
  hos.short_name AS ", conf$aggregate$unit_level$hospital$name, ",
  hos.orgnr AS ", paste0(prefix, conf$aggregate$unit_level$hospital$name), ",
  h.short_name AS ", conf$aggregate$unit_level$hf$name, ",
  h.orgnr AS ", paste0(prefix, conf$aggregate$unit_level$hf$name), ",
  r.short_name AS ", conf$aggregate$unit_level$rhf$name, ",
  r.orgnr AS ", paste0(prefix, conf$aggregate$unit_level$rhf$name), ",
  n.short_name AS ", conf$aggregate$unit_level$nation$name, ",
  n.orgnr AS ", paste0(prefix, conf$aggregate$unit_level$nation$name), "
FROM
  hospital hos
LEFT JOIN hf h ON
  hos.hf_orgnr=h.orgnr
LEFT JOIN rhf r ON
  h.rhf_orgnr=r.orgnr
LEFT JOIN nation n ON
  r.nation_orgnr=n.orgnr;"
  )

  pool::dbGetQuery(pool, query)
}


#' @rdname db_get
#' @export
get_all_orgnr <- function(pool, include_short_name = FALSE) {

  conf <- get_config()

  query <- paste0("
SELECT
  orgnr,
  '", conf$aggregate$unit_level$hospital$name, "' AS unit_level,
  short_name
FROM
  hospital
UNION
SELECT
  orgnr,
  '", conf$aggregate$unit_level$hf$name, "' AS unit_level,
  short_name
FROM
  hf
UNION
SELECT orgnr,
  '", conf$aggregate$unit_level$rhf$name, "' AS unit_level,
  short_name
FROM
  rhf
UNION
SELECT orgnr,
  '", conf$aggregate$unit_level$nation$name, "' AS unit_level,
  short_name
FROM
  nation;")

  dat <- pool::dbGetQuery(pool, query)

  if (!include_short_name) {
    dat <- dat %>%
      dplyr::select(-c("short_name"))
  }

  dat
}


#' @rdname db_get
#' @export
get_user <- function(pool, sample = NA) {

  conf <- get_config()
  query <- paste0("
SELECT
  ", paste0(conf$db$tab$user$insert, collapse = ",\n "), "
FROM
  user
WHERE
  valid=1")

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "AND RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}


#' @rdname db_get
#' @export
get_users <- function(pool, valid = TRUE) {

  query <- paste0("
SELECT
  *
FROM
  user
"
  )

  if (valid) {
    query <- paste(query, "WHERE\n  valid=1")
  }

  pool::dbGetQuery(pool, query)
}

#' @rdname db_get
#' @export
get_registry_indicators <- function(pool, registry) {

  query <- paste0("
SELECT
  id
FROM
  ind
WHERE
  registry_id=", registry, ";"
  )

  pool::dbGetQuery(pool, query)
}


#' @rdname db_get
#' @export
get_registry_medfield <- function(pool, registry) {

  query <- paste0("
SELECT
  mr.medfield_id,
  m.name,
  m.full_name
FROM
  registry_medfield mr
LEFT JOIN medfield m ON
  mr.medfield_id=m.id
WHERE
  mr.registry_id=", registry, ";"
  )

  pool::dbGetQuery(pool, query)
}


#' @rdname db_get
#' @export
get_medfield_registry <- function(pool, medfield) {

  query <- paste0("
SELECT
  DISTINCT(registry_id)
FROM
  registry_medfield
WHERE
  medfield_id=", medfield, ";"
  )

  pool::dbGetQuery(pool, query)
}

#' @rdname db_get
#' @export
get_registry_user <- function(pool, registry) {

  query <- paste0("
SELECT
  user_registry.user_id,
  user.name
FROM
  user_registry
LEFT JOIN user ON
  user_registry.user_id=user.id
WHERE
  user_registry.registry_id=", registry, ";"
  )

  pool::dbGetQuery(pool, query)
}


#' @rdname db_get
#' @export
get_user_registry <- function(pool, user) {

  query <- paste0("
SELECT
  DISTINCT(registry_id)
FROM
  user_registry
WHERE
  user_id=", user, ";"
  )

  pool::dbGetQuery(pool, query)
}


#' @rdname db_get
#' @export
get_aggdata_delivery <- function(pool) {

  # get current delivery ids in data
  query <- paste0("
SELECT
  ind_id,
  context,
  MAX(delivery_id) as id
FROM
  data
GROUP BY
  ind_id,
  context;"
  )

  dat <- pool::dbGetQuery(pool, query)

  # get delivery data
  query <- paste0("
SELECT
  id,
  time AS delivery_time,
  latest_update AS delivery_latest_update,
  latest_affirm AS delivery_latest_affirm
FROM
  delivery;"
  )

  delivery <- pool::dbGetQuery(pool, query)

  # add times to data
  dat <- dat %>%
    dplyr::left_join(delivery, by = "id") %>%
    dplyr::select(-c("id"))

  # get aggdata
  query <- paste0("
SELECT
  id,
  ind_id,
  context
FROM
  agg_data;"
  )

  agg <- pool::dbGetQuery(pool, query)

  aggdata_delivery <- agg %>%
    dplyr::left_join(dat, by = c("ind_id", "context")) %>%
    dplyr::select(
      "id",
      "delivery_time",
      "delivery_latest_update",
      "delivery_latest_affirm"
    )

  # remove missing times
  aggdata_delivery[!is.na(aggdata_delivery$delivery_time), ]
}

#' @rdname db_get
#' @export
get_aggdata_delivery_time <- function(pool) {
  lifecycle::deprecate_warn(
    "0.27.0", "imongr::get_aggdata_delivery_time()",
    "imongr::get_aggdata_delivery()"
  )

  # get current delivery ids in data
  query <- paste0("
SELECT
  ind_id,
  context,
  MAX(delivery_id) as id
FROM
  data
GROUP BY
  ind_id,
  context;"
  )

  dat <- pool::dbGetQuery(pool, query)

  # get delivery data
  query <- paste0("
SELECT
  id,
  time AS delivery_time
FROM
  delivery;"
  )

  delivery <- pool::dbGetQuery(pool, query)

  # add times to data
  dat <- dat %>%
    dplyr::left_join(delivery, by = "id") %>%
    dplyr::select(-c("id"))

  # get aggdata
  query <- paste0("
SELECT
  id,
  ind_id,
  context
FROM
  agg_data;"
  )

  agg <- pool::dbGetQuery(pool, query)

  aggdata_delivery_time <- agg %>%
    dplyr::left_join(dat, by = c("ind_id", "context")) %>%
    dplyr::select("id", "delivery_time")

  # remove missing times
  aggdata_delivery_time[!is.na(aggdata_delivery_time$delivery_time), ]

}

#' @rdname db_get
#' @export
get_aggdata <- function(pool, registry) {

  col_names <- pool::dbGetQuery(pool, "SELECT * FROM agg_data WHERE 1 = 0") %>%
    colnames()

  col_names <- paste0("ad.", col_names) %>% paste(collapse = ", ")

  query <- paste0(
    "SELECT ", col_names,
    " FROM agg_data AS ad
    LEFT JOIN ind on ad.ind_id = ind.id
    WHERE registry_id = ", registry)

  aggdata <- pool::dbGetQuery(pool, query)

  # Change timestamp and date formats to strings to avoid unexpected changes to the data
  aggdata$delivery_time <- as.character(aggdata$delivery_time)
  aggdata$time <- as.character(aggdata$time)
  aggdata$delivery_latest_update <- as.character(aggdata$delivery_latest_update)
  aggdata$delivery_latest_affirm <- as.character(aggdata$delivery_latest_affirm)

  aggdata[is.na(aggdata)] <- ""

  return(aggdata)
}
