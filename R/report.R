#' Functions providing reports to be used in imongr
#'
#' @param pool Database pool object for connection to the production system
#' @param pool_verify Database pool object for connection to staged data
#'
#' @return A data frame holding the report data
#' @name report
#' @aliases registry_status_report .registry_status_data
NULL

#' @rdname report
.registry_status_data <- function(pool) {
  reg <- get_table(pool, "registry")
  data <- get_table(pool, "data")
  ind <- get_table(pool, "ind")
  delivery <- get_table_raw(pool, "delivery")

  # pick relevant vars and if possible, aggregate
  reg <- reg %>%
    dplyr::select("id", "name")
  data <- data %>%
    dplyr::select("delivery_id", "year", "ind_id")
  ind <- ind %>%
    dplyr::select("id", "registry_id")
  delivery <- delivery %>%
    dplyr::select("id", "time")

  # join sets
  data %>%
    dplyr::left_join(ind, by = c("ind_id" = "id")) %>%
    dplyr::left_join(reg, by = c("registry_id" = "id")) %>%
    dplyr::left_join(delivery, by = c("delivery_id" = "id"))
}

#' @rdname report
#' @export
registry_status_report <- function(pool, pool_verify) {
  rdf <- .registry_status_data(pool) %>%
    dplyr::select("name", "year", "time") %>%
    dplyr::group_by(.data$name) %>%
    dplyr::summarise(
      min_year = min(.data$year),
      max_year = max(.data$year),
      last_publish = max(.data$time),
      unique_years = length(unique(.data$year))
    )

  vrdf <- .registry_status_data(pool_verify) %>%
    dplyr::select("name", "time") %>%
    dplyr::group_by(.data$name) %>%
    dplyr::summarise(last_upload = max(.data$time))

  dplyr::left_join(rdf, vrdf, by = "name") %>%
    dplyr::transmute(
      Register = .data$name,
      `Fra og med` = .data$min_year,
      `Til og med` = .data$max_year,
      `Unike aar` = .data$unique_years,
      `Sist publisert` = as.Date(.data$last_publish),
      `Sist lastet opp` = as.Date(.data$last_upload)
    )
}
