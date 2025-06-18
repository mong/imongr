#' Aggregate hospital data
#'
#' Aggregation of indicators as provided for each unit (hospital) at
#' organization levels above hospital: health trust, regional health trust and
#' national.
#'
#' All functions are adapted from qmongr/qmongrdata. However, the nomenclature
#' of function arguments are changed somewhat. Main source of underlying data
#' is the qmongr database and where tables are used in these function their
#' names are kept as is (\emph{org} and \emph{ind}). Other data frames
#' passed into or between function is denoted \emph{df}.
#' The aggregate consists of the following variables:
#' \describe{
#'   \item{year}{The year of the current record}
#'   \item{ind_id}{Indicator ID}
#'   \item{orgnr}{Organization ID}
#'   \item{context}{How orgnr is to be understood, \emph{e.g.} caregiver,
#'   or patient residency}
#'   \item{count}{Number of observations of the current record}
#'   \item{var}{Summarised indicator value, for instance mean or median}
#'   \item{unit_level}{Code providing level of aggregation such as 'hospital',
#'   'hf' (health trust), 'rhf' (regional health trust) and 'national'}
#'   \item{unit_name}{Name of the organization unit}
#' }
#'
#' @param df Data frame
#' @param org Data frame holding the flat org table with all levels needed. May
#' be obtained by \code{get_flat_org(pool)}
#' @param ind Data frame holding the indicator db table providing all
#' data on each indicator
#' @param ind_noagg Character vector identifying indicator (ids) that are not to
#' be aggregated (used as is). Default is \code{character()}
#' @param orgnr_name_map Data frame with global mapping of organization id and
#' name. Applied on data to be used as is (no aggregation).
#' @param aggs Data frame of (pre) aggregated data
#' @param diff Data frame with diff data
#' @param conf List of configuration
#' @param from_level Integer specifying from what level to aggregate from
#' @return Data frame in raw, grouped or aggregated form
#' @name aggregate
#' @aliases agg agg_dg agg_from_level agg_residual agg_udef
#' @importFrom rlang .data
NULL

#' @rdname aggregate
#' @export
agg <- function(df, org, ind, ind_noagg = character(), orgnr_name_map) {
  conf <- get_config()

  missing_var <- setdiff(
    conf$db$tab$data$insert[conf$aggregate$data_var_ind],
    names(df)
  )

  if (length(missing_var) > 0) {
    stop(paste0(
      "Missing var(s) in df: ", paste(missing_var, collapse = ", "),
      ". Cannot go on!"
    ))
  }

  # split by data that are not for aggregation (none-fraction indicators)
  df_noagg <- dplyr::filter(df, .data$ind_id %in% ind_noagg) |>
    dplyr::left_join(orgnr_name_map, by = "orgnr") |>
    dplyr::select(
      "context", "year", "ind_id", "unit_level",
      "unit_name", "var", "denominator", "orgnr"
    )
  df <- df |>
    dplyr::filter(!.data$ind_id %in% ind_noagg)
  message(paste("  Fjerner", dim(df_noagg)[1], "rader for indikatorer"))
  message(paste("   ", dim(df)[1], "aggregerbare rader gjenstår"))

  if (dim(df)[1] > 0) {
    unit_levels <- unique(df$unit_level)

    # aggregate each level
    aggs <- data.frame()
    for (i in seq_len(length(unit_levels))) {
      agg <- agg_from_level(
        df[df$unit_level == unit_levels[i], ], org, ind,
        conf,
        conf$aggregate$unit_level[[unit_levels[i]]]$level
      )
      aggs <- rbind(aggs, agg)
    }

    # sum over levels (i.e. organizations)
    aggs <- aggs |>
      dplyr::group_by(.data$year, .data$ind_id, .data$orgnr, .data$context) |>
      dplyr::reframe(var = sum(.data$var),
                     denominator = sum(.data$denominator),
                     unit_level = unique(.data$unit_level),
                     unit_name = unique(.data$unit_name)) |>
      as.data.frame()

    # DANGER! As of the Sep 15 2020 project meeting, pre-aggregate on short_name
    # DANGER! A less risky way of proper handling of 'groups' should be applied
    # DANGER! at a later stage
    message("  Pre-aggregerer etter kortnavn")
    message(paste("    Rader f\u00f8r pre-aggregering:", dim(df)[1]))
    aggs <- aggs |>
      dplyr::group_by(
        .data$context, .data$year, .data$ind_id, .data$unit_level,
        .data$unit_name
      ) |>
      dplyr::summarise(
        var = sum(.data$var),
        denominator = sum(.data$denominator),
        orgnr = min(.data$orgnr)
      )
    message(paste("    Rader etter pre-aggregering:", dim(df)[1]))
    # DANGER end!


    # make parts
    aggs$var <- aggs$var / aggs$denominator

    aggs <- aggs |>
      dplyr::arrange(
        .data$context,
        .data$ind_id,
        .data$year,
        .data$unit_level
      ) |>
      dplyr::ungroup()

    # add data for indicators that is not aggregated
    aggs <- rbind(aggs, df_noagg)
  } else {
    message("  Ingenting \u00e5 aggregere")
    aggs <- df_noagg
  }

  # add/update dg and all depending indicators
  aggs <- agg_dg(aggs, ind)

  aggs
}

#' @rdname aggregate
#' @export
agg_dg <- function(aggs, ind) {
  # no dg as outset
  aggs$dg <- NA

  # map data set ind ids to corresponding dg, new column named 'dg_id'
  ind_id_dg_id <- dplyr::select(ind, "id", "dg_id")
  aggs <- aggs |>
    dplyr::left_join(ind_id_dg_id, by = c("ind_id" = "id"))

  # dgs from current data that we will work on
  dgs <- unique(aggs$dg_id[!is.na(aggs$dg_id)])

  # current dg data
  dg_data <- dplyr::filter(aggs, .data$ind_id %in% dgs)

  # unique years represented by dgs in data, ascending order
  years <- dplyr::select(dg_data, "year") |>
    dplyr::distinct() |>
    dplyr::arrange() |>
    dplyr::pull()

  # dg might not be present for every year, hence start filling in oldest
  # and consecutively replace these with newer if present. Data older than
  # the oldest dg will not be provided with a dg and will remain default (NA)
  # at: temporary aggs for each iteration
  # dt: temporary dg_data for each iteration
  for (i in seq_len(length(years))) {
    at <- dplyr::filter(aggs, .data$year >= years[i]) |>
      dplyr::select(-c("dg"))
    dt <- dplyr::filter(dg_data, .data$year == years[i]) |>
      dplyr::select("ind_id", "unit_name", "var", "context") |>
      dplyr::rename(dg_id = "ind_id", dg = "var")
    # join current year dg into data from current year and newer. Then, move on
    # with only those vars needed for updating aggs
    at <- at |>
      dplyr::left_join(dt, by = c("dg_id", "unit_name", "context")) |>
      dplyr::select(
        "ind_id", "dg_id", "unit_level",
        "unit_name", "orgnr", "year",
        "denominator", "var", "dg", "context"
      )

    # update aggs from current year and newer with dg from current year
    aggs <- aggs |>
      dplyr::filter(.data$year < years[i]) |>
      dplyr::bind_rows(at)
  }

  aggs |> dplyr::select(-c("dg_id"))
}

#' @rdname aggregate
#' @export
agg_from_level <- function(df, org, ind, conf, from_level) {
  all_unit_levels <- names(conf$aggregate$unit_level)
  unit_levels <- all_unit_levels[from_level:length(all_unit_levels)]
  groups <- paste0(conf$aggregate$orgnr$prefix, unit_levels)

  # data must be single level only
  data_level <- unique(df$unit_level)
  stopifnot(length(data_level) == 1)

  # level as defined in data must correspond to requested from_level
  stopifnot(data_level == all_unit_levels[from_level])

  # rename orgnr var in data corresponding to this level
  names(df)[names(df) == "orgnr"] <- groups[1]

  # join in flat org from this level and up
  org <- unique(org[, names(org) %in% c(unit_levels, groups)])
  df <- dplyr::left_join(df, org, by = groups[1])

  agg <- data.frame()

  for (i in seq_len(length(groups))) {
    idf <- df |>
      dplyr::group_by(
        .data[["year"]], .data[["ind_id"]], .data[["context"]],
        .data[[groups[i]]]
      )
    idf <- dplyr::summarise(idf,
      var = sum(.data$var),
      denominator = sum(.data$denominator)
    )
    idf$unit_level <- rep(unit_levels[i], dim(idf)[1])
    this_org <- org |>
      dplyr::select(dplyr::all_of(groups[i]), dplyr::all_of(unit_levels[i])) |>
      dplyr::distinct()
    idf <- dplyr::left_join(idf, this_org, by = groups[i])
    names(idf)[names(idf) == groups[i]] <- "orgnr"
    names(idf)[names(idf) == unit_levels[i]] <- "unit_name"
    idf <- idf |>
      dplyr::ungroup() |>
      as.data.frame()
    agg <- rbind(agg, idf)
  }

  agg
}
