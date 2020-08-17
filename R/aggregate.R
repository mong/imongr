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
#' passed into or between function are denoted \emph{df} or \emph{gdf} where
#' the latter denotes a data frame that has been grouped (by
#' \code{dplyr::group_by()}. The aggregate consists of the following varables:
#' \describe{
#'   \item{year}{The year of the current record}
#'   \item{ind_id}{Indicator ID}
#'   \item{orgnr}{Organization ID}
#'   \item{count}{Number of observations of the current record}
#'   \item{var}{Summarised indicator value, for instance mean or median}
#'   \item{level}{Code providing evaluated discret level such as 'H' (high),
#'   'M' (intermediate) and 'L' (low)}
#'   \item{desired_level}{Name providing the desired level such as 'Lavt' (low)
#'   or 'Høyt' (high)}
#'   \item{unit_level}{Code provideing level of aggregation such as 'hospital',
#'   'hf' (health trust), 'rhf' (regional health trust) and 'national'}
#'   \item{unit_name}{Name of the organization unit}
#' }
#'
#' @param df Data frame
#' @param gdf Data frame of grouped data
#' @param org Data frame holding the flat org table with all levels needed. May
#' be obtained by \code{get_flat_org(pool)}
#' @param ind Data frame holding the indicator db table providing all
#' data on each indicator
#' @param group Character string defining the name of the grouping variable
#' in a data frame
#' @return Data frame in raw, grouped or aggregated form
#' @name aggregate
#' @aliases agg agg_from_level agg_residual agg_udef make_group compute_group
#' compute_indicator_mean compute_indicator_median get_indicator_level
NULL

#' @rdname aggregate
#' @export
agg <- function(df, org, ind) {

  conf <- get_config()

  missing_var <- setdiff(conf$db$tab$data$insert[conf$aggregate$data_var_ind],
                         names(df))

  if (length(missing_var) > 0) {
    stop(paste0("Missing var(s) in df: ", paste(missing_var, collapse = ", "),
                ". Cannot go on!"))
  }

  unit_levels <- unique(df$unit_level)

  # aggregate each level
  aggs <- data.frame()
  for (i in seq_len(length(unit_levels))) {
    agg <- agg_from_level(df[df$unit_level == unit_levels[i], ], org, ind, conf,
                          conf$aggregate$unit_level[[unit_levels[i]]]$level)
    aggs <- rbind(aggs, agg)
  }

  # sum over levels (i.e. organizations)
  aggs <- aggs %>%
    dplyr::group_by(year, ind_id, orgnr) %>%
    dplyr::summarise(var = sum(var),
                     denominator = sum(denominator),
                     unit_level = unique(unit_level),
                     unit_name = unique(unit_name)) %>%
    dplyr::ungroup() %>%
    as.data.frame()

  # find diffs between levels (if any, to be placed in undefined orgs)
  diff <- agg_residual(aggs, conf)

  # place residuals in undefined orgs (diffs < 0 will be added to above level)
  if (dim(diff)[1] > 0) {
    udef <- agg_udef(diff, conf)
  } else {
    udef <- aggs[FALSE, ]
  }

  # add undefined, if any
  aggs <- rbind(aggs, udef)

  # fake vars not processed yet
  aggs <- aggs %>%
    dplyr::rename(count = denominator)
  aggs$level <- "T"
  aggs$desired_level <- "Test"

  aggs

}

agg_from_level <- function(df, org, ind, conf, from_level) {

  all_unit_levels <- names(conf$aggregate$unit_level)
  unit_levels <- all_unit_levels[from_level:length(all_unit_levels)]
  groups <- paste0(conf$aggregate$orgnr$prefix, unit_levels)

  # data must be singel level only
  data_level <- unique(df$unit_level)
  stopifnot(length(data_level) == 1)

  # level as defined in data must correspond to requested from_level
  stopifnot(data_level == all_unit_levels[from_level])

  # rename orgnr var in data corresponing to this level
  names(df)[names(df) == "orgnr"] <- groups[1]

  # join in flat org from this level and up
  org <- org[, names(org) %in% c(unit_levels, groups)]
  df <- dplyr::left_join(df, org, by = groups[1])

  agg <- data.frame()

  for (i in seq_len(length(groups))) {
    print(paste("Aggregating unit level", unit_levels[i]))
    idf <- df %>%
      dplyr::group_by(.data[["year"]], .data[["ind_id"]], .data[[groups[i]]])
    idf <- dplyr::summarise(idf, var = sum(var), denominator = sum(denominator))
    #idf <- make_group(df, group = groups[i])
    #idf <- compute_group(idf, ind)
    idf$unit_level <- rep(unit_levels[i], dim(idf)[1])
    this_org <- org %>%
      dplyr::select(.data[[groups[i]]], .data[[unit_levels[i]]]) %>%
      dplyr::distinct()
    idf <- dplyr::left_join(idf, this_org, by = groups[i])
    names(idf)[names(idf) == groups[i]] <- "orgnr"
    names(idf)[names(idf) == unit_levels[i]] <- "unit_name"
    idf <- idf %>%
      dplyr::ungroup() %>%
      as.data.frame()
    agg <- rbind(agg, idf)
  }

  agg

}

agg_sum <- function() {

}

agg_residual <- function(aggs, conf) {

  # diff sum levels
  print("Finding residuals...")
  sum_unit_level <- aggs %>%
    dplyr::group_by(year, ind_id, unit_level) %>%
    dplyr::summarise(var = sum(var),
                     denominator = sum(denominator))

  ## find diffs descending from top level
  level <- vector()
  name <- vector()
  for (i in seq_len(length(conf$aggregate$unit_level))) {
    this_unit <-
      conf$aggregate$unit_level[[names(conf$aggregate$unit_level[i])]]
    level <- c(level, this_unit$level)
    name <- c(name, this_unit$name)
  }
  desc <-
    data.frame(name = name, level = level)[order(level, decreasing = TRUE), ]
  diff <- data.frame()
  for (i in seq_len(dim(desc)[1] - 1)) {
    l0 <- sum_unit_level %>%
      dplyr::filter(unit_level == desc$name[i])
    l1 <- sum_unit_level %>%
      dplyr::filter(unit_level == desc$name[i + 1])
    l1$var_diff <- l0$var - l1$var
    l1$denominator_diff <- l0$denominator - l1$denominator
    if (i == 1) {
      diff <- l1
    } else {
      diff <- rbind(diff, l1)
    }
  }

  # remove rows with no residuals
  ind <- diff$var_diff == 0 & diff$denominator_diff == 0
  as.data.frame(diff[!ind, ])

}

agg_udef <- function(diff, conf) {

  if (any(diff$denominator_diff == 0)) {
    stop(paste("Got data where denominator difference is 0.",
               "This must be an error. Stopping!"))
  }
  # prep descending name-level data frame
  level <- vector()
  name <- vector()
  for (i in seq_len(length(conf$aggregate$unit_level))) {
    this_unit <-
      conf$aggregate$unit_level[[names(conf$aggregate$unit_level[i])]]
    level <- c(level, this_unit$level)
    name <- c(name, this_unit$name)
  }
  unit <-
    data.frame(name = name, level = level,
               stringsAsFactors = FALSE)[order(level), ]

  # add orgnr column to diff data
  diff <- cbind(diff, orgnr = NA)

  for (i in seq_len(dim(unit)[1] - 1)) {
    idiff <- diff[diff$unit_level == unit$name[i], ]
    u0 <- idiff %>%
      dplyr::filter(denominator_diff > 0)
    u1 <- idiff %>%
      dplyr::filter(denominator_diff < 0)
    u0$orgnr <- conf$aggregate$orgnr$undefined[[unit$name[i]]]
    u1$orgnr <- conf$aggregate$orgnr$undefined[[unit$name[i + 1]]]
    u <- rbind(u0, u1)
    diff[diff$unit_level == unit$name[i]]$orgnr <- u$orgnr
  }

  diff$var <- diff$var_diff
  diff$denominator <- diff$denominator_diff
  diff[, !names(diff) %in% c("var_diff", "denominator_diff")]
}

#' @rdname aggregate
#' @export
make_group <- function(df, group = "") {

  df <- df %>%
    dplyr::group_by(
      .data[["year"]],
      .data[["ind_id"]]
    )

  if (group != "") {
    df <- df %>%
      dplyr::group_by(
        .data[[group]],
        .add = TRUE
      )
  }

  df
}

#' @rdname aggregate
#' @export
compute_group <- function(gdf, ind) {

  indicator_median <- ind$id[is.na(ind$type)]
  indicator_mean <- ind$id[ind$type == "andel"]
  indicator_mean <- indicator_mean[!is.na(indicator_mean)]

  gmean <- gdf %>%
    dplyr::filter(.data[["ind_id"]] %in% indicator_mean) %>%
    compute_indicator_mean()

  gmedian <- gdf %>%
    dplyr::filter(.data[["ind_id"]] %in% indicator_median) %>%
    compute_indicator_median()

  g <- dplyr::bind_rows(gmean, gmedian) %>%
    dplyr::arrange(.data[["ind_id"]]) %>%
    dplyr::ungroup() %>%
    as.data.frame()

  get_indicator_level(gdf = g, ind)
}


#' @rdname aggregate
#' @export
compute_indicator_mean <- function(gdf)  {

  gdf %>%
    dplyr::summarise(
      count = dplyr::n(),
      var = mean(.data[["var"]])
    )
}

#' @rdname aggregate
#' @export
compute_indicator_median <- function(gdf)  {

  gdf %>%
    dplyr::summarise(
      count = dplyr::n(),
      var = stats::median(.data[["var"]])
    )
}

#' @rdname aggregate
#' @importFrom rlang .data
#' @export
get_indicator_level <- function(gdf, ind) {
  gdf$level <- ""
  gdf$desired_level <- ""
  high <- function(value, green, yellow) {
    if (value > green) {
      level <- "H"
    } else if (value < green & value > yellow) {
      level <- "M"
    } else {
      level <- "L"
    }
    desired_level <-  "H\u00F8yt"
    return(list(level = level, desired_level = desired_level))
  }
  low <- function(value, green, yellow) {
    if (value < green) {
      level <- "H"
    } else if (value > green & value < yellow) {
      level <- "M"
    } else {
      level <- "L"
    }
    desired_level <-  "Lavt"
    return(list(level = level, desired_level = desired_level))
  }
  lapply(
    seq_len(nrow(gdf)),
    function(x) {
      data_row <- gdf[x, ]
      ind <- ind %>%
        dplyr::filter(.data[["id"]] == data_row[["ind_id"]])
      if (!is.na(ind[["level_direction"]])) {
        if (!is.na(ind[["level_green"]]) &
            !is.na(ind[["level_yellow"]])) {
          if (ind[["level_direction"]] == 1) {
            level <- high(
              value = data_row[["var"]],
              green = ind[["level_green"]],
              yellow = ind[["level_yellow"]]
            )
          } else if (ind[["level_direction"]] == 0) {
            level <- low(
              value = data_row[["var"]],
              green = ind[["level_green"]],
              yellow =  ind[["level_yellow"]]
            )
          }
        } else {
          desired_level <- switch(
            paste(ind[["level_direction"]]),
            "1" =  "H\u00F8yt",
            "0" =  "Lavt"
          )
          level <- list(level = "undefined", desired_level = desired_level)
        }
      } else{
        level <- list(level = "undefined", desired_level = "undefined")
      }
      gdf$level[x] <<- level$level
      gdf$desired_level[x] <<- level$desired_level
    }
  )

  gdf
}
