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
#' \code{dplyr::group_by()}. The aggregate consists of the following variables:
#' \describe{
#'   \item{year}{The year of the current record}
#'   \item{ind_id}{Indicator ID}
#'   \item{orgnr}{Organization ID}
#'   \item{context}{How orgnr is to be understood, \emph{e.g.} caregiver,
#'   or patient residency}
#'   \item{count}{Number of observations of the current record}
#'   \item{var}{Summarised indicator value, for instance mean or median}
#'   \item{level}{Code providing evaluated discrete level such as 'H' (high),
#'   'M' (intermediate) and 'L' (low)}
#'   \item{level_direction}{Name providing the desired level such as 'Lavt'
#'   (low) or 'Høyt' (high)}
#'   \item{unit_level}{Code providing level of aggregation such as 'hospital',
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
#' @param aggs Data frame of (pre) aggregated data
#' @param diff Data frame with diff data
#' @param conf List of configuration
#' @param from_level Integer specifying from what level to aggregate from
#' @return Data frame in raw, grouped or aggregated form
#' @name aggregate
#' @aliases agg agg_dg agg_from_level agg_residual agg_udef get_indicator_level
#' @importFrom rlang .data
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
    agg <- agg_from_level(df[df$unit_level == unit_levels[i], ], org, ind,
                          conf,
                          conf$aggregate$unit_level[[unit_levels[i]]]$level)
    aggs <- rbind(aggs, agg)
  }

  # sum over levels (i.e. organizations)
  aggs <- aggs %>%
    dplyr::group_by(.data$year, .data$ind_id, .data$orgnr, .data$context) %>%
    dplyr::summarise(var = sum(.data$var),
                     denominator = sum(.data$denominator),
                     unit_level = unique(.data$unit_level),
                     unit_name = unique(.data$unit_name)) %>%
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

  # DANGER! As of the Sep 15 2020 project meeting, pre-aggregate on short_name
  # DANGER! A less risky way of proper handling of 'groups' should be applied
  # DANGER! at a later stage
  message("Pre-aggregating by short name")
  message(paste("  ...rows before pre-aggregation:", dim(df)[1]))
  aggs <- aggs %>%
    dplyr::group_by(.data$context, .data$year, .data$ind_id, .data$unit_level,
                    .data$unit_name) %>%
    dplyr::summarise(var = sum(.data$var),
                     denominator = sum(.data$denominator),
                     orgnr = min(.data$orgnr))
  message(paste("  ...rows after pre-aggregation:", dim(df)[1]))
  # DANGER end!


  # make parts
  aggs$var <- aggs$var / aggs$denominator

  aggs <- aggs %>%
    dplyr::arrange(.data$context,
                   .data$ind_id,
                   .data$year,
                   .data$unit_level) %>%
    dplyr::ungroup()

  # class values into defined levels
  aggs <- get_indicator_level(aggs, ind)

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
  ind_id_dg_id <- dplyr::select(ind, .data$id, .data$dg_id)
  aggs <- aggs %>%
    dplyr::left_join(ind_id_dg_id, by = c("ind_id" = "id"))

  # dgs from current data that we will work on
  dgs <- unique(aggs$dg_id[!is.na(aggs$dg_id)])

  # current dg data
  dg_data <- dplyr::filter(aggs, .data$ind_id %in% dgs)

  # unique years represented by dgs in data, ascending order
  years <- dplyr::select(dg_data, .data$year) %>%
    dplyr::distinct() %>%
    dplyr::arrange() %>%
    dplyr::pull()

  # dg might not be present for every year, hence start filling in oldest
  # and consecutively replace these with newer if present. Data older than
  # the oldest dg will not be provided with a dg and will remain default (NA)
  # at: temporary aggs for each iteration
  # dt: temporary dg_data for each iteration
  for (i in seq_len(length(years))) {
    at <- dplyr::filter(aggs, .data$year >= years[i]) %>%
      dplyr::select(!.data$dg)
    dt <- dplyr::filter(dg_data, .data$year == years[i]) %>%
      dplyr::select(.data$ind_id, .data$orgnr, .data$var, .data$context) %>%
      dplyr::rename(dg_id = .data$ind_id, dg = .data$var)
    # join current year dg into data from current year and newer. Then, move on
    # with only those vars needed for updating aggs
    at <- at %>%
      dplyr::left_join(dt, by = c("dg_id", "orgnr", "context")) %>%
      dplyr::select(.data$ind_id, .data$dg_id, .data$unit_level,
                    .data$unit_name, .data$orgnr, .data$year,
                    .data$denominator, .data$var, .data$level,
                    .data$level_direction, .data$dg, .data$context)

    # update aggs from current year and newer with dg from current year
    aggs <- aggs %>%
      dplyr::filter(.data$year < years[i]) %>%
      dplyr::bind_rows(at)
  }

  aggs %>% dplyr::select(!.data$dg_id)
}

#' @rdname aggregate
#' @export
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
  org <- unique(org[, names(org) %in% c(unit_levels, groups)])
  df <- dplyr::left_join(df, org, by = groups[1])

  agg <- data.frame()

  for (i in seq_len(length(groups))) {
    idf <- df %>%
      dplyr::group_by(.data[["year"]], .data[["ind_id"]], .data[["context"]],
                      .data[[groups[i]]])
    idf <- dplyr::summarise(idf, var = sum(.data$var),
                            denominator = sum(.data$denominator))
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


#' @rdname aggregate
#' @export
agg_residual <- function(aggs, conf) {

  # diff sum levels
  sum_unit_level <- aggs %>%
    dplyr::group_by(.data$context, .data$year, .data$ind_id,
                    .data$unit_level) %>%
    dplyr::summarise(var = sum(.data$var),
                     denominator = sum(.data$denominator))

  ## sort orgs from tow level and down
  level <- vector()
  name <- vector()
  for (i in seq_len(length(conf$aggregate$unit_level))) {
    this_unit <-
      conf$aggregate$unit_level[[names(conf$aggregate$unit_level[i])]]
    level <- c(level, this_unit$level)
    name <- c(name, this_unit$name)
  }
  desc <-
    data.frame(name = name, level = level,
               stringsAsFactors = FALSE)[order(level, decreasing = TRUE), ]

  # baseline compleete combination of context, year and indicator
  set0 <- unique(paste(aggs$context, aggs$year, aggs$ind_id, sep = ";"))

  # prep initial set
  l0 <- sum_unit_level %>%
    dplyr::filter(.data$unit_level == desc$name[1]) %>%
    dplyr::arrange(.data$context, .data$year, .data$ind_id)
  set1 <- paste(l0$context, l0$year, l0$ind_id, sep = ";")
  setd <- setdiff(set0, set1)
  if (length(setd) > 0) {
    lt <- tibble::tibble(context = gsub(";(.+?);(.+?)$", "", setd),
                         year = as.integer(gsub("^(.+?);|;(.+?)$", "", setd)),
                         ind_id = gsub("^(.+?);(.+?);", "", setd),
                         unit_level = desc$name[1],
                         var = 0,
                         denominator = 0)
    l0 <- l0 %>%
      dplyr::bind_rows(lt) %>%
      dplyr::arrange(.data$context, .data$year, .data$ind_id)
  }

  diff <- data.frame()
  for (i in seq_len(dim(desc)[1] - 1)) {
    l1 <- sum_unit_level %>%
      dplyr::filter(.data$unit_level == desc$name[i + 1]) %>%
      dplyr::arrange(.data$context, .data$year, .data$ind_id)
    # pad missing context-year-indicator combinations with 0, if any
    set1 <- paste(l1$context, l1$year, l1$ind_id, sep = ";")
    setd <- setdiff(set0, set1)
    if (length(setd) > 0) {
      lt <- tibble::tibble(context = gsub(";(.+?);(.+?)$", "", setd),
                           year = as.integer(gsub("^(.+?);|;(.+?)$", "", setd)),
                           ind_id = gsub("^(.+?);(.+?);", "", setd),
                           unit_level = desc$name[i + 1],
                           var = 0,
                           denominator = 0)
      l1 <- l1 %>%
        dplyr::bind_rows(lt) %>%
        dplyr::arrange(.data$context, .data$year, .data$ind_id)
    }

    l1$var_diff <- l0$var - l1$var
    l1$denominator_diff <- l0$denominator - l1$denominator
    if (i == 1) {
      diff <- l1
    } else {
      diff <- rbind(diff, l1)
    }
    # for next level (iteration)
    l0 <- l1
  }

  # remove rows with no residuals
  ind <- diff$var_diff == 0 & diff$denominator_diff == 0
  as.data.frame(diff[!ind, ])

}

#' @rdname aggregate
#' @export
agg_udef <- function(diff, conf) {

  if (any(diff$denominator_diff == 0)) {
    warning(paste("Got data where denominator difference is 0 when",
                  "aggregating undefined organizations. This is a strong",
                  "indication of error in the underlying data. As a first",
                  "step, check that events are not duplicated across",
                  "organization levels."))
  }
  # prep ascending name-level data frame
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

  # add orgnr and unit_name columns to diff data
  diff <- cbind(diff, orgnr = NA, unit_name = NA)

  for (i in seq_len(dim(unit)[1] - 1)) {
    idiff <- diff[diff$unit_level == unit$name[i], ]
    u0 <- idiff %>%
      dplyr::filter(.data$denominator_diff > 0)
    u1 <- idiff %>%
      dplyr::filter(.data$denominator_diff < 0)
    if (length(u0$orgnr) > 0) {
      u0$orgnr <- conf$aggregate$orgnr$undefined[[unit$name[i]]]
      u0$unit_name <- conf$aggregate$udef_unit_level[[unit$name[i]]]$name
    }
    if (length(u1$orgnr) > 0) {
      u1$orgnr <- conf$aggregate$orgnr$undefined[[unit$name[i + 1]]]
      u1$unit_name <- conf$aggregate$udef_unit_level[[unit$name[i + 1]]]$name
    }
    u <- rbind(u0, u1)
    diff[diff$unit_level == unit$name[i], ]$orgnr <- u$orgnr
    diff[diff$unit_level == unit$name[i], ]$unit_name <- u$unit_name
  }

  # A positive difference in var and denominator numbers compared to the below
  # level must be inherited to underlying orgs since these organizations where
  # not known during previous summation of levels during aggregation.
  # Condition apply: top level will never contain any 'undefined' organization!

  # (re)organize levels from top to bottom and skip top level (see above cond)
  unit <- unit[order(level, decreasing = TRUE), ]
  unit <- unit[!unit$level == max(unit$level), ]

  # baseline compleete combination of context, year and indicator
  set0 <- unique(paste(diff$context, diff$year, diff$ind_id, sep = ";"))

  # downwards inherit for each level and combination of year and indicator
  l0 <- dplyr::filter(diff, .data$unit_level == unit$name[1]) %>%
    dplyr::arrange(.data$context, .data$year, .data$ind_id)
  set1 <- paste(l0$context, l0$year, l0$ind_id, sep = ";")
  setd <- setdiff(set0, set1)
  if (length(setd) > 0) {
    lt <- tibble::tibble(context = gsub(";(.+?);(.+?)$", "", setd),
                         year = as.integer(gsub("^(.+?);|;(.+?)$", "", setd)),
                         ind_id = gsub("^(.+?);(.+?);", "", setd),
                         unit_level = unit$name[1],
                         var = 0,
                         denominator = 0,
                         var_diff = 0,
                         denominator_diff = 0,
                         orgnr =
                           conf$aggregate$orgnr$undefined[[unit$name[1]]],
                         unit_name =
                           conf$aggregate$udef_unit_level[[unit$name[1]]]$name)
    l0 <- l0 %>%
      dplyr::bind_rows(lt) %>%
      dplyr::arrange(.data$context, .data$year, .data$ind_id)
  }

  l <- l0
  for (i in seq_len(dim(unit)[1] - 1)) {
    l1 <- dplyr::filter(diff, .data$unit_level == unit$name[i + 1]) %>%
      dplyr::arrange(.data$context, .data$year, .data$ind_id)
    # pad missing context-year-indicator combinations with 0, if any
    set1 <- paste(l1$context, l1$year, l1$ind_id, sep = ";")
    setd <- setdiff(set0, set1)
    if (length(setd) > 0) {
      lt <-
        tibble::tibble(
          context = gsub(";(.+?);(.+?)$", "", setd),
          year = as.integer(gsub("^(.+?);|;(.+?)$", "", setd)),
          ind_id = gsub("^(.+?);(.+?);", "", setd),
          unit_level = unit$name[i + 1],
          var = 0,
          denominator = 0,
          var_diff = 0,
          denominator_diff = 0,
          orgnr = conf$aggregate$orgnr$undefined[[unit$name[i + 1]]],
          unit_name = conf$aggregate$udef_unit_level[[unit$name[i + 1]]]$name
        )
      l1 <- l1 %>%
        dplyr::bind_rows(lt) %>%
        dplyr::arrange(.data$context, .data$year, .data$ind_id)
    }
    l1$var_diff <- l1$var_diff + l0$var_diff
    l1$denominator_diff <- l1$denominator_diff + l0$denominator_diff

    l <- rbind(l, l1)

    # prep for next level (iteration)
    l0 <- l1
  }

  # throw out any records with denominator == 0
  diff <- l[l$denominator_diff > 0, ]

  diff$var <- diff$var_diff
  diff$denominator <- diff$denominator_diff
  diff[, !names(diff) %in% c("var_diff", "denominator_diff")]
}


#' @rdname aggregate
#' @importFrom rlang .data
#' @export
get_indicator_level <- function(gdf, ind) {
  gdf$level <- ""
  gdf$level_direction <- NA

  round_digits <- function(level) {
    # return 2 decimal places for levels equal 1 or greater under the assumption
    # that integer percentages will always be used in presentations
    if (level >= 1) {
      return(2)
    } else {
      # NB! significant digits set based on the db field type of levels that is
      # currently set to DOUBLE(7,3). If db spec changes the number of digits
      # for signif() must be updated accordingly
      decimals <- nchar(signif(abs(level), digits = 6)) - 2
      # for levels with one decimal place force 2 under same as above assumption
      # and also enforce 3 decimals for levels below 0.1. Hence, for the current
      # db type DOUBLE(7,3) the fallback of the below logic is redundant and
      # only kept in case of a future increase of allowed level decimals in db
      if (decimals < 2) {
        return(2)
      } else if (level < 0.1) {
        return(max(decimals, 3))
      } else {
        return(decimals)
      }
    }
  }
  high <- function(value, green, yellow) {
    gd <- round_digits(green)
    yd <- round_digits(yellow)
    if (round(value, digits = gd) >= green) {
      level <- "H"
    } else if (round(value, digits = gd) < green &
               round(value, digits = yd) >= yellow) {
      level <- "M"
    } else {
      level <- "L"
    }
    level_direction <- 1
    return(list(level = level, level_direction = level_direction))
  }
  low <- function(value, green, yellow) {
    gd <- round_digits(green)
    yd <- round_digits(yellow)
    if (round(value, digits = gd) <= green) {
      level <- "H"
    } else if (round(value, digits = gd) > green &
               round(value, digits = yd) <= yellow) {
      level <- "M"
    } else {
      level <- "L"
    }
    level_direction <- 0
    return(list(level = level, level_direction = level_direction))
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
          level_direction <- switch(
            paste(ind[["level_direction"]]),
            "1" =  1,
            "0" =  0
          )
          level <- list(level = "undefined", level_direction = level_direction)
        }
      } else{
        level <- list(level = "undefined", level_direction = NA)
      }
      gdf$level[x] <<- level$level
      gdf$level_direction[x] <<- level$level_direction
    }
  )

  gdf
}
