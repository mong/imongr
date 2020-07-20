#' Aggregate data
#'
#' Aggregation of indicators as provided for each unit (hospital) at
#' organization levels above hospital: health trust, regional health trust and
#' national.
#'
#' All functions are adapted from qmongr/qmongrdata. However, the nomenclature
#' of function arguments are changed somewhat. Main source of underlying data
#' is the qmongr database and where tables are used in these function their
#' names are kept as is (\emph{org} and \emph{indicator}). Other data frames
#' passed into or between function are denoted \emph{df} or \emph{gdf} where
#' the latter denotes a data frame that has been grouped (by
#' \code{dplyr::group_by()}).
#'
#' @param df Data frame
#' @param gdf Data frame of grouped data
#' @param org Data frame holding the org db table providing all organization
#' data for each unit (from hospital to health trust)
#' @param indicator Data frame holding the indicator db table providing all
#' data on each indicator
#' @param group Character string defining the name of the grouping variable
#' in a data frame
#' @return Data frame in raw, grouped or aggregated form
#' @name aggregate
#' @aliases agg add_unit_id make_group compute_group compute_indicator_mean
#' compute_indicator_median get_indicator_level
NULL

#' @rdname aggregate
#' @importFrom rlang .data
#' @export
agg <- function(df, org, indicator) {

  df <- add_unit_id(df, org)

  groups <- c("OrgNrShus", "OrgNrHF", "OrgNrRHF", "")
  unit_levels <- c("shus", "hf", "rhf", "nasjonal")
  unit_names <- c("SykehusNavn", "Hfkortnavn", "RHF", "nasjonal")

  #groups <- c("OrgNrRHF", "")
  #unit_levels <- c("rhf", "nasjonal")

  agg <- data.frame()

  for (i in seq_len(length(groups))) {
    idf <- make_group(df, group = groups[i])
    idf <- compute_group(idf, indicator)
    idf$unit_level <- rep(unit_levels[i], dim(idf)[1])
    if (unit_levels[i] == "nasjonal") {
      idf$OrgNr <- rep(0, dim(idf)[1])
      idf$unit_name <- rep("Nasjonal", dim(idf)[1])
    } else {
      idf <- idf %>%
        dplyr::left_join(
          org %>%
            dplyr::select(
              .data[[groups[i]]],
              .data[[unit_names[i]]]
            ), by = groups[i]
        )
      names(idf)[names(idf) == groups[i]] <- "OrgNr"
      names(idf)[names(idf) == unit_names[i]] <- "unit_name"
    }

    names(idf)[names(idf) == "KvalIndID"] <- "IndID"
    agg <- rbind(agg, idf)
  }

  agg
}

#' @rdname aggregate
#' @importFrom rlang .data
#' @export
add_unit_id <- function(df, org) {

  df <- df %>%
    dplyr::left_join(
      org %>%
        dplyr::select(
          .data[["OrgNrShus"]],
          .data[["OrgNrHF"]],
          .data[["OrgNrRHF"]]
        ),
      by = "OrgNrShus"
    )
}

#' @rdname aggregate
#' @importFrom rlang .data
#' @export
make_group <- function(df, group = "") {

  df <- df %>%
    dplyr::group_by(
      .data[["Aar"]],
      .data[["KvalIndID"]]
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
#' @importFrom rlang .data
#' @export
compute_group <- function(gdf, indicator) {

  # currently nonsensical. Method(s) to be fetched from indicator db table
  indicator_median <- "intensiv2"
  indicator_mean <- c(
    "nakke1", "nakke2", "nakke3", "nakke4", "intensiv1",
    "norgast1",  "norgast2",  "norgast3",  "norgast4",
    "norgast5",  "norgast6",  "norgast7",  "norgast8",
    "norgast9",  "norgast10"
  )
  gmean <- gdf %>%
    dplyr::filter(.data[["KvalIndID"]] %in% indicator_mean) %>%
    compute_indicator_mean()

  gmedian <- gdf %>%
    dplyr::filter(.data[["KvalIndID"]] %in% indicator_median) %>%
    compute_indicator_median()

  g <- dplyr::bind_rows(gmean, gmedian) %>%
    dplyr::arrange(.data[["KvalIndID"]]) %>%
    dplyr::ungroup() %>%
    as.data.frame()

  get_indicator_level(gdf = g, indicator)
}


#' @rdname aggregate
#' @importFrom rlang .data
#' @export
compute_indicator_mean <- function(gdf)  {

  gdf %>%
    dplyr::summarise(
      count = dplyr::n(),
      indicator = mean(.data[["Variabel"]])
    )
}

#' @rdname aggregate
#' @importFrom rlang .data
#' @export
compute_indicator_median <- function(gdf)  {

  gdf %>%
    dplyr::summarise(
      count = dplyr::n(),
      indicator = stats::median(.data[["Variabel"]])
    )
}

#' @rdname aggregate
#' @importFrom rlang .data
#' @export
get_indicator_level <- function(gdf, indicator) {
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
      indicator <- indicator %>%
        dplyr::filter(.data[["IndID"]] == data_row[["KvalIndID"]])
      if (!is.na(indicator[["MaalRetn"]])) {
        if (!is.na(indicator[["MaalNivaaGronn"]]) &
            !is.na(indicator[["MaalNivaaGul"]])) {
          if (indicator[["MaalRetn"]] == 1) {
            level <- high(
              value = data_row[["indicator"]],
              green = indicator[["MaalNivaaGronn"]],
              yellow = indicator[["MaalNivaaGul"]]
            )
          } else if (indicator[["MaalRetn"]] == 0) {
            level <- low(
              value = data_row[["indicator"]],
              green = indicator[["MaalNivaaGronn"]],
              yellow =  indicator[["MaalNivaaGul"]]
            )
          }
        } else {
          desired_level <- switch(
            paste(indicator[["MaalRetn"]]),
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

