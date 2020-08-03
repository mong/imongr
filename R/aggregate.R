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
#' @aliases agg add_unit_id make_group compute_group compute_indicator_mean
#' compute_indicator_median get_indicator_level
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

  # add flat orgs to data
  df <- df %>%
    dplyr::left_join(., org,
                     by = c("hospital_id" = paste0(conf$aggregate$orgnr$suffix,
                                                   conf$aggregate$unit_level$hospital)
                     )
    )
  #df <- add_unit_id(df, org)

  groups <- paste0(conf$aggregate$orgnr$suffix, conf$aggregate$unit_level)
  unit_levels <- conf$aggregate$unit_level
  unit_names <- c("SykehusNavn", "Hfkortnavn", "RHF", "nasjonal")

  agg <- data.frame()

  for (i in seq_len(length(groups))) {
    idf <- make_group(df, group = groups[i])
    idf <- compute_group(idf, ind)
    idf$unit_level <- rep(unit_levels[i], dim(idf)[1])
    this_org <- org %>%
      dplyr::select(.data[[groups[i]]], .data[[unit_levels[i]]]) %>%
      dplyr::distinct()
    idf <- dplyr::left_join(idf, this_org, by = groups[i])
    names(idf)[names(idf) == groups[i]] <- "orgnr"
    names(idf)[names(idf) == unit_names[i]] <- "unit_name"
    agg <- rbind(agg, idf)
  }

  agg
}

#' @rdname aggregate
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

  # currently nonsensical. Method(s) to be fetched from ind db table
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

  get_indicator_level(gdf = g, ind)
}


#' @rdname aggregate
#' @export
compute_indicator_mean <- function(gdf)  {

  gdf %>%
    dplyr::summarise(
      count = dplyr::n(),
      indicator = mean(.data[["Variabel"]])
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
        dplyr::filter(.data[["IndID"]] == data_row[["KvalIndID"]])
      if (!is.na(ind[["MaalRetn"]])) {
        if (!is.na(ind[["MaalNivaaGronn"]]) &
            !is.na(ind[["MaalNivaaGul"]])) {
          if (ind[["MaalRetn"]] == 1) {
            level <- high(
              value = data_row[["var"]],
              green = ind[["MaalNivaaGronn"]],
              yellow = ind[["MaalNivaaGul"]]
            )
          } else if (ind[["MaalRetn"]] == 0) {
            level <- low(
              value = data_row[["var"]],
              green = ind[["MaalNivaaGronn"]],
              yellow =  ind[["MaalNivaaGul"]]
            )
          }
        } else {
          desired_level <- switch(
            paste(ind[["MaalRetn"]]),
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
