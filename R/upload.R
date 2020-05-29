#' Title
#'
#' @param df Data frame
#' @param path Character path to a file
#' @param sep Character filed sep
#' @param dec Character decimal sep
#' @param encoding Character encoding
#' @param df Data frame
#' @param n Numeric sample size
#' @param skip character vector defining data frame variables to skip
#' @param random Logical sample method
#'
#' @return
#' @importFrom utils read.csv
#' @name upload
#' @aliases csv_to_df sample_df
NULL


#' @rdname upload
#' @export
check_report <- function(df, pool) {

  conf <- get_config()
  r <- check_upload(df, pool)

  if (all(!r$fail)) {
    msg <- paste("<font color=\"#00FF00\">", conf$upload$ok)
  } else {
    msg <- "<font color=\"#FF0000\">"
    for (i in seq_len(length(r$unit))) {
      if (r$fail[i]) {
        msg <- paste(msg, "<b>", conf$upload$check[r$unit[i]], "</b>",
                     r$report[i], "<br>")
      }
    }
  }
  paste(msg, "</font>")
}

#' @rdname upload
#' @export
check_upload <- function(df, pool) {

  unit <- character()
  fail <- logical()
  report <- character()

  conf <- get_config()

  for (i in seq_len(length(conf$upload$check))) {
    unit[i] <- names(conf$upload$check)[i]
    r <- do.call(paste0("check_", unit[i]),
                 args = list(df = df, conf = conf, pool = pool))
    fail[i] <- r$fail
    report[i] <- paste(r$report, collapse = ", ")
  }
  list(unit = unit, fail = fail, report = report)
}


#' @rdname upload
#' @export
check_missing_var <- function(df, conf, pool) {

  fail <- TRUE
  report <- setdiff(conf$db$tab$data$insert[conf$upload$data_var_ind],
                    names(df))
  if (length(report) == 0) {
    fail <- FALSE
  }
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
check_invalid_var <- function(df, conf, pool) {

  fail <- TRUE
  report <- setdiff(names(df),
                    conf$db$tab$data$insert[conf$upload$data_var_ind])
  if (length(report) == 0) {
    fail <- FALSE
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_invalid_org <- function(df, conf, pool) {

  fail <- TRUE
  if ("OrgNrShus" %in% names(df)) {
    report <- setdiff(df$OrgNrShus, get_org(pool)$OrgNrShus)
  } else {
    report <- "Field missing"
  }
  if (length(report) == 0) {
    fail <- FALSE
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_invalid_ind <- function(df, conf, pool) {

  fail <- TRUE
  if ("KvalIndID" %in% names(df)) {
    report <- setdiff(df$KvalIndID, get_indicator(pool)$IndID)
  } else {
    report <- "Field missing"
  }
  if (length(report) == 0) {
    fail <- FALSE
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_none_numeric_var <- function(df, conf, pool) {

  fail <- TRUE
  report <- ""
  if ("Variabel" %in% names(df)) {
    if (is.numeric(df$Variabel)) {
      fail <- FALSE
    }
  } else {
    report <- "Field missing"
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_duplicate_delivery <- function(df, conf, pool) {

  fail <- delivery_exist_in_db(pool, df)
  report <- ""
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
csv_to_df <- function(path, sep = ",", dec, encoding = "UTF-8") {

  if(!file.exists(path)) {
    stop(paste("The file", path, "does not exist!"))
  }

  read.csv(path, header = TRUE, sep = sep, dec = dec, fileEncoding = encoding)
}


#' @rdname upload
#' @export
sample_df <- function(df, skip = c(""), n, random = FALSE) {

  if (ncol(df) > length(skip)) {
    df <- df[ , !(names(df) %in% skip)]
  }

  if (n > nrow(df)) {
    n <- nrow(df)
  }

  if (is.na(n)) {
    return(df)
  } else {
    if (random) {
      return(df[sample(1:nrow(df), n), ])
    } else {
      return(df[1:n, ])
    }
  }
}