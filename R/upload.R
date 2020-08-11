#' Title
#'
#' @param pool Data base pool object
#' @param conf List of configuration
#' @param df Data frame
#' @param registry Integer registry id
#' @param path Character path to a file
#' @param sep Character filed sep
#' @param dec Character decimal sep
#' @param encoding Character encoding
#' @param n Numeric sample size
#' @param skip character vector defining data frame variables to skip
#' @param random Logical sample method
#'
#' @return whatever
#' @importFrom utils read.csv
#' @name upload
#' @aliases check_report check_upload check_missing_var check_invalid_var
#' check_invalid_org check_invalid_ind check_none_numeric_var
#' check_duplicate_delivery csv_to_df sample_df
NULL


#' @rdname upload
#' @export
check_report <- function(registry, df, pool) {

  conf <- get_config()
  r <- check_upload(registry, df, pool)

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
check_upload <- function(registry, df, pool) {

  unit <- character()
  fail <- logical()
  report <- character()

  conf <- get_config()

  for (i in seq_len(length(conf$upload$check))) {
    unit[i] <- names(conf$upload$check)[i]
    r <- do.call(paste0("check_", unit[i]),
                 args = list(registry = registry, df = df, conf = conf,
                             pool = pool))
    fail[i] <- r$fail
    report[i] <- paste(r$report, collapse = ", ")
  }
  list(unit = unit, fail = fail, report = report)
}


#' @rdname upload
#' @export
check_missing_var <- function(registry, df, conf, pool) {

  fail <- TRUE
  report <- setdiff(conf$upload$file$vars, names(df))
  if (length(report) == 0) {
    fail <- FALSE
  }
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
check_invalid_var <- function(registry, df, conf, pool) {

  fail <- TRUE
  report <- setdiff(names(df), conf$upload$file$vars)
  if (length(report) == 0) {
    fail <- FALSE
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_invalid_org <- function(registry, df, conf, pool) {

  fail <- TRUE
  if ("orgnr" %in% names(df)) {
    report <- setdiff(df$orgnr, get_all_orgnr(pool)$orgnr)
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
check_invalid_ind <- function(registry, df, conf, pool) {

  fail <- TRUE

  if ("ind_id" %in% names(df)) {
    indicator <- unique(df$ind_id)
    report <- setdiff(df$ind_id,
                      get_registry_indicators(pool, registry)$id)
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
check_none_numeric_var <- function(registry, df, conf, pool) {

  fail <- TRUE
  report <- ""
  if ("var" %in% names(df)) {
    if (is.numeric(df$var)) {
      fail <- FALSE
    }
  } else {
    report <- "Field missing"
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_duplicate_delivery <- function(registry, df, conf, pool) {

  fail <- delivery_exist_in_db(pool, df)
  report <- ""
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
csv_to_df <- function(path, sep = ",", dec, encoding = "UTF-8") {

  if (!file.exists(path)) {
    stop(paste("The file", path, "does not exist!"))
  }

  df <- read.csv(path, header = TRUE, sep = sep, dec = dec,
                 fileEncoding = encoding)

  if (!"denominator" %in% names(df)) {
    df <- cbind(df, denominator = 1L)
  }

  df
}


#' @rdname upload
#' @export
sample_df <- function(df, skip = c(""), n, random = FALSE) {

  if (ncol(df) > length(skip)) {
    df <- df[, !(names(df) %in% skip)]
  }

  if (is.na(n)) {
    return(df)
  } else {
    if (n > nrow(df)) {
      n <- nrow(df)
    }
    if (random) {
      return(df[sample(seq_len(nrow(df)), n), ])
    } else {
      return(df[1:n, ])
    }
  }
}
