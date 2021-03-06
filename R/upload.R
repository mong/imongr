#' Upload data to imongr
#'
#' Various functions used when data are being uploaded to imongr. All checks
#' regarding consistency goes here
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
#' @param mail_msg Character vector holding (part of) email body
#'
#' @return whatever
#' @importFrom utils read.csv URLencode
#' @name upload
#' @aliases check_report check_upload check_missing_registry check_missing_var
#' check_invalid_var check_invalid_org check_invalid_context check_invalid_ind
#' check_none_numeric_var check_duplicate_delivery csv_to_df mail_check_report
#' sample_df
NULL


#' @rdname upload
#' @export
check_report <- function(registry, df, pool) {

  conf <- get_config()
  r <- check_upload(registry, df, pool)

  if (all(!r$fail)) {
    msg <- paste("<font color=\"#00B300\">", conf$upload$ok)
  } else {
    mail_msg <- ""
    msg <- "<font color=\"#FF0000\">"
    for (i in seq_len(length(r$unit))) {
      if (r$fail[i]) {
        msg <- paste(msg, "<b>", conf$upload$check[r$unit[i]], "</b>",
                     r$report[i], "<br>")
        mail_msg <- paste(mail_msg, conf$upload$check[r$unit[i]],
                          r$report[i], "\n")
      }
    }
    msg <- paste(msg, mail_check_report(pool, registry, mail_msg))
  }
  paste(msg, "</font>")
}


#' @rdname upload
#' @export
mail_check_report <- function(pool, registry, mail_msg) {

  user <- get_user_data(pool)
  to <- "mailto:mong@skde.no"
  subject <- paste("imongr: Feilmelding ved opplasting",
                   get_registry_name(pool, registry))
  body <- paste("Hei,",
                "\n\nDet kan godt hende jeg trenger hjelp med f\u00f8lgende",
                "feil:",
                "\n\n", paste(gsub("'", "", mail_msg)),
                "\n\nSo long og vennlig hilsen,\n", user$name, "\n", user$phone)

  content <- paste0(to, "?subject=", URLencode(subject), "&body=",
                    URLencode(body))

  paste0("<a href='", content, "'>Send feilmelding til SKDE</a>")
}


#' @rdname upload
#' @export
check_upload <- function(registry, df, pool) {

  unit <- character()
  fail <- logical()
  report <- character()

  conf <- get_config()

  # special case if there registry is not defined
  if (registry == "") {
    unit <- "missing_registry"
    fail <- TRUE
    report <- conf$upload$check_empty
  } else {
    for (i in seq_len(length(conf$upload$check))) {
      unit[i] <- names(conf$upload$check)[i]
      r <- do.call(paste0("check_", unit[i]),
                   args = list(registry = registry, df = df, conf = conf,
                               pool = pool))
      fail[i] <- r$fail
      report[i] <- paste(paste0("'", r$report, "'"), collapse = ", ")
    }
  }
  list(unit = unit, fail = fail, report = report)
}

#' @rdname upload
#' @export
check_missing_registry <- function(registry, df, conf, pool) {

  # pro forma, will never fail but present to maintain consistent config
  fail <- FALSE
  report <- character()
  if (registry == "") {
    fail <- TRUE
    report <- conf$upload$check_empty
  }
  list(fail = fail, report = report)
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
check_invalid_context <- function(registry, df, conf, pool) {

  fail <- TRUE
  if ("context" %in% names(df)) {
    report <- setdiff(df$context, conf$upload$file$vals$context)
  } else {
    report <- conf$upload$check_empty
  }
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
    report <- conf$upload$check_empty
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
    report <- setdiff(df$ind_id,
                      get_registry_indicators(pool, registry)$id)
  } else {
    report <- conf$upload$check_empty
  }
  if (length(report) == 0) {
    fail <- FALSE
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_numeric_var <- function(registry, df, conf, pool) {

  fail <- TRUE
  report <- ""
  if ("var" %in% names(df)) {
    if (is.numeric(df$var)) {
      fail <- FALSE
    }
  } else {
    report <- conf$upload$check_empty
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_natural_var <- function(registry, df, conf, pool) {

  fail <- TRUE
  report <- ""
  if ("var" %in% names(df) && is.numeric(df$var)) {
    if (all(natural(df$var))) {
      fail <- FALSE
    }
  } else {
    report <- conf$upload$check_impossible
  }
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
check_overflow_var <- function(registry, df, conf, pool) {

  fail <- TRUE
  report <- ""
  if ("var" %in% names(df) && is.numeric(df$var) &&
      "denominator" %in% names(df) && is.numeric(df$denominator)) {
    if (all(df$var <= df$denominator)) {
      fail <- FALSE
    }
  } else {
    report <- conf$upload$check_impossible
  }
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
check_numeric_denominator <- function(registry, df, conf, pool) {

  fail <- TRUE
  report <- ""
  if ("denominator" %in% names(df)) {
    if (is.numeric(df$denominator)) {
      fail <- FALSE
    }
  } else {
    report <- conf$upload$check_empty
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_natural_denominator <- function(registry, df, conf, pool) {

  fail <- TRUE
  report <- ""
  if ("denominator" %in% names(df) && is.numeric(df$denominator)) {
    if (all(natural(df$denominator))) {
      fail <- FALSE
    }
  } else {
    report <- conf$upload$check_impossible
  }
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
check_zero_denominator <- function(registry, df, conf, pool) {

  fail <- TRUE
  report <- ""
  if ("denominator" %in% names(df) && is.numeric(df$denominator)) {
    if (all(df$denominator > 0)) {
      fail <- FALSE
    }
  } else {
    report <- conf$upload$check_impossible
  }
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
check_duplicate_delivery <- function(registry, df, conf, pool) {

  fail <- duplicate_delivery(pool, df)
  report <- ""
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
csv_to_df <- function(path, sep = ",", dec, encoding = "UTF-8") {

  if (!file.exists(path)) {
    stop(paste("The file", path, "does not exist!"))
  }

  std_enc <- c("UTF-8", "LATIN1")

  tryCatch(
    withCallingHandlers({
        df <- read.csv(path, header = TRUE, sep = sep, dec = dec,
                       fileEncoding = encoding, stringsAsFactors = FALSE)
      },
      warning = function(w) {
        alternative_encoding <- setdiff(std_enc, encoding)
        warning(paste("imongr is trying the alternative encoding",
                      alternative_encoding, "when reading a csv file",
                      "hopefully recovering from initial warning:\n\t", w))
        df <<- read.csv(path, header = TRUE, sep = sep, dec = dec,
                        fileEncoding = alternative_encoding,
                        stringsAsFactors = FALSE)
        invokeRestart("silent")
      }
      ),
    error = function(e) {
      return(e)
    },
    finally = {
    }
  )

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
