#' Upload data to imongr
#'
#' Various functions used when data are being uploaded to imongr. All checks
#' regarding consistency goes here
#'
#' @param pool Data base pool object
#' @param conf List of configuration
#' @param df Data frame holding indicator data
#' @param ind Data frame with indicators
#' @param registry Integer registry id
#' @param path Character path to a file
#' @param sep Character filed sep
#' @param dec Character decimal sep
#' @param encoding Character encoding
#' @param n Numeric sample size
#' @param skip character vector defining data frame variables to skip
#' @param random Logical sample method
#' @param mail_msg Character vector holding (part of) email body
#' @param return_ind Logical whether indicators should be returned. FALSE by
#'   default
#'
#' @return whatever
#' @importFrom utils read.csv URLencode
#' @name upload
#' @aliases check_report check_upload check_missing_registry check_mixing_ind
#'   check_missing_var check_invalid_var check_invalid_org check_invalid_context
#'   check_invalid_ind check_none_numeric_var
#'   csv_to_df mail_check_report sample_df indicator_is_fraction
#'   filter_fraction_indicator
NULL


#' @rdname upload
#' @export
check_report <- function(registry, df, ind, pool) {

  conf <- get_config()
  r <- check_upload(registry, df, ind, pool)

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
check_upload <- function(registry, df, ind, pool) {

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
      r <- do.call(
        paste0("check_", unit[i]),
        args = list(
          registry = registry,
          df = df,
          ind = ind,
          conf = conf,
          pool = pool
        )
      )
      fail[i] <- r$fail
      report[i] <- paste(paste0("'", r$report, "'"), collapse = ", ")
    }
  }
  list(unit = unit, fail = fail, report = report)
}

#' @rdname upload
#' @export
check_missing_registry <- function(registry, df, ind, conf, pool) {

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
check_mixing_ind <- function(registry, df, ind, conf, pool) {

  # upload cannot contain a mix of fractions and other types of indicators
  ind_is_fraction <- indicator_is_fraction(pool, df, conf, return_ind = TRUE)
  if (all(ind_is_fraction$is_fraction) || all(!ind_is_fraction$is_fraction)) {
    list(fail = FALSE, report = "")
  } else {
    report <- paste(ind_is_fraction$ind[!ind_is_fraction$is_fraction],
                    collapse = ", ")
    list(fail = TRUE, report = report)
  }
}

#' @rdname upload
#' @export
check_missing_var <- function(registry, df, ind, conf, pool) {

  fail <- TRUE
  report <- setdiff(conf$upload$file$vars, names(df))
  if (length(report) == 0) {
    fail <- FALSE
  }
  list(fail = fail, report = report)
}

#' @rdname upload
#' @export
check_invalid_var <- function(registry, df, ind, conf, pool) {

  fail <- TRUE
  report <- setdiff(names(df), conf$upload$file$vars)
  if (length(report) == 0) {
    fail <- FALSE
  }
  list(fail = fail, report = report)
}


#' @rdname upload
#' @export
check_invalid_context <- function(registry, df, ind, conf, pool) {

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
check_invalid_org <- function(registry, df, ind, conf, pool) {

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
check_invalid_ind <- function(registry, df, ind, conf, pool) {

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
check_numeric_var <- function(registry, df, ind, conf, pool) {

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
check_natural_var <- function(registry, df, ind, conf, pool) {

  fail <- TRUE
  report <- ""

  if ("var" %in% names(df) && is.numeric(df$var) && "ind_id" %in% names(df)) {

    # check only on indicator data that are true fractions
    df <- filter_fraction_indicator(pool, df, conf)
    if (dim(df)[1] < 1) {
      return(list(fail = FALSE, report = ""))
    }

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
check_overflow_var <- function(registry, df, ind, conf, pool) {

  fail <- TRUE
  report <- ""
  if ("var" %in% names(df) && is.numeric(df$var) &&
      "denominator" %in% names(df) && is.numeric(df$denominator) &&
      "ind_id" %in% names(df)) {

    # check only on indicator data that are true fractions
    df <- filter_fraction_indicator(pool, df, conf)
    if (dim(df)[1] < 1) {
      return(list(fail = FALSE, report = ""))
    }

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
check_numeric_denominator <- function(registry, df, ind, conf, pool) {

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
check_natural_denominator <- function(registry, df, ind, conf, pool) {

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
check_zero_denominator <- function(registry, df, ind, conf, pool) {

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
check_duplicated_inds <- function(registry, df, ind, conf, pool) {

  fail <- FALSE
  report <- ""

  if ("ind_id" %in% names(df)) {

    ind_id_type <- data.frame(ind_id = ind$id, type = ind$type)
    orgnr <- get_all_orgnr(pool)


    df_calc <- df %>%
      dplyr::left_join(ind_id_type, by = "ind_id") %>%
      dplyr::left_join(orgnr, by = "orgnr") %>%
      dplyr::filter(grepl("beregnet", type)) %>%
      dplyr::select(ind_id, orgnr, context, unit_level, year) %>%
      dplyr::group_by_all() %>%
      dplyr::count() %>%
      dplyr::filter(n > 1)

    if (nrow(df_calc) > 0) {
      fail <- TRUE
      report <- unique(df_calc$ind_id)
    }
  } else {
    report <- conf$upload$check_impossible
  }
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


#' @rdname upload
#' @export
indicator_is_fraction <- function(pool, df, conf, return_ind = FALSE) {

  ind_id <- unique(df$ind_id)
  ind <- imongr::get_table(pool, "ind")
  ind <- ind %>%
    dplyr::filter(.data$id %in% ind_id) %>%
    dplyr::select("id", "type")

  if (!return_ind) {
    ind$type %in% conf$var$fraction$type
  } else {
    data.frame(ind = ind$id, is_fraction = ind$type %in% conf$var$fraction$type)
  }

}

#' @rdname upload
#' @export
filter_fraction_indicator <- function(pool, df, conf) {

  frac <- indicator_is_fraction(pool, df, conf, return_ind = TRUE)

  df %>%
    dplyr::left_join(frac, by = c("ind_id" = "ind")) %>%
    dplyr::filter(.data$is_fraction) %>%
    dplyr::select(-c("is_fraction"))
}
