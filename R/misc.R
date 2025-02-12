#' Tools and whatever
#'
#' @param vals A numeric vector
#' @param tolerance A (small) positive floating point number used to evaluate if
#'   a numeric can be regarded as a whole number. Default depends on the running
#'   environment and set to \code{.Machine$double.eps^0.5}
#' @param df A data frame
#' @param ind A data frame holding indicator data.
#' @param include_data_table Logical defining if the data table is to be
#' populated by data too. By default TRUE
#' @param newline String element defining line break for formatting. Default is
#' \code{<br>}
#' @param prompt Logical to prompt for user input. Default is TRUE
#' @return Invisible
#' @name misc
#' @aliases natural md5_checksum user_widget version_info no_opt_out_ok
#' insert_sample_data delete_all_data
NULL


#' @rdname misc
#' @export
natural <- function(vals, tolerance = .Machine$double.eps^0.5) {
  if (any(is.na(vals)) || !is.numeric(vals)) {
    return(FALSE)
  }
  if (any(vals < 0)) {
    return(FALSE)
  }
  abs(vals - round(vals)) < tolerance
}


#' @rdname misc
#' @export
md5_checksum <- function(df, ind = "") {
  f1 <- tempfile()
  f2 <- tempfile()
  utils::write.csv(df, file = f1)
  utils::write.csv(ind, file = f2)

  fc <- file(f1, "r")
  t1 <- readLines(fc)
  close(fc)

  fc <- file(f2, "r")
  t2 <- readLines(fc)
  close(fc)

  t <- paste0(paste0(t1, collapse = ""), paste0(t2, collapse = ""))

  digest::digest(t, algo = "md5", serialize = FALSE)
}


#' @rdname misc
#' @export
user_widget <- function() {
  conf <- get_config()

  bslib::nav_menu(
    get_user_name(),
    align = "right",
    bslib::nav_item(
      shiny::tags$a(
        shiny::icon("info-circle"),
        id = "app_info",
        href = "#",
        class = "action-button",
        "Informasjon",
      ),
      shiny::tags$a(
        shiny::icon("sign-out-alt"),
        conf$profile$logout$text,
        href = conf$profile$logout$url
      )
    )
  )
}


#' @rdname misc
#' @export
#' @importFrom utils installed.packages
#' @examples
#' version_info()
version_info <- function(newline = "<br>") {
  conf <- get_config()
  pkg <- conf$app_text$info$version$app
  vpkg <- installed.packages()[pkg, 3]
  paste0(pkg, " v", vpkg, newline, collapse = "")
}


#' @rdname misc
#' @export
#' @examples
#' no_opt_out_ok()
no_opt_out_ok <- function() {
  conf <- get_config()
  msg <- conf$app_text$action_button$no_opt_out_ok
  sample(msg, 1)
}

#' @rdname misc
#' @export
insert_sample_data <- function(include_data_table = TRUE) {
  pool <- make_pool()

  insert_table(pool, table = "nation", df = imongr::nation)
  insert_table(pool, table = "rhf", df = imongr::rhf)
  insert_table(pool, table = "hf", df = imongr::hf)
  insert_table(pool, table = "hospital", df = imongr::hospital)
  insert_table(pool, table = "registry", df = imongr::registry)
  insert_table(pool, table = "ind", df = imongr::ind)
  insert_table(pool, table = "user", df = imongr::user)
  insert_table(pool, table = "user_registry", df = imongr::user_registry)
  insert_table(pool, table = "publish", df = imongr::publish)
  insert_table(pool, table = "user_registry", df = imongr::user_registry)
  insert_table(pool, table = "delivery", df = imongr::delivery)
  insert_table(pool, table = "medfield", df = imongr::medfield)
  insert_table(pool,
    table = "registry_medfield",
    df = imongr::registry_medfield
  )

  if (include_data_table) {
    insert_table(pool, table = "data", df = imongr::data)
  }

  drain_pool(pool)

  invisible()
}


#' @rdname misc
#' @export
delete_all_data <- function(prompt = TRUE) {
  if (prompt) {
    ans <- readline(paste(
      "WARNING! This will delete all data from the db.",
      "If this is the intention type 'YES' now "
    ))
  } else {
    ans <- "YES"
  }

  if (ans == "YES") {
    conf <- get_config()
    tabs <- names(conf$db$tab)
    query <- "DROP TABLE IF EXISTS "
    pool <- make_pool()
    message("...dropping tables...")
    pool::dbExecute(pool, "ALTER TABLE `delivery` DROP FOREIGN KEY `fk_delivery_publish`;")
    for (i in seq_len(length(tabs))) {
      pool::dbExecute(pool, paste0(query, tabs[i], ";"))
    }
    fc <- file(system.file("2_create_tabs.sql", package = "imongr"), "r")
    t <- readLines(fc)
    close(fc)
    sql <- paste0(t, collapse = "\n")
    queries <- strsplit(sql, ";")[[1]]
    message("...recreating tables...")
    for (i in seq_len(length(queries))) {
      pool::dbExecute(pool, queries[i])
    }
    drain_pool(pool)
    message("Done.")
  } else {
    message("Aborting")
  }

  invisible()
}

#' @rdname misc
#' @export
invalidate_cache <- function() {
  login_info <- Sys.getenv("AWS_ACCESS_KEY_ID")
  which_aws <- system("which aws")
  if (login_info == "" || which_aws != 0) {
    return(NULL)
  }
  system("aws sts get-caller-identity")
  system("aws cloudfront create-invalidation --distribution-id ${distribution_id} --path \"/*\"")
  message("Invaliderte cache")
}

#' @rdname misc
#' @noRd
validateName <- function(x, existing_names) {
  if (is.null(x)) {
    return(NULL)
  } else {
    if (grepl("^[a-zA-Z0-9_]+$", x) && !(tolower(x) %in% tolower(existing_names))) {
      return(NULL)
    } else {
      return(
        "Kan ikke være tom, inneholde mellomrom eller spesialtegn, 
        eller v\u00e6re lik en et eksisterende navn."
      )
    }
  }
}
