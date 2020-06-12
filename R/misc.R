#' Tools and whatever
#'
#' @param include_data_table Logical defining if the data table is to be
#' populated by data too. By default TRUE
#' @param newline String element defining line break for formatting. Default is
#' \code{<br>}
#' @return Invisible
#' @name misc
#' @aliases navbar_widget version_info no_opt_out_ok insert_sample_data
#' delete_all_data
NULL


#' @rdname misc
#' @export
navbar_widget <- function() {

  conf <- get_config()

  app_info <- shiny::tags$a(
    id = "app_info",
    href = "#",
    class = "action-button",
    "Info")

  logout <- shiny::tags$a(
    id = "logout",
    href = conf$profile$logout$url,
    conf$profile$logout$text)

  txt_widget <-
    paste0("var header = $('.navbar> .container-fluid');\n",
           "header.append('<div class=\"navbar-brand\" style=\"float:right; ",
           "vertical-align:baseline ;font-size:70%\">",
           app_info, "<br>",
           get_user_name(), "<br>",
           logout,
           "</div>');\n")

  shiny::tags$script(shiny::HTML(txt_widget))
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
  msg <- conf$app_text$info$action_button$no_opt_out_ok
  sample(msg, 1)

}

#' @rdname misc
#' @export
insert_sample_data <- function(include_data_table = TRUE) {

  pool <- make_pool()

  insert_tab(pool, table = "registry", df = imongr::registry)
  insert_tab(pool, table = "org", df = imongr::org)
  insert_tab(pool, table = "indicator", df = imongr::indicator)
  insert_tab(pool, table = "user", df = imongr::user)
  insert_tab(pool, table = "user_registry", df = imongr::user_registry)
  insert_tab(pool, table = "delivery", df = imongr::delivery)

  if (include_data_table) {
    insert_tab(pool, table = "data", df = imongr::data)
  }

  drain_pool(pool)

  invisible()
}


#' @rdname misc
#' @export
delete_all_data <- function() {

  ans <- readline(paste("WARNING! This will delete all data from the db.",
                        "If this is the intention type 'YES' now "))

  if (ans == "YES") {
    conf <- get_config()
    tabs <- names(conf$db$tab)
    query <- "DROP TABLE "
    pool <- make_pool()
    message("...dropping tables...")
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
