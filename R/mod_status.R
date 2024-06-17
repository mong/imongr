#' Shiny module providing UI and server functions for registry status overview
#'
#' @param id Character string module namespace
#' @param pool A database pool object connecting to production data
#' @param pool_verify A database pool object connecting to staging data
#'
#' @return Shiny objects for the imongr app
#'
#' #' @name mod_status
#' @aliases status_ui status_server status_app
NULL

format_data <- function(dat) {
  # Initialise data frame with column names
  years <- sort(unique(dat$year))
  registries <- sort(unique(dat$registry_id))
  dat_format <- data.frame(matrix(ncol = length(years) + 1, nrow = length(registries)))
  colnames(dat_format) <- c("Register", years)

  dat_format$Register <- registries


  for (i in seq_along(registries)) {
    for (j in seq_along(years)) {
      verdict <- dat[dat$registry_id == registries[i] & dat$year == years[j], ]

      if (ncol(verdict) > 0 && nrow(verdict) > 0) {
        dat_format[i, j + 1] <- verdict$verdict
      } else {
        dat_format[i, j + 1] <- NA
      }
    }
  }

  return(dat_format)
}

#' @rdname mod_status
#' @export
status_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    DT::dataTableOutput(ns("status_table"))
  )
}

#' @rdname mod_status
#' @export
status_server <- function(id, pool, pool_verify) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      dat <- pool::dbGetQuery(pool, "SELECT registry_id, year, verdict FROM evaluation")

      rv = shiny::reactiveValues(
        dat_format = format_data(dat)
      )

      output$status_table <- DT::renderDataTable(rv$dat_format)
    }
  )
}

#' @rdname mod_status
#' @export
status_app <- function(pool, pool_verify) {
  ui <- shiny::fluidPage(
    status_ui("status")
  )

  server <- function(input, output, session) {
    status_server("status", pool, pool_verify)
  }

  shiny::shinyApp(ui, server)
}