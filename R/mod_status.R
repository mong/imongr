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

      output$status_table <- DT::renderDataTable(dat)
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