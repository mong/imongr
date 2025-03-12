#' Shiny module providing GUI and server logic for the selected indicators tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#' @param pool_verify A database pool object
#' @param registry_tracker Integer defining registry id
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_selected_indicators
#' @aliases selected_indicators_ui selected_indicators_server
NULL

#' @rdname mod_selected_indicators
#' @export
selected_indicators_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_indicator_registry")),
        shiny::uiOutput(ns("select_indicator")),
        shiny::hr(),
        shiny::uiOutput(ns("update_indicator_val")),
        shiny::uiOutput(ns("message"))
      ),
       shiny::mainPanel()
    )
  )
}

#' @rdname mod_selected_indicators
#' @export
selected_indicators_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}