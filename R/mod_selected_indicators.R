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
        shiny::uiOutput(ns("select_registry")),
        shiny::uiOutput(ns("select_indicator")),
        shiny::hr(),
        shiny::uiOutput(ns("update_indicators")),
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
    conf <- get_config()

    rv_return <- shiny::reactiveValues()

    output$select_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("selected_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    shiny::observeEvent(input$selected_registry, {
      rv_return$registry_id <- input$selected_registry
    })

    output$select_indicator <- shiny::renderUI({
      shiny::req(input$selected_registry)
      shiny::selectInput(
        ns("selected_indicator"), "Velg indikator:",
        choices = get_registry_indicators(pool_verify, input$selected_registry)$id
      )
    })

    return(rv_return)
  })
}