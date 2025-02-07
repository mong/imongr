#' Shiny module providing GUI and server logic for the indicator tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#' @param pool_verify A database pool object
#' @param registry_tracker Integer defining registry id
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_indicator
#' @aliases indicator_ui indicator_server indicator_app
NULL

#' @rdname mod_project
#' @export
project_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_indicator_registry")),
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("edit_title")),
        shiny::uiOutput(ns("title_oversize")),
        shiny::uiOutput(ns("edit_short")),
        shiny::uiOutput(ns("short_oversize")),
        shiny::uiOutput(ns("edit_long")),
        shiny::uiOutput(ns("long_oversize")),
        shiny::uiOutput(ns("update_text"))
      )
    )
  )
}

#' @rdname mod_project
#' @export
project_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- get_config()
    rv_return <- shiny::reactiveValues()

    output$select_indicator_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("project_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    shiny::observeEvent(input$indicator_registry, {
      rv_return$registry_id <- input$indicator_registry
    })

    return(rv_return)
  })
}