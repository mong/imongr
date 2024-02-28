#' @rdname mod_ekspertgruppen
#' @eksport
ekspertgruppen_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_registry")),
        shiny::uiOutput(ns("select_year"))
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("vurdering"))
      )
    )
  )
}

#' @rdname mod_ekspertgruppen
#' @eksport
ekspertgruppen_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- get_config()

    rv <- shiny::reactiveValues(
      vurdering_fritekst = ""
    )

    rv_return <- shiny::reactiveValues()


    # Track indicator and registry
    shiny::observeEvent(input$indicator_registry, {
      rv_return$registry_id <- input$indicator_registry
    })

    shiny::observeEvent(input$indicator_registry, {
      rv_return$registry_id <- input$indicator_registry
    })



    output$select_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("indicator_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    output$vurdering <- shiny::renderUI({
      shiny::textAreaInput(
        ns("vurdering_fritekst"), "Vurdering av \u00e5rsrapporten",
        value = rv$vurdering_fritekst, width = "90%", rows = 8
      )
    })

    return(rv_return)
  })
}

#' @rdname mod_ekspertgruppen
#' @eksport
ekspertgruppen_app <- function(pool_verify) {
  ui <- shiny::fluidPage(
    ekspertgruppen_ui("ekspertgruppen")
  )

  server <- function(input, output, session) {
    ekspertgruppen_server("ekspertgruppen", pool_verify)
  }

  shiny::shinyApp(ui, server)
}