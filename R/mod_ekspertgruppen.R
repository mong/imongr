#' @rdname mod_ekspertgruppen
#' @eksport
ekspertgruppen_ui <- function(id) {

  ns <- shiny::NS(id)
  conf <- get_config()

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Vurdering av \u00e5rsrapporter"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_registry")),
        shiny::numericInput(
          ns("oppgitt_dg"),
          "DG i % oppgitt av regionene utfra \u00e5rsrapporter", 
          value = 0,
          min = 0,
          max = 100,
          step = 1
        ),
        shiny::br(),
        shiny::hr(),
        shiny::br(),
        shiny::textOutput(ns("vurdert_stadium")),
        shiny::br(),
        shiny::hr(),
        shiny::br(),
        shiny::actionButton(
          ns("send_inn"),
          "Send inn vurdering",
          shiny::icon("paper-plane")
        )
      ),
      shiny::mainPanel(
        shiny::h3("Stadium 1"),
        shiny::checkboxInput(ns("S1_1"), conf$vurdering$stadium_1$krav_1, width = "100%"),
        shiny::checkboxInput(ns("S1_2"), conf$vurdering$stadium_1$krav_2, width = "100%"),
        shiny::checkboxInput(ns("S1_3"), conf$vurdering$stadium_1$krav_3, width = "100%"),
        shiny::checkboxInput(ns("S1_4"), conf$vurdering$stadium_1$krav_4, width = "100%"),
        shiny::checkboxInput(ns("S1_5"), conf$vurdering$stadium_1$krav_5, width = "100%"),
        shiny::checkboxInput(ns("S1_6"), conf$vurdering$stadium_1$krav_6, width = "100%"),
        shiny::checkboxInput(ns("S1_7"), conf$vurdering$stadium_1$krav_7, width = "100%"),
        shiny::h3("Stadium 2"),
        shiny::checkboxInput(ns("S2_1"), conf$vurdering$stadium_2$krav_1, width = "100%"),
        shiny::checkboxInput(ns("S2_2"), conf$vurdering$stadium_2$krav_2, width = "100%"),
        shiny::checkboxInput(ns("S2_3"), conf$vurdering$stadium_2$krav_3, width = "100%"),
        shiny::checkboxInput(ns("S2_4"), conf$vurdering$stadium_2$krav_4, width = "100%"),
        shiny::checkboxInput(ns("S2_5"), conf$vurdering$stadium_2$krav_5, width = "100%"),
        shiny::h3("Stadium 3"),
        shiny::checkboxInput(ns("S3_1"), conf$vurdering$stadium_3$krav_1, width = "100%"),
        shiny::checkboxInput(ns("S3_2"), conf$vurdering$stadium_3$krav_2, width = "100%"),
        shiny::checkboxInput(ns("S3_3"), conf$vurdering$stadium_3$krav_3, width = "100%"),
        shiny::checkboxInput(ns("S3_4"), conf$vurdering$stadium_3$krav_4, width = "100%"),
        shiny::checkboxInput(ns("S3_5"), conf$vurdering$stadium_3$krav_5, width = "100%"),
        shiny::checkboxInput(ns("S3_6"), conf$vurdering$stadium_3$krav_5, width = "100%"),
        shiny::h3("Stadium 4"),
        shiny::checkboxInput(ns("S4_1"), conf$vurdering$stadium_4$krav_1, width = "100%"),
        shiny::checkboxInput(ns("S4_2"), conf$vurdering$stadium_4$krav_2, width = "100%"),
        shiny::checkboxInput(ns("S4_3"), conf$vurdering$stadium_4$krav_3, width = "100%"),
        shiny::checkboxInput(ns("S4_4"), conf$vurdering$stadium_4$krav_4, width = "100%"),
        shiny::checkboxInput(ns("S4_5"), conf$vurdering$stadium_4$krav_5, width = "100%"),
        shiny::h3("Niv\u00e5 A"),
        shiny::checkboxInput(ns("N_A"), conf$vurdering$nivaa_A, width = "100%"),
        shiny::h3("Niv\u00e5 B"),
        shiny::checkboxInput(ns("N_B"), conf$vurdering$nivaa_B, width = "100%"),
        shiny::h3("Niv\u00e5 C"),
        shiny::checkboxInput(ns("N_C"), conf$vurdering$nivaa_C, width = "100%"),
        shiny::h3("Vurdering"),
        shiny::uiOutput(ns("vurdering")),
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

    output$select_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("indicator_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    output$vurdert_stadium <- shiny::renderText("Registeret vurderes til: ")

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