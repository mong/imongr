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
        shiny::uiOutput(ns("select_year")),
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
        shiny::h3("Stadium 2"),
        shiny::checkboxInput(ns("S2_1"), conf$vurdering$stadium_2$krav_01, width = "100%"),
        shiny::checkboxInput(ns("S2_2"), conf$vurdering$stadium_2$krav_02, width = "100%"),
        shiny::checkboxInput(ns("S2_3"), conf$vurdering$stadium_2$krav_03, width = "100%"),
        shiny::checkboxInput(ns("S2_4"), conf$vurdering$stadium_2$krav_04, width = "100%"),
        shiny::checkboxInput(ns("S2_5"), conf$vurdering$stadium_2$krav_05, width = "100%"),
        shiny::h3("Stadium 3"),
        shiny::checkboxInput(ns("S3_1"), conf$vurdering$stadium_3$krav_06, width = "100%"),
        shiny::checkboxInput(ns("S3_2"), conf$vurdering$stadium_3$krav_07, width = "100%"),
        shiny::checkboxInput(ns("S3_3"), conf$vurdering$stadium_3$krav_08, width = "100%"),
        shiny::checkboxInput(ns("S3_4"), conf$vurdering$stadium_3$krav_09, width = "100%"),
        shiny::checkboxInput(ns("S3_5"), conf$vurdering$stadium_3$krav_10, width = "100%"),
        shiny::checkboxInput(ns("S3_6"), conf$vurdering$stadium_3$krav_11, width = "100%"),
        shiny::h3("Stadium 4"),
        shiny::checkboxInput(ns("S4_1"), conf$vurdering$stadium_4$krav_12, width = "100%"),
        shiny::checkboxInput(ns("S4_2"), conf$vurdering$stadium_4$krav_13, width = "100%"),
        shiny::checkboxInput(ns("S4_3"), conf$vurdering$stadium_4$krav_14, width = "100%"),
        shiny::checkboxInput(ns("S4_4"), conf$vurdering$stadium_4$krav_15, width = "100%"),
        shiny::checkboxInput(ns("S4_5"), conf$vurdering$stadium_4$krav_16, width = "100%"),
        shiny::h3("Niv\u00e5 A"),
        shiny::checkboxInput(ns("N_A"), conf$vurdering$nivaa_A, width = "100%"),
        shiny::uiOutput(ns("N_A_kommentar")),
        shiny::h3("Niv\u00e5 B"),
        shiny::checkboxInput(ns("N_B"), conf$vurdering$nivaa_B, width = "100%"),
        shiny::uiOutput(ns("N_B_kommentar")),
        shiny::h3("Niv\u00e5 C"),
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
      vurdering_fritekst = "",
      stadium2 = FALSE,
      stadium3 = FALSE,
      stadium4 = FALSE,
      level = "",
      vurdering = ""
    )

    rv_return <- shiny::reactiveValues()

    # Track indicator and registry
    shiny::observeEvent(input$indicator_registry, {
      rv_return$registry_id <- input$indicator_registry
    })


    output$select_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("selected_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    output$select_year <- shiny::renderUI({
      shiny::selectInput(
        ns("selected_year"),
        "Velg \u00e5r",
        c(2022 : format(Sys.Date(), "%Y"))
      )
    })

    output$vurdert_stadium <- shiny::renderText("Registeret vurderes til: ")

    output$vurdering <- shiny::renderUI({
      shiny::textAreaInput(
        ns("vurdering_fritekst"), "Vurdering av \u00e5rsrapporten",
        value = rv$vurdering_fritekst, width = "90%", rows = 8
      )
    })

    output$N_A_kommentar <- shiny::renderUI({
      shiny::textAreaInput(
        ns("N_A_fritekst"), "Kommentarer til niv\u00e5 A",
        value = rv$vurdering_fritekst, width = "90%", rows = 4
      )
    })

    output$N_B_kommentar <- shiny::renderUI({
      shiny::textAreaInput(
        ns("N_B_fritekst"), "Kommentarer til niv\u00e5 B",
        value = rv$vurdering_fritekst, width = "90%", rows = 4
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