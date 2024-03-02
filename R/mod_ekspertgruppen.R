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
        shiny::uiOutput(ns("lagre"))
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
      stadium = NA,
      level = NA,
      vurdering = (rep(FALSE, 18)),
      table_data = data.frame(user_id = get_user_id(pool)),
    )

    rv_return <- shiny::reactiveValues()

    update_form <- shiny::reactive({
      list(input$selected_registry, input$selected_year)
    })

    # Hold oversikt over valgt register
    shiny::observeEvent(input$indicator_registry, {
      rv_return$registry_id <- input$indicator_registry
    })

    #######################
    ##### UI sidemeny #####
    #######################

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
        c(2022L : as.numeric(format(Sys.Date(), "%Y")) - 1),
        selected = 2023
      )
    })

    output$vurdert_stadium <- shiny::renderText(paste0("Registeret vurderes til: ", rv$stadium, rv$level))

    # Gjem knapp hvis årstall ikke er fjoråret
    output$lagre <- shiny::renderUI({
      shiny::req(input$selected_registry)
      shiny::validate(
        shiny::need(dplyr::between(input$oppgitt_dg, 0, 100), "Dekningsgrad må være fra 0 til 100\n"),
        shiny::need(input$selected_year == as.numeric(format(Sys.Date(), "%Y")) - 1,
                    "Redigering tillates kun på aktuelt rapporteringsår")
      )
      shiny::actionButton(
        ns("send_inn"),
        "Lagre",
        shiny::icon("floppy-disk")
      )
    })

    #######################
    ##### UI tekstfelt ####
    #######################

    output$vurdering <- shiny::renderUI({
      shiny::textAreaInput(
        ns("vurdering_fritekst"), "Vurdering av \u00e5rsrapporten",
        value = "", width = "90%", rows = 8
      )
    })

    output$N_A_kommentar <- shiny::renderUI({
      shiny::textAreaInput(
        ns("N_A_fritekst"), "Kommentarer til niv\u00e5 A",
        value = "", width = "90%", rows = 4
      )
    })

    output$N_B_kommentar <- shiny::renderUI({
      shiny::textAreaInput(
        ns("N_B_fritekst"), "Kommentarer til niv\u00e5 B",
        value = "", width = "90%", rows = 4
      )
    })

    ################################
    ##### Reaktivitet sidemeny #####
    ################################

    shiny::observeEvent(input$selected_registry, {
      rv$table_data$registry_id <- input$selected_registry
    })

    shiny::observeEvent(input$selected_year, {
      rv$table_data$year <- input$selected_year
    })

    shiny::observeEvent(input$oppgitt_dg, {
      rv$table_data$oppgitt_dg <- input$oppgitt_dg
    })

    shiny::observeEvent(input$N_A_fritekst, {
      rv$table_data$fritekst_A <- input$N_A_fritekst
    })

    shiny::observeEvent(input$N_B_fritekst, {
      rv$table_data$fritekst_B <- input$N_B_fritekst
    })

    shiny::observeEvent(input$vurdering_fritekst, {
      rv$table_data$vurdering <- input$vurdering_fritekst
    })

    shiny::observeEvent(rv$vurdering, {
      rv$stadium <- 1

      if (all(rv$vurdering[1:16])) {
        rv$stadium <- 4
        rv$table_data$stadium <- 1
      } else if (all(rv$vurdering[1:11])) {
        rv$stadium <- 3
        rv$table_data$stadium <- 3
      } else if (all(rv$vurdering[1:5])) {
        rv$stadium <- 2
        rv$table_data$stadium <- 2
      }

      rv$level <- "C"

      if (all(rv$vurdering[17:18])) {
        rv$level <- "A"
      } else if (rv$vurdering[18]) {
        rv$level <- "B"
      }

      rv$table_data$stadium <- paste0(rv$stadium, rv$level)
    })

    ##### Lagre #####
    shiny::observeEvent(input$send_inn, {

      update_vurdering(pool, rv$table_data, input$selected_registry, input$selected_year)

      shinyalert::shinyalert("Ferdig",
        "Dine data er n\u00e5 lagret",
        type = "success",
        showConfirmButton = FALSE,
        timer = 2000
      )
    })

    ##############################
    ##### Reaktivitet skjema #####
    ##############################

    # Stadium 2
    shiny::observeEvent(input$S2_1, {
      rv$vurdering[1] <- input$S2_1
      rv$table_data$krav_01 <- as.numeric(input$S2_1)
    })

    shiny::observeEvent(input$S2_2, {
      rv$vurdering[2] <- input$S2_2
      rv$table_data$krav_02 <- as.numeric(input$S2_2)
    })

    shiny::observeEvent(input$S2_3, {
      rv$vurdering[3] <- input$S2_3
      rv$table_data$krav_03 <- as.numeric(input$S2_3)
    })

    shiny::observeEvent(input$S2_4, {
      rv$vurdering[4] <- input$S2_4
      rv$table_data$krav_04 <- as.numeric(input$S2_4)
    })

    shiny::observeEvent(input$S2_5, {
      rv$vurdering[5] <- input$S2_5
      rv$table_data$krav_05 <- as.numeric(input$S2_5)
    })

    # Stadium 3
    shiny::observeEvent(input$S3_1, {
      rv$vurdering[6] <- input$S3_1
      rv$table_data$krav_06 <- as.numeric(input$S3_1)
    })

    shiny::observeEvent(input$S3_2, {
      rv$vurdering[7] <- input$S3_2
      rv$table_data$krav_07 <- as.numeric(input$S3_2)
    })

    shiny::observeEvent(input$S3_3, {
      rv$vurdering[8] <- input$S3_3
      rv$table_data$krav_08 <- as.numeric(input$S3_3)
    })

    shiny::observeEvent(input$S3_4, {
      rv$vurdering[9] <- input$S3_4
      rv$table_data$krav_09 <- as.numeric(input$S3_4)
    })

    shiny::observeEvent(input$S3_5, {
      rv$vurdering[10] <- input$S3_5
      rv$table_data$krav_10 <- as.numeric(input$S3_5)
    })

    shiny::observeEvent(input$S3_6, {
      rv$vurdering[11] <- input$S3_6
      rv$table_data$krav_11 <- as.numeric(input$S3_6)
    })

    # Stadium 4
    shiny::observeEvent(input$S4_1, {
      rv$vurdering[12] <- input$S4_1
      rv$table_data$krav_12 <- as.numeric(input$S4_1)
    })

    shiny::observeEvent(input$S4_2, {
      rv$vurdering[13] <- input$S4_2
      rv$table_data$krav_13 <- as.numeric(input$S4_2)
    })

    shiny::observeEvent(input$S4_3, {
      rv$vurdering[14] <- input$S4_3
      rv$table_data$krav_14 <- as.numeric(input$S4_3)
    })

    shiny::observeEvent(input$S4_4, {
      rv$vurdering[15] <- input$S4_4
      rv$table_data$krav_15 <- as.numeric(input$S4_4)
    })

    shiny::observeEvent(input$S4_5, {
      rv$vurdering[16] <- input$S4_5
      rv$table_data$krav_16 <- as.numeric(input$S4_5)
    })

    # Nivå A, B og C
    shiny::observeEvent(input$N_A, {
      rv$vurdering[17] <- input$N_A
      rv$table_data$krav_17 <- as.numeric(input$N_A)
    })

    shiny::observeEvent(input$N_B, {
      rv$vurdering[18] <- input$N_B
      rv$table_data$krav_18 <- as.numeric(input$N_B)
    })

    # Oppdater skjema ved valg av år og register
    shiny::observeEvent(update_form(), {

      dat <- pool::dbGetQuery(pool, "SELECT * FROM vurdering")
      dat <- dat[(dat$year == input$selected_year) & (dat$registry_id == input$selected_registry), ]

      if (nrow(dat) == 1) {
        shiny::updateCheckboxInput(session, "S2_1", value = dat$krav_01[1])
        shiny::updateCheckboxInput(session, "S2_2", value = dat$krav_02[1])
        shiny::updateCheckboxInput(session, "S2_3", value = dat$krav_03[1])
        shiny::updateCheckboxInput(session, "S2_4", value = dat$krav_04[1])
        shiny::updateCheckboxInput(session, "S2_5", value = dat$krav_05[1])
        shiny::updateCheckboxInput(session, "S2_6", value = dat$krav_06[1])
        shiny::updateCheckboxInput(session, "S2_7", value = dat$krav_07[1])
        shiny::updateCheckboxInput(session, "S2_8", value = dat$krav_08[1])
        shiny::updateCheckboxInput(session, "S2_9", value = dat$krav_09[1])
        shiny::updateCheckboxInput(session, "S2_10", value = dat$krav_10[1])
        shiny::updateCheckboxInput(session, "S2_11", value = dat$krav_11[1])
        shiny::updateCheckboxInput(session, "S2_12", value = dat$krav_12[1])
        shiny::updateCheckboxInput(session, "S2_13", value = dat$krav_13[1])
        shiny::updateCheckboxInput(session, "S2_14", value = dat$krav_14[1])
        shiny::updateCheckboxInput(session, "S2_15", value = dat$krav_15[1])
        shiny::updateCheckboxInput(session, "S2_16", value = dat$krav_16[1])
        shiny::updateCheckboxInput(session, "S2_17", value = dat$krav_17[1])
        shiny::updateCheckboxInput(session, "S2_18", value = dat$krav_18[1])

        shiny::updateTextInput(session, "N_A_fritekst", value = dat$fritekst_A)
        shiny::updateTextInput(session, "N_B_fritekst", value = dat$fritekst_B)

        shiny::updateNumericInput(session, "oppgitt_dg", value = dat$oppgitt_dg)

      } else {
        shiny::updateCheckboxInput(session, "S2_1", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_2", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_3", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_4", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_5", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_6", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_7", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_8", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_9", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_10", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_11", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_12", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_13", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_14", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_15", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_16", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_17", value = FALSE)
        shiny::updateCheckboxInput(session, "S2_18", value = FALSE)

        shiny::updateTextInput(session, "N_A_fritekst", value = dat$fritekst_A)
        shiny::updateTextInput(session, "N_B_fritekst", value = dat$fritekst_B)

        shiny::updateNumericInput(session, "oppgitt_dg", value = 0)
      }
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
