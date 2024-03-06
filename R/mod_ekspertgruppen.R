#' @rdname mod_ekspertgruppen
#' @eksport
ekspertgruppen_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Vurdering av \u00e5rsrapporter"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_registry")),
        shiny::uiOutput(ns("registry_url")),
        shiny::br(),
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
        shiny::uiOutput(ns("radio1")),
        shiny::uiOutput(ns("radio2")),
        shiny::uiOutput(ns("radio3")),
        shiny::uiOutput(ns("radio4")),
        shiny::uiOutput(ns("radio5")),
        shiny::h3("Stadium 3"),
        shiny::uiOutput(ns("radio6")),
        shiny::uiOutput(ns("radio7")),
        shiny::uiOutput(ns("radio8")),
        shiny::uiOutput(ns("radio9")),
        shiny::uiOutput(ns("radio10")),
        shiny::uiOutput(ns("radio11")),
        shiny::h3("Stadium 4"),
        shiny::uiOutput(ns("radio12")),
        shiny::uiOutput(ns("radio13")),
        shiny::uiOutput(ns("radio14")),
        shiny::uiOutput(ns("radio15")),
        shiny::uiOutput(ns("radio16")),
        shiny::h3("Niv\u00e5 A"),
        shiny::uiOutput(ns("radio17")),
        shiny::uiOutput(ns("N_A_kommentar")),
        shiny::h3("Niv\u00e5 B"),
        shiny::uiOutput(ns("radio18")),
        shiny::uiOutput(ns("N_B_kommentar")),
        shiny::h3("Ekspertgruppens vurdering"),
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

    krav_tabell <- pool::dbGetQuery(pool, "SELECT * FROM krav")

    # Vis avkrysningsfelt kun hvis de er aktuelle for valgt år
    check_year_range <- function(ind) {
      return(shiny::req((as.numeric(input$selected_year) %>%
                           dplyr::between(krav_tabell[ind, ]$introduction_year, krav_tabell[ind, ]$last_year)))
      )
    }

    rv <- shiny::reactiveValues(
      stadium = NA,
      level = NA,
      vurdering = (rep(FALSE, 18)),
      table_data = data.frame(user_id = get_user_id(pool)),
      registry_url = NULL
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

    shiny::observeEvent(input$selected_registry, {
      query <- paste0("SELECT url FROM registry WHERE id = ", input$selected_registry)
      dat <- pool::dbGetQuery(pool, query)

      if (!is.na(dat$url)) {
        rv$registry_url <- shiny::a("Hjemmeside til registeret", href = dat$url, target = "_blank")
      } else {
        rv$registry_url <- NULL
      }
    })

    output$registry_url <- shiny::renderUI({
      shiny::tagList(rv$registry_url)
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

    #####################
    ##### UI skjema #####
    #####################

    # Stadium 2
    output$radio1 <- shiny::renderUI({
      check_year_range(1)
      shiny::checkboxInput(ns("krav_1"), krav_tabell[1, ]$criteria, width = "100%")
    })

    output$radio2 <- shiny::renderUI({
      check_year_range(2)
      shiny::checkboxInput(ns("krav_2"), krav_tabell[2, ]$criteria, width = "100%")
    })

    output$radio3 <- shiny::renderUI({
      check_year_range(3)
      shiny::checkboxInput(ns("krav_3"), krav_tabell[3, ]$criteria, width = "100%")
    })

    output$radio4 <- shiny::renderUI({
      check_year_range(4)
      shiny::checkboxInput(ns("krav_4"), krav_tabell[4, ]$criteria, width = "100%")
    })

    output$radio5 <- shiny::renderUI({
      check_year_range(5)
      shiny::checkboxInput(ns("krav_5"), krav_tabell[5, ]$criteria, width = "100%")
    })

    # Stadium 3
    output$radio6 <- shiny::renderUI({
      check_year_range(6)
      shiny::checkboxInput(ns("krav_6"), krav_tabell[6, ]$criteria, width = "100%")
    })

    output$radio7 <- shiny::renderUI({
      check_year_range(7)
      shiny::checkboxInput(ns("krav_7"), krav_tabell[7, ]$criteria, width = "100%")
    })

    output$radio8 <- shiny::renderUI({
      check_year_range(8)
      shiny::checkboxInput(ns("krav_8"), krav_tabell[8, ]$criteria, width = "100%")
    })

    output$radio9 <- shiny::renderUI({
      check_year_range(9)
      shiny::checkboxInput(ns("krav_9"), krav_tabell[9, ]$criteria, width = "100%")
    })

    output$radio10 <- shiny::renderUI({
      check_year_range(10)
      shiny::checkboxInput(ns("krav_10"), krav_tabell[10, ]$criteria, width = "100%")
    })

    output$radio11 <- shiny::renderUI({
      check_year_range(11)
      shiny::checkboxInput(ns("krav_11"), krav_tabell[11, ]$criteria, width = "100%")
    })

    # Stadium 4
    output$radio12 <- shiny::renderUI({
      check_year_range(12)
      shiny::checkboxInput(ns("krav_12"), krav_tabell[12, ]$criteria, width = "100%")
    })

    output$radio13 <- shiny::renderUI({
      check_year_range(13)
      shiny::checkboxInput(ns("krav_13"), krav_tabell[13, ]$criteria, width = "100%")
    })

    output$radio14 <- shiny::renderUI({
      check_year_range(14)
      shiny::checkboxInput(ns("krav_14"), krav_tabell[14, ]$criteria, width = "100%")
    })

    output$radio15 <- shiny::renderUI({
      check_year_range(15)
      shiny::checkboxInput(ns("krav_15"), krav_tabell[15, ]$criteria, width = "100%")
    })

    output$radio16 <- shiny::renderUI({
      check_year_range(16)
      shiny::checkboxInput(ns("krav_16"), krav_tabell[16, ]$criteria, width = "100%")
    })

    # Nivåer
    output$radio17 <- shiny::renderUI({
      check_year_range(17)
      shiny::checkboxInput(ns("krav_17"), krav_tabell[17, ]$criteria, width = "100%")
    })

    output$radio18 <- shiny::renderUI({
      check_year_range(18)
      shiny::checkboxInput(ns("krav_18"), krav_tabell[18, ]$criteria, width = "100%")
    })

    ##############################
    ##### Reaktivitet skjema #####
    ##############################

    # Stadium 2
    shiny::observeEvent(input$krav_1, {
      rv$vurdering[1] <- input$krav_1
      rv$table_data$krav_01 <- as.numeric(input$krav_1)
    })

    shiny::observeEvent(input$krav_2, {
      rv$vurdering[2] <- input$krav_2
      rv$table_data$krav_02 <- as.numeric(input$krav_2)
    })

    shiny::observeEvent(input$krav_3, {
      rv$vurdering[3] <- input$krav_3
      rv$table_data$krav_03 <- as.numeric(input$krav_3)
    })

    shiny::observeEvent(input$krav_4, {
      rv$vurdering[4] <- input$krav_4
      rv$table_data$krav_04 <- as.numeric(input$krav_4)
    })

    shiny::observeEvent(input$krav_5, {
      rv$vurdering[5] <- input$krav_5
      rv$table_data$krav_05 <- as.numeric(input$krav_5)
    })

    # Stadium 3
    shiny::observeEvent(input$krav_6, {
      rv$vurdering[6] <- input$krav_6
      rv$table_data$krav_06 <- as.numeric(input$krav_6)
    })

    shiny::observeEvent(input$krav_7, {
      rv$vurdering[7] <- input$krav_7
      rv$table_data$krav_07 <- as.numeric(input$krav_7)
    })

    shiny::observeEvent(input$krav_8, {
      rv$vurdering[8] <- input$krav_8
      rv$table_data$krav_08 <- as.numeric(input$krav_8)
    })

    shiny::observeEvent(input$krav_9, {
      rv$vurdering[9] <- input$krav_9
      rv$table_data$krav_09 <- as.numeric(input$krav_9)
    })

    shiny::observeEvent(input$krav_10, {
      rv$vurdering[10] <- input$krav_10
      rv$table_data$krav_10 <- as.numeric(input$krav_10)
    })

    shiny::observeEvent(input$krav_11, {
      rv$vurdering[11] <- input$krav_11
      rv$table_data$krav_11 <- as.numeric(input$krav_11)
    })

    # Stadium 4
    shiny::observeEvent(input$krav_12, {
      rv$vurdering[12] <- input$krav_12
      rv$table_data$krav_12 <- as.numeric(input$krav_12)
    })

    shiny::observeEvent(input$krav_13, {
      rv$vurdering[13] <- input$krav_13
      rv$table_data$krav_13 <- as.numeric(input$krav_13)
    })

    shiny::observeEvent(input$krav_14, {
      rv$vurdering[14] <- input$krav_14
      rv$table_data$krav_14 <- as.numeric(input$krav_14)
    })

    shiny::observeEvent(input$krav_15, {
      rv$vurdering[15] <- input$krav_15
      rv$table_data$krav_15 <- as.numeric(input$krav_15)
    })

    shiny::observeEvent(input$krav_16, {
      rv$vurdering[16] <- input$krav_16
      rv$table_data$krav_16 <- as.numeric(input$krav_16)
    })

    # Nivå A, B og C
    shiny::observeEvent(input$krav_17, {
      rv$vurdering[17] <- input$krav_17
      rv$table_data$krav_17 <- as.numeric(input$krav_17)
    })

    shiny::observeEvent(input$krav_18, {
      rv$vurdering[18] <- input$krav_18
      rv$table_data$krav_18 <- as.numeric(input$krav_18)
    })

    # Oppdater skjema ved valg av år og register
    shiny::observeEvent(update_form(), {

      dat <- pool::dbGetQuery(pool, "SELECT * FROM vurdering")
      dat <- dat[(dat$year == input$selected_year) & (dat$registry_id == input$selected_registry), ]

      if (nrow(dat) == 1) {
        shiny::updateCheckboxInput(session, "krav_1", value = dat$krav_01[1])
        shiny::updateCheckboxInput(session, "krav_2", value = dat$krav_02[1])
        shiny::updateCheckboxInput(session, "krav_3", value = dat$krav_03[1])
        shiny::updateCheckboxInput(session, "krav_4", value = dat$krav_04[1])
        shiny::updateCheckboxInput(session, "krav_5", value = dat$krav_05[1])
        shiny::updateCheckboxInput(session, "krav_6", value = dat$krav_06[1])
        shiny::updateCheckboxInput(session, "krav_7", value = dat$krav_07[1])
        shiny::updateCheckboxInput(session, "krav_8", value = dat$krav_08[1])
        shiny::updateCheckboxInput(session, "krav_9", value = dat$krav_09[1])
        shiny::updateCheckboxInput(session, "krav_10", value = dat$krav_10[1])
        shiny::updateCheckboxInput(session, "krav_11", value = dat$krav_11[1])
        shiny::updateCheckboxInput(session, "krav_12", value = dat$krav_12[1])
        shiny::updateCheckboxInput(session, "krav_13", value = dat$krav_13[1])
        shiny::updateCheckboxInput(session, "krav_14", value = dat$krav_14[1])
        shiny::updateCheckboxInput(session, "krav_15", value = dat$krav_15[1])
        shiny::updateCheckboxInput(session, "krav_16", value = dat$krav_16[1])
        shiny::updateCheckboxInput(session, "krav_17", value = dat$krav_17[1])
        shiny::updateCheckboxInput(session, "krav_18", value = dat$krav_18[1])

        shiny::updateTextInput(session, "N_A_fritekst", value = dat$fritekst_A)
        shiny::updateTextInput(session, "N_B_fritekst", value = dat$fritekst_B)

        shiny::updateNumericInput(session, "oppgitt_dg", value = dat$oppgitt_dg)

      } else {
        shiny::updateCheckboxInput(session, "krav_1", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_2", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_3", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_4", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_5", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_6", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_7", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_8", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_9", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_10", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_11", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_12", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_13", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_14", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_15", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_16", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_17", value = FALSE)
        shiny::updateCheckboxInput(session, "krav_18", value = FALSE)

        shiny::updateTextInput(session, "N_A_fritekst", value = dat$fritekst_A)
        shiny::updateTextInput(session, "N_send_innB_fritekst", value = dat$fritekst_B)

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
