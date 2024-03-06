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
        shiny::uiOutput(ns("checkbox1")),
        shiny::uiOutput(ns("checkbox2")),
        shiny::uiOutput(ns("checkbox3")),
        shiny::uiOutput(ns("checkbox4")),
        shiny::uiOutput(ns("checkbox5")),
        shiny::h3("Stadium 3"),
        shiny::uiOutput(ns("checkbox6")),
        shiny::uiOutput(ns("checkbox7")),
        shiny::uiOutput(ns("checkbox8")),
        shiny::uiOutput(ns("checkbox9")),
        shiny::uiOutput(ns("checkbox10")),
        shiny::uiOutput(ns("checkbox11")),
        shiny::h3("Stadium 4"),
        shiny::uiOutput(ns("checkbox12")),
        shiny::uiOutput(ns("checkbox13")),
        shiny::uiOutput(ns("checkbox14")),
        shiny::uiOutput(ns("checkbox15")),
        shiny::uiOutput(ns("checkbox16")),
        shiny::h3("Niv\u00e5 A"),
        shiny::uiOutput(ns("checkbox17")),
        shiny::uiOutput(ns("N_A_kommentar")),
        shiny::h3("Niv\u00e5 B"),
        shiny::uiOutput(ns("checkbox18")),
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

    n_krav <- 18

    rv <- shiny::reactiveValues(
      stadium = NA,
      level = NA,
      vurdering = (rep(FALSE, n_krav)),
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

    # Regn ut stadium og nivå
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

    lapply(X = 1:n_krav, FUN = function(i) {
      output[[paste0("checkbox", i)]] <- shiny::renderUI({

        shiny::req((as.numeric(input$selected_year) %>%
                      dplyr::between(krav_tabell$introduction_year[i], krav_tabell$last_year[i])))

        shiny::tags$div(
          title = paste0(krav_tabell$guide[i], "\n\n", krav_tabell$section[i]),
          shiny::checkboxInput(ns(paste0("krav_", i)), shiny::HTML(krav_tabell$criteria[i]), width = "100%")
        )
      })
    }
    )

    ##############################
    ##### Reaktivitet skjema #####
    ##############################

    lapply(X = 1:n_krav, FUN = function(i) {

      col_name <- paste0("krav_", i)

      shiny::observeEvent(input[[col_name]], {
        rv$vurdering[i] <- input[[col_name]]
        rv$table_data[[col_name]] <- as.numeric(input[[col_name]])
      })
    }
    )

    # Oppdater skjema ved valg av år og register
    shiny::observeEvent(update_form(), {

      dat <- pool::dbGetQuery(pool, "SELECT * FROM vurdering")
      dat <- dat[(dat$year == input$selected_year) & (dat$registry_id == input$selected_registry), ]

      if (nrow(dat) == 1) {
        lapply(X = 1:n_krav, FUN = function(i) {
          col_name <- paste0("krav_", i)
          shiny::updateCheckboxInput(session, col_name, value = dat[[col_name]][1])
        })

        shiny::updateTextInput(session, "N_A_fritekst", value = dat$fritekst_A)
        shiny::updateTextInput(session, "N_B_fritekst", value = dat$fritekst_B)

        shiny::updateNumericInput(session, "oppgitt_dg", value = dat$oppgitt_dg)

      } else {
        lapply(X = 1:n_krav, FUN = function(i) {
          shiny::updateCheckboxInput(session, paste0("krav_", i), value = FALSE)
        })

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
