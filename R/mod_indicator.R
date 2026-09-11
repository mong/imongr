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

#' @rdname mod_indicator
#' @export
indicator_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_indicator_registry")),
        shiny::tabsetPanel(
          id = ns("indicator_tabs"),
          selected = "Indikator",
          shiny::tabPanel(
            value = "Indikator",
            title = "Lag eller endre indikator",
            shiny::br(),
            shiny::uiOutput(ns("select_indicator")),
            shiny::fluidRow(
              shiny::column(4, shiny::uiOutput(ns("add_new_indicator"))),
              shiny::column(4, shiny::uiOutput(ns("remove_indicator_data_btn"))),
              shiny::column(4, shiny::uiOutput(ns("remove_indicator_completely_btn")))
            ),
            shiny::hr(),
            shiny::uiOutput(ns("select_dg_id")),
            shiny::uiOutput(ns("set_include")),
            shiny::uiOutput(ns("set_level_direction")),
            shiny::uiOutput(ns("set_level_green")),
            shiny::uiOutput(ns("set_level_yellow")),
            shiny::uiOutput(ns("set_min_denominator")),
            shiny::uiOutput(ns("set_type")),
            shiny::uiOutput(ns("set_format")),
            shiny::uiOutput(ns("set_digits")),
            shiny::uiOutput(ns("update_indicator_val")),
            shiny::uiOutput(ns("message"))
          ),
          shiny::tabPanel(
            value = "Sortering",
            title = "Sortere indikatorer",
            shiny::br(),
            shiny::uiOutput(ns("sorting_info"))
          )
        )
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("indicator_text_editor"))
      )
    )
  )
}

#' @rdname mod_indicator
#' @export
indicator_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shinyjs::useShinyjs()
    conf <- get_config()

    validateIndName <- function(x) {
      existing_ind_ids <- pool::dbGetQuery(pool_verify, "SELECT id FROM ind")$id

      return(validateName(x, existing_ind_ids))
    }

    inputValidator <- shinyvalidate::InputValidator$new(session = session)
    inputValidator$add_rule("new_ind_name", validateIndName)
    inputValidator$enable()

    rv <- shiny::reactiveValues(
      level_logi = "st\u00f8rre eller lik:",
      level_green_min = 0,
      level_green_max = 1,
      level_consistent = TRUE,
      title_oversize = FALSE,
      short_oversize = FALSE,
      long_oversize = FALSE,
      new_ind_counter = 0
    )

    rv_return <- shiny::reactiveValues()

    nordic_languages <- c(
      no = "Norsk",
      dk = "Dansk",
      se = "Svensk",
      fi = "Finsk",
      is = "Islandsk",
      en = "Engelsk"
    )
    required_nordic_languages <- names(nordic_languages)

    nordic_existing_text <- shiny::reactive({
      shiny::req(input$indicator)
      get_nordic_ind_text(pool_verify, input$indicator)
    })

    rv_nordic <- shiny::reactive({
      data.frame(
        ind_id = input$indicator,
        language = names(nordic_languages),
        title = vapply(names(nordic_languages), function(language_code) {
          input[[paste0("ind_title_", language_code)]] %||% ""
        }, character(1)),
        description = vapply(names(nordic_languages), function(language_code) {
          input[[paste0("ind_long_", language_code)]] %||% ""
        }, character(1)),
        row.names = NULL
      )
    })

    nordic_saved <- shiny::reactiveVal(NULL)

    shiny::observeEvent(nordic_existing_text(), {
      nordic_saved(nordic_existing_text())
    })

    nordic_changed <- shiny::reactive({
      saved <- nordic_saved()
      current <- rv_nordic()
      !identical(
        paste(current$title, current$description),
        vapply(current$language, function(language_code) {
          saved_row <- saved[saved$language == language_code, ]
          if (nrow(saved_row) == 0) " " else paste(saved_row$title, saved_row$description)
        }, character(1), USE.NAMES = FALSE)
      )
    })

    nordic_text_editor <- function(language_code) {
      existing_text <- nordic_existing_text()
      existing_row <- existing_text[existing_text$language == language_code, ]
      title <- if (nrow(existing_row)) existing_row$title[[1]] else ""
      description <- if (nrow(existing_row)) existing_row$description[[1]] else ""

      shiny::tagList(
        shiny::textAreaInput(
          ns(paste0("ind_title_", language_code)),
          "Indikatortittel (maks 255 tegn)",
          value = title,
          width = "90%", rows = 2
        ),
        shiny::uiOutput(ns(paste0("title_oversize_", language_code))),
        shiny::textAreaInput(
          ns(paste0("ind_long_", language_code)),
          "Indikatorbeskrivelse (maks 2047 tegn)",
          value = description,
          width = "90%", rows = 16
        ),
        shiny::uiOutput(ns(paste0("long_oversize_", language_code)))
      )
    }

    for (language_code in names(nordic_languages)) {
      local({
        code <- language_code
        output[[paste0("title_oversize_", code)]] <- shiny::renderUI({
          oversize_check(isTRUE(nchar(input[[paste0("ind_title_", code)]]) > 255), conf)
        })
        output[[paste0("long_oversize_", code)]] <- shiny::renderUI({
          oversize_check(isTRUE(nchar(input[[paste0("ind_long_", code)]]) > 2047), conf)
        })
      })
    }

    level_limits <- shiny::reactive({
      if (nrow(rv$ind_data) != 0) {
        if (rv$ind_data$level_direction == 1) {
          rv$level_green_min <- rv$ind_data$level_yellow
          rv$level_green_max <- 1
          rv$level_yellow_min <- 0
          rv$level_yellow_max <- rv$ind_data$level_yellow
        } else {
          rv$level_green_min <- 0
          rv$level_green_max <- rv$ind_data$level_yellow
          rv$level_yellow_min <- rv$ind_data$level_yellow
          rv$level_yellow_max <- 1
        }
      }
    })

    level_consistent <- shiny::reactive({
      levels_consistent_check(input, conf)
    })

    shiny::observeEvent(input$indicator_registry, {
      rv_return$registry_id <- input$indicator_registry
    })
    output$indicator_text_editor <- shiny::renderUI({
      shiny::req(input$indicator_registry)
      nordic_state <- get_nordic_state(pool_verify, input$indicator_registry)

      if (isTRUE(nordic_state == 0)) {
        shiny::tagList(
          shiny::uiOutput(ns("edit_ind_title")),
          shiny::uiOutput(ns("title_oversize")),
          shiny::uiOutput(ns("edit_ind_short")),
          shiny::uiOutput(ns("short_oversize")),
          shiny::uiOutput(ns("edit_ind_long")),
          shiny::uiOutput(ns("long_oversize")),
          shiny::uiOutput(ns("update_indicator_txt"))
        )
      } else if (isTRUE(nordic_state == 1)) {
        shiny::tagList(
          do.call(shiny::tabsetPanel, lapply(names(nordic_languages), function(language_code) {
            shiny::tabPanel(
              nordic_languages[[language_code]],
              nordic_text_editor(language_code)
            )
          })),
          shiny::uiOutput(ns("update_nordic_button"))
        )
      }
    })
    shiny::observeEvent(input$indicator, {
      rv$ind_data <- get_registry_ind(pool_verify, input$indicator_registry)
      rv$ind_data <- rv$ind_data |>
        dplyr::filter(.data$id == input$indicator) |>
        dplyr::mutate(
          title = dplyr::case_when(
            is.na(title) ~ "",
            TRUE ~ title
          ),
          short_description = dplyr::case_when(
            is.na(short_description) ~ "",
            TRUE ~ short_description
          ),
          long_description = dplyr::case_when(
            is.na(long_description) ~ "",
            TRUE ~ long_description
          )
        )
      level_limits()
    })

    shiny::observeEvent(rv$ind_data, {
      rv$sformat <- rv$ind_data |>
        dplyr::mutate(
          format = substr(.data$sformat, nchar(.data$sformat), nchar(.data$sformat)),
          digits = substr(.data$sformat, 3, nchar(.data$sformat) - 1)
        ) |>
        dplyr::select("format", "digits")
    })

    shiny::observeEvent(input$level_direction, {
      rv$level_logi <- ifelse(input$level_direction, "st\u00f8rre eller lik:", "mindre eller lik:")
      level_consistent()
    })

    shiny::observeEvent(input$update_val, {
      rv$ind_data$include <- input$include
      rv$ind_data$dg_id <- check_no_dg(input$dg_id)
      rv$ind_data$level_direction <- input$level_direction
      rv$ind_data$level_green <- input$level_green
      rv$ind_data$level_yellow <- input$level_yellow
      rv$ind_data$min_denominator <- input$min_denominator
      rv$ind_data$type <- input$type
      rv$ind_data$sformat <- paste0(",.", input$digits, input$format)
      update_ind_val(pool_verify, rv$ind_data)
      rv$ind_data <- get_registry_ind(pool_verify, input$indicator_registry)
      rv$ind_data <- rv$ind_data |>
        dplyr::filter(.data$id == input$indicator)
    })

    shiny::observeEvent(input$ind_title, {
      rv$title_oversize <- ifelse(nchar(input$ind_title) > 255, TRUE, FALSE)
    })

    shiny::observeEvent(input$ind_short, {
      rv$short_oversize <- ifelse(nchar(input$ind_short) > 1023, TRUE, FALSE)
    })

    shiny::observeEvent(input$ind_long, {
      rv$long_oversize <- ifelse(nchar(input$ind_long) > 2047, TRUE, FALSE)
    })

    shiny::observeEvent(input$update_txt, {
      rv$ind_data$title <- input$ind_title
      rv$ind_data$short_description <- input$ind_short
      rv$ind_data$long_description <- input$ind_long
      update_ind_text(pool_verify, rv$ind_data)
      rv$ind_data <- get_registry_ind(pool_verify, input$indicator_registry) |>
        dplyr::filter(.data$id == input$indicator)
    })

    shiny::observeEvent(input$update_nordic_txt, {
      shiny::req(nordic_changed())
      update_nordic_ind_text(pool_verify, rv_nordic())
      update_nordic_ind_text(pool, rv_nordic())
      nordic_saved(rv_nordic())
    })

    shiny::observeEvent(input$new_indicator, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$h3("Velg navn p\u00e5 ny indikator"),
          shiny::textInput(ns("new_ind_name"), "Indikatornavn"),
          shiny::selectInput(
            ns("new_ind_type"), "Indikatortype:",
            choices = conf$indicator$types, selected = rv$ind_data$type
          ),
          footer = shiny::tagList(
            shiny::actionButton(ns("new_ind_submit"), "OK"),
            shiny::modalButton("Avbryt")
          )
        )
      )
      shinyjs::disable("new_ind_submit")
    })

    shiny::observeEvent(input$remove_indicator_completely, {
      is_in_project <- pool::dbGetQuery(
        pool_verify,
        paste0(
          "SELECT EXISTS (SELECT 1 FROM project_ind WHERE ind_id = '",
          input$indicator, "') AS exists_flag;"
        )
      )$exists_flag
      if (is_in_project) {
        shiny::showModal(
          shiny::modalDialog(
            shiny::tags$h3("Denne indikatoren er i bruk i ett eller flere prosjekter og kan ikke fjernes."),
            footer = shiny::modalButton("Lukk")
          )
        )
      } else {
        shiny::showModal(
          shiny::modalDialog(
            shiny::tags$h3("Er du sikker p\u00e5 at du vil fjerne denne indikatoren?"),
            footer = shiny::tagList(
              shiny::actionButton(ns("remove_ind_submit"), "OK"),
              shiny::modalButton("Avbryt")
            )
          )
        )
      }
    })

    shiny::observeEvent(input$new_ind_name, {
      if (nchar(input$new_ind_name) > 0) {
        if (is.null(validateIndName(input$new_ind_name))) {
          shinyjs::enable("new_ind_submit")
        } else {
          shinyjs::disable("new_ind_submit")
        }
      }
    })

    shiny::observeEvent(input$new_ind_submit, {
      shiny::removeModal()
      rv$new_ind_name <- input$new_ind_name
    })

    shiny::observeEvent(input$remove_ind_submit, {
      shiny::removeModal()

      remove_agg_query <- paste0("DELETE FROM agg_data WHERE ind_id = '", input$indicator, "';")
      remove_ind_query <- paste0("DELETE FROM ind WHERE id = '", input$indicator, "';")

      pool::dbExecute(pool, remove_agg_query)
      pool::dbExecute(pool_verify, remove_agg_query)
      pool::dbExecute(pool, remove_ind_query)
      pool::dbExecute(pool_verify, remove_ind_query)

      rv$new_ind_counter <- rv$new_ind_counter - 1
    })

    shiny::observeEvent(input$remove_indicator_data, {
      is_in_project <- pool::dbGetQuery(
        pool_verify, paste0(
          "SELECT EXISTS (SELECT 1 FROM project_ind WHERE ind_id = '", input$indicator, "') AS exists_flag;"
        )
      )$exists_flag
      if (is_in_project) {
        shiny::showModal(
          shiny::modalDialog(
            shiny::tags$h3("Denne indikatoren er i bruk i ett eller flere prosjekter og kan ikke fjernes."),
            footer = shiny::modalButton("Lukk")
          )
        )
      } else {
        years_with_data <- pool::dbGetQuery(
          pool_verify, paste0(
            "SELECT DISTINCT year FROM data WHERE ind_id = '", input$indicator, "';"
          )
        )$year |>
          sort(decreasing = FALSE)
        shiny::showModal(
          shiny::modalDialog(
            shiny::fluidRow(
              shiny::column(6, shiny::radioButtons(ns("remove_year_type"), "Type:",
                choices = c("Til og med", "Enkeltår")
              )),
              shiny::column(
                6,
                shiny::selectInput(ns("remove_year"), "Velg \u00e5r for data som skal fjernes:",
                  choices = years_with_data
                )
              )
            ),
            shiny::selectInput(ns("remove_context"), "Velg kontekst som skal fjernes:",
              choices = c("Alle", "caregiver", "resident")
            ),
            footer = shiny::tagList(
              shiny::actionButton(ns("remove_ind_data_submit"), "Fjern data"),
              shiny::modalButton("Avbryt")
            )
          )
        )
      }
    })

    shiny::observeEvent(input$remove_ind_data_submit, {
      if (input$remove_year_type == "Til og med") {
        year_condition <- paste0("year <= ", input$remove_year)
      } else {
        year_condition <- paste0("year = ", input$remove_year)
      }
      if (input$remove_context == "Alle") {
        context_condition <- ""
      } else {
        context_condition <- paste0("AND context = '", input$remove_context, "'")
      }
      shiny::removeModal()

      remove_agg_query <- paste0(
        "DELETE FROM data WHERE ind_id = '",
        input$indicator, "' AND ", year_condition, " ", context_condition, ";"
      )
      remove_data_query <- paste0(
        "DELETE FROM agg_data WHERE ind_id = '", input$indicator,
        "' AND ", year_condition, " ", context_condition, ";"
      )

      pool::dbExecute(pool, remove_agg_query)
      pool::dbExecute(pool_verify, remove_agg_query)
      pool::dbExecute(pool, remove_data_query)
      pool::dbExecute(pool_verify, remove_data_query)
      rv$new_ind_counter <- rv$new_ind_counter - 1
    })

    shiny::observeEvent(rv$new_ind_name, {
      query <- paste0(
        "INSERT INTO ind (id, registry_id) VALUES ( '",
        rv$new_ind_name, "', '", input$indicator_registry, "');"
      )

      new_ind_data <- data.frame(
        id = rv$new_ind_name,
        dg_id = NA,
        include = 0,
        title = "Indikatortittel",
        name = "a",
        type = input$new_ind_type,
        sformat = ifelse(grepl("andel", input$new_ind_type), ",.0%", ",.0f"),
        min_denominator = NA,
        level_green = NA,
        level_yellow = NA,
        level_direction = 1,
        short_description = "Kort indikatorbeskrivelse",
        long_description = "Lang indikatorbeskrivelse",
        registry_id = input$indicator_registry
      )

      pool::dbExecute(pool, query)
      pool::dbExecute(pool_verify, query)

      update_ind_val(pool, new_ind_data)
      update_ind_val(pool_verify, new_ind_data)

      update_ind_text(pool, new_ind_data)
      update_ind_text(pool_verify, new_ind_data)

      rv$new_ind_counter <- rv$new_ind_counter + 1
    })

    output$select_indicator_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("indicator_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    output$select_indicator <- shiny::renderUI({
      rv$new_ind_counter
      shiny::req(input$indicator_registry)
      shiny::selectInput(
        ns("indicator"), "Velg indikator:",
        choices = get_registry_indicators(pool_verify, input$indicator_registry)$id,
        selected = rv$new_ind_name
      )
    })

    output$select_dg_id <- shiny::renderUI({
      shiny::req(input$indicator_registry, rv$ind_data)

      shiny::selectInput(
        ns("dg_id"), "Tilh\u00f8rende dekningsgradsindikator:",
        choices = c("Ingen", get_dg_indicators(pool_verify, input$indicator_registry)$id),
        selected = check_no_dg(rv$ind_data$dg_id)
      )
    })

    output$add_new_indicator <- shiny::renderUI({
      shiny::req(input$indicator_registry)
      shiny::actionButton(ns("new_indicator"), "Lag helt ny indikator")
    })

    output$remove_indicator_completely_btn <- shiny::renderUI({
      shiny::req(input$indicator_registry)
      shiny::actionButton(ns("remove_indicator_completely"), "Fjern indikator med all tilh\u00f8rende data")
    })

    output$remove_indicator_data_btn <- shiny::renderUI({
      shiny::req(input$indicator_registry)
      shiny::actionButton(ns("remove_indicator_data"), "Fjern data for denne indikatoren")
    })

    output$set_include <- shiny::renderUI({
      shiny::req(input$indicator, rv$ind_data$include)
      shiny::tags$div(
        title = "Angi om indikatoren skal vises på apps.skde.no/behandlingskvalitet",
        bslib::input_switch(
          ns("include"), "Vis på Behandlingskvalitet",
          value = as.logical(rv$ind_data$include)
        )
      )
    })

    output$set_level_direction <- shiny::renderUI({
      shiny::req(input$indicator, rv$ind_data$level_direction)
      shiny::tags$div(
        title = paste(
          "Ta vekk haken hvis synkende indikatorverdier gir \u00f8kt",
          "m\u00e5loppn\u00e5else"
        ),
        shiny::checkboxInput(
          ns("level_direction"),
          "H\u00f8y verdi gir h\u00f8y m\u00e5loppn\u00e5else",
          value = as.logical(rv$ind_data$level_direction)
        )
      )
    })

    output$set_level_green <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::tags$div(
        title = "Grenseverdi for beste m\u00e5loppn\u00e5else",
        shiny::numericInput(
          ns("level_green"),
          paste("H\u00f8y m\u00e5loppn\u00e5else", rv$level_logi),
          value = rv$ind_data$level_green,
          min = rv$level_green_min,
          max = rv$level_green_max,
          step = 0.1
        )
      )
    })

    output$set_level_yellow <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::tags$div(
        title = "Grenseverdi for middels m\u00e5loppn\u00e5else",
        shiny::numericInput(
          ns("level_yellow"),
          paste("Middels m\u00e5loppn\u00e5else", rv$level_logi),
          value = rv$ind_data$level_yellow,
          min = rv$level_yellow_min,
          max = rv$level_yellow_max,
          step = 0.1
        )
      )
    })

    output$set_min_denominator <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::tags$div(
        title = paste(
          "Minste antall observasjoner (N) som kreves for at",
          "indikatoren presenteres"
        ),
        shiny::numericInput(
          ns("min_denominator"), "Minste antall observasjoner:",
          value = rv$ind_data$min_denominator, min = 0
        )
      )
    })

    output$set_type <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::tags$div(
        title = paste(
          "Normalt sett vil grad av m\u00e5loppn\u00e5else gis som antall",
          "hendelser som har oppn\u00e5dd m\u00e5let (var) delt p\u00e5",
          "totalt antall hendelser (denominator) og da skal indikatortype",
          "settes til 'andel' eller 'dg_andel'. Alternativt kan grad av",
          "m\u00e5loppn\u00e5else beregnes f\u00f8r opplasting til",
          "behandlingskvalitet/sykehusprofil, og da skal indikatortype",
          "settes til en av de andre kategoriene."
        ),
        shiny::selectInput(
          ns("type"), "Indikatortype:",
          choices = conf$indicator$types, selected = rv$ind_data$type
        )
      )
    })

    output$set_format <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::tags$div(
        title = paste(
          "Angir om indikatoren er oppgitt i prosent eller desimaltall"
        ),
        shiny::selectInput(
          ns("format"), "Indikatorformat:",
          choices = setNames(
            as.list(conf$indicator$formats), conf$indicator$format_labels
          ), selected = rv$sformat$format
        )
      )
    })

    output$set_digits <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::tags$div(
        title = paste(
          "Antall desimaler"
        ),
        shiny::numericInput(
          ns("digits"), "Antall desimaler:",
          value = rv$sformat$digits, min = 0
        )
      )
    })

    output$update_indicator_val <- shiny::renderUI({
      update_check(input, conf, ns, rv, level_consistent)
    })

    output$edit_ind_title <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::textAreaInput(
        ns("ind_title"), "Indikatortittel (maks 255 tegn)",
        value = rv$ind_data$title, width = "90%", rows = 2
      )
    })

    output$title_oversize <- shiny::renderUI({
      oversize_check(rv$title_oversize, conf)
    })

    output$edit_ind_short <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::textAreaInput(
        ns("ind_short"), "Kort indikatorbeskrivelse (maks 1023 tegn)",
        value = rv$ind_data$short_description, width = "90%", rows = 8
      )
    })

    output$short_oversize <- shiny::renderUI({
      oversize_check(rv$short_oversize, conf)
    })

    output$edit_ind_long <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::textAreaInput(
        ns("ind_long"), "Lang indikatorbeskrivelse (maks 2047 tegn)",
        value = rv$ind_data$long_description, width = "90%", rows = 16
      )
    })

    output$long_oversize <- shiny::renderUI({
      oversize_check(rv$long_oversize, conf)
    })

    output$update_indicator_txt <- shiny::renderUI({
      update_indicator_txt_check(input, conf, ns, rv)
    })

    output$update_nordic_button <- shiny::renderUI({
      nordic_df <- rv_nordic()
      required_text <- nordic_df[nordic_df$language %in% required_nordic_languages, ]
      missing_descriptions <- sum(!nzchar(trimws(required_text$description)))
      has_oversize_text <- any(vapply(required_nordic_languages, function(language_code) {
        nchar(input[[paste0("ind_title_", language_code)]] %||% "") > 255 ||
          nchar(input[[paste0("ind_long_", language_code)]] %||% "") > 2047
      }, logical(1)))

      if (!has_oversize_text) {
        missing_text <- if (missing_descriptions > 0) {
          shiny::tags$p(sprintf("%d språk mangler beskrivelse", missing_descriptions))
        }

        shiny::tagList(
          missing_text,
          shiny::actionButton(
            ns("update_nordic_txt"),
            "Oppdater tekster",
            style = if (nordic_changed()) {
              conf$profile$action_button_style
            } else {
              "background-color: #B9B9B9; border-color: #B9B9B9; color: white;"
            }
          )
        )
      }
    })


    output$sorting_info <- shiny::renderUI({
      shiny::req(input$indicator_registry)
      shiny::tagList(
        shiny::tags$p(
          "Flytt indikatorer mellom kolonnene for \u00e5 velge om de skal inkluderes eller ekskluderes."
        ),
        shiny::tags$p(
          "Kolonnen for inkluderte indikatorer bestemmer sorteringsrekkef\u00f8lgen."
        ),
        shiny::tags$p(
          "Husk \u00e5 lagre sorteringen n\u00e5r du er ferdig."
        )
      )
    })

    sorting_indicators <- shiny::reactive({
      shiny::req(input$indicator_registry)
      indicators <- get_registry_ind(pool_verify, input$indicator_registry)
      indicators <- indicators[!grepl("^dg", indicators$type), ]

      include_indicators <- indicators[!is.na(indicators$include) & indicators$include == 1, ]
      include_indicators <- include_indicators[
        order(is.na(include_indicators$name), include_indicators$name, include_indicators$id),
      ]

      exclude_indicators <- indicators[!is.na(indicators$include) & indicators$include == 0, ]
      exclude_indicators <- exclude_indicators[
        order(is.na(exclude_indicators$name), exclude_indicators$name, exclude_indicators$id),
      ]

      list(
        include = stats::setNames(
          include_indicators$title,
          include_indicators$id
        ),
        exclude = stats::setNames(
          exclude_indicators$title,
          exclude_indicators$id
        )
      )
    })

    output$indicator_main_panel <- shiny::renderUI({
      shiny::req(input$indicator_tabs)

      if (identical(input$indicator_tabs, "Sortering")) {
        shiny::tagList(
          shiny::tags$div(
            style = "display: flex; align-items: center; justify-content: space-between; gap: 12px;",
            shiny::tags$h3("Sorter og inkluder indikatorer", style = "margin: 0;"),
            shiny::actionButton(
              ns("sorting_instructions"),
              "Instruksjoner",
              icon = shiny::icon("circle-info"),
              class = "btn btn-outline-secondary"
            )
          ),
          shiny::br(),
          shiny::tags$p(
            "Dra indikatorer mellom kolonnene og sorter dem i riktig rekkef\u00f8lge.",
            style = "margin-bottom: 16px;"
          ),
          shiny::tags$p(
            "Kolonnen \"Skal vises p\u00e5 behandlingskvalitet\" bestemmer hvilke indikatorer
            som vises p\u00e5 apps.skde.no/behandlingskvalitet.",
            style = "margin-bottom: 16px;"
          ),
          sortable::bucket_list(
            header = NULL,
            group_name = ns("indicator_sorting"),
            sortable::add_rank_list(
              text = "Skal vises p\u00e5 behandlingskvalitet",
              labels = sorting_indicators()$include,
              input_id = ns("indicator_sorting_include")
            ),
            sortable::add_rank_list(
              text = "Vises IKKE p\u00e5 behandlingskvalitet",
              labels = sorting_indicators()$exclude,
              input_id = ns("indicator_sorting_exclude")
            )
          ),
          shiny::actionButton(ns("save_sorting"), "Lagre sortering")
        )
      } else {
        shiny::tagList(
          shiny::uiOutput(ns("edit_ind_title")),
          shiny::uiOutput(ns("title_oversize")),
          shiny::uiOutput(ns("edit_ind_short")),
          shiny::uiOutput(ns("short_oversize")),
          shiny::uiOutput(ns("edit_ind_long")),
          shiny::uiOutput(ns("long_oversize")),
          shiny::uiOutput(ns("update_indicator_txt"))
        )
      }
    })

    shiny::observeEvent(input$sorting_instructions, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Instruksjoner for sortering",
          shiny::tags$p(
            "Bruk denne visningen for å velge hvilke indikatorer som skal vises og i hvilken rekkefølge.",
            style = "margin-bottom: 12px;"
          ),
          shiny::tags$ol(
            shiny::tags$li("Dra indikatorer til \"Skal vises på behandlingskvalitet\" for å vise dem på nettsiden."),
            shiny::tags$li("La indikatorer som ikke skal vises ligge i \"Vises IKKE på behandlingskvalitet\"."),
            shiny::tags$li("Sorter rekkefølgen i \"Skal vises på behandlingskvalitet\" fra topp til bunn."),
            shiny::tags$li("Trykk Lagre sortering når du er ferdig."),
            style = "margin-bottom: 16px;"
          ),
          shiny::tags$p(
            "Husk: Rekkefølgen i \"Skal vises på behandlingskvalitet\" blir den rekkefølgen som brukes videre.",
            style = "margin-bottom: 12px;"
          ),
          shiny::img(
            src = "www/sortering_instruksjoner.gif",
            style = "max-width: 100%; height: auto; border-radius: 6px;"
          ),
          easyClose = TRUE,
          size = "xl",
          footer = shiny::modalButton("Lukk")
        )
      )
    })

    shiny::observeEvent(input$save_sorting, {
      shiny::req(input$indicator_sorting)

      sorting_state <- input$indicator_sorting
      if (length(sorting_state) < 2) {
        return()
      }

      include_ids <- sorting_state[[1]]
      exclude_ids <- sorting_state[[2]]

      escape_sql <- function(x) {
        gsub("'", "''", x, fixed = TRUE)
      }

      include_names <- letters[seq_along(include_ids)]

      all_ids <- c(include_ids, exclude_ids)

      if (length(all_ids) == 0) {
        shiny::showNotification("Ingen indikatorer å lagre.", type = "message")
        return()
      }

      update_include_query <- paste0(
        "UPDATE ind SET include = CASE id ",
        paste0(
          c(
            paste0("WHEN '", escape_sql(include_ids), "' THEN 1"),
            paste0("WHEN '", escape_sql(exclude_ids), "' THEN 0")
          ),
          collapse = " "
        ),
        " END WHERE id IN ('", paste(escape_sql(all_ids), collapse = "', '"), "');"
      )

      update_name_query <- paste0(
        "UPDATE ind SET name = CASE id ",
        paste0(
          "WHEN '", escape_sql(include_ids), "' THEN '", include_names, "'",
          collapse = " "
        ),
        " END WHERE id IN ('", paste(escape_sql(include_ids), collapse = "', '"), "');"
      )
      pool::dbExecute(pool_verify, update_include_query)
      pool::dbExecute(pool, update_include_query)
      pool::dbExecute(pool_verify, update_name_query)
      pool::dbExecute(pool, update_name_query)
      shiny::showNotification("Sortering lagret.", type = "message")
    })

    return(rv_return)
  })
}

#' @rdname mod_indicator
#' @export
indicator_app <- function(pool_verify) {
  ui <- shiny::fluidPage(
    indicator_ui("ind")
  )

  server <- function(input, output, sessjon) {
    indicator_server("ind", pool_verify)
  }

  shiny::shinyApp(ui, server)
}
