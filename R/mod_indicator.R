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
        shiny::uiOutput(ns("select_indicator")),
        shiny::uiOutput(ns("add_new_indicator")),
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
      shiny::mainPanel(
        shiny::uiOutput(ns("edit_ind_title")),
        shiny::uiOutput(ns("title_oversize")),
        shiny::uiOutput(ns("edit_ind_short")),
        shiny::uiOutput(ns("short_oversize")),
        shiny::uiOutput(ns("edit_ind_long")),
        shiny::uiOutput(ns("long_oversize")),
        shiny::uiOutput(ns("update_indicator_txt"))
      )
    )
  )
}

#' Replace "Ingen" with NA in a string
#'
#' This function is intended for use in indicator_server.
#' We would like the select_dg_id input to return NA,
#' but shiny::selectInput turns NAs into strings.
#' The input should therefore go through this function.
#'
#' @param s string
check_no_dg <- function(s) {
  dplyr::case_when(s == "Ingen" ~ NA, .default = s)
}

#' Check that the accomplishment thresholds are conistent
#'
#' This function is intended for use in indicator_server.
#' Threshold are set for high accomplishment (green)
#' and medium accomplihment (yellow). The green threshold
#' should be higher than the yellow. If not, and error message
#' is displayed.
#'
#' @param input Shiny input object
#' @param conf get_config() output
levels_consistent_check <- function(input, conf) {
  if (!is.na(input$level_green) && !is.na(input$level_yellow)) {
    if (input$level_direction) {
      if (input$level_green >= input$level_yellow) {
        shinyjs::html("message", "")
        return(TRUE)
      } else {
        shinyjs::html("message", "")
        shinyjs::html(
          "message",
          conf$indicator$level_inconsistent_message
        )
        return(FALSE)
      }
    } else {
      if (input$level_yellow >= input$level_green) {
        shinyjs::html("message", "")
        return(TRUE)
      } else {
        shinyjs::html("message", "")
        shinyjs::html(
          "message",
          conf$indicator$level_inconsistent_message
        )
        return(FALSE)
      }
    }
  } else {
    shinyjs::html("message", "")
    return(TRUE)
  }
}

#' Display an oversize warning if there are too many characters in the input text
#'
#' This function is intended for use in indicator_server.
#' If the input text for indicator title, short description
#' or long description is too long according to the specification
#' in the config file, then an error message is displayed.
#'
#' @param oversize Logical
#' @param conf get_config() output
oversize_check <- function(oversize, conf) {
  if (oversize) {
    shiny::HTML(conf$indicator$oversize_message)
  } else {
    NULL
  }
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

    shiny::observeEvent(input$new_ind_name, {
      print(nchar(input$new_ind_name))
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

    shiny::observeEvent(rv$new_ind_name, {
      query <- paste0("INSERT INTO ind (id, registry_id) VALUES ( '",
                      rv$new_ind_name, "', '", input$indicator_registry, "');")

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
      shiny::req(input$indicator_registry, rv$ind_data, !grepl("dg_", rv$ind_data$type))

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

    update_check <- function() {
      if (any(c(
        is.null(input$indicator),
        is.null(input$include),
        is.null(input$level_direction),
        nrow(rv$ind_data) == 0
      ))) {
        NULL
      } else {
        no_new_values <- c(
          identical(input$include, as.logical(rv$ind_data$include)),
          identical(check_no_dg(input$dg_id), rv$ind_data$dg_id),
          identical(
            input$level_direction,
            as.logical(rv$ind_data$level_direction)
          ),
          identical(
            as.numeric(input$level_green),
            as.numeric(rv$ind_data$level_green)
          ),
          identical(
            as.numeric(input$level_yellow),
            as.numeric(rv$ind_data$level_yellow)
          ),
          identical(
            as.numeric(input$min_denominator),
            as.numeric(rv$ind_data$min_denominator)
          ),
          identical(input$type, rv$ind_data$type),
          identical(input$format, rv$sformat$format),
          identical(
            as.numeric(input$digits),
            as.numeric(rv$sformat$digits)
          )
        )
        if (all(no_new_values)) {
          return(NULL)
        } else {
          if (level_consistent()) {
            return(
              shiny::actionButton(
                ns("update_val"),
                "Oppdat\u00e9r verdier",
                style = conf$profile$action_button_style
              )
            )
          } else {
            return(NULL)
          }
        }
      }
    }

    output$update_indicator_val <- shiny::renderUI({
      update_check()
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

    update_indicator_txt_check <- function() {
      if (any(c(rv$title_oversize, rv$short_oversize, rv$long_oversize))) {
        NULL
      } else {
        no_new_text <- c(
          identical(input$ind_short, rv$ind_data$short_description),
          identical(input$ind_title, rv$ind_data$title),
          identical(input$ind_long, rv$ind_data$long_description)
        )
        if (all(no_new_text)) {
          return(NULL)
        } else if (nrow(rv$ind_data != 0)) {
          shiny::actionButton(
            ns("update_txt"),
            "Oppdat\u00e9r tekster",
            style = conf$profile$action_button_style
          )
        }
      }
    }

    output$update_indicator_txt <- shiny::renderUI({
      update_indicator_txt_check()
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
