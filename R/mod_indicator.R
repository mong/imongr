#' Shiny module providing GUI and server logic for the indicator tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_indicator
#' @aliases indicator_input indicator_server indicator_app
NULL

#' @rdname mod_indicator
#' @export
indicator_input <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_indicator_registry")),
        shiny::uiOutput(ns("select_indicator")),
        shiny::hr(),
        shiny::uiOutput(ns("set_include")),
        shiny::uiOutput(ns("set_level_direction")),
        shiny::uiOutput(ns("set_level_green")),
        shiny::uiOutput(ns("set_level_yellow")),
        shiny::uiOutput(ns("set_min_denominator")),
        shiny::uiOutput(ns("set_type")),
        shiny::uiOutput(ns("update_indicator_val"))
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


#' @rdname mod_indicator
#' @export
indicator_server <- function(id, pool) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    conf <- get_config()

    rv <- shiny::reactiveValues(
      indicator_reg = character(),
      level_logi = "st\u00f8rre eller lik:",
      type = c("andel"),
      title_oversize = FALSE,
      short_oversize = FALSE,
      long_oversize = FALSE
    )

    oversize_message <- "<i style='color:red;'>Teksten er for lang!</i><br><br>"

    shiny::observeEvent(input$level_direction, {
      if (input$level_direction) {
        rv$level_logi <- "st\u00f8rre eller lik:"
      } else {
        rv$level_logi <- "mindre eller lik:"
      }
    })
    shiny::observeEvent(input$min_denominator, {
      rv$val_change <- TRUE
    })

    shiny::observeEvent(input$indicator, {
      rv$ind_data <- get_registry_ind(pool, input$indicator_registry) %>%
        dplyr::filter(.data$id == input$indicator)
      rv$val_change <- FALSE
    })

    shiny::observeEvent(input$ind_title, {
      if (nchar(input$ind_title) > 255) {
        rv$title_oversize <- TRUE
      } else {
        rv$title_oversize <- FALSE
      }
    })

    shiny::observeEvent(input$ind_short, {
      if (nchar(input$ind_short) > 1023) {
        rv$short_oversize <- TRUE
      } else {
        rv$short_oversize <- FALSE
      }
    })

    shiny::observeEvent(input$ind_long, {
      if (nchar(input$ind_long) > 2047) {
        rv$long_oversize <- TRUE
      } else {
        rv$long_oversize <- FALSE
      }
    })

    shiny::observeEvent(input$update_txt, {
      rv$ind_data$title <- input$ind_title
      rv$ind_data$short_description <- input$ind_short
      rv$ind_data$long_description <- input$ind_long
      update_ind_text(pool, rv$ind_data)
      rv$ind_data <- get_registry_ind(pool, input$indicator_registry) %>%
        dplyr::filter(.data$id == input$indicator)
    })

    output$select_indicator_registry <- shiny::renderUI({
      select_registry_ui(pool, conf, input_id = ns("indicator_registry"),
                         context = "verify", current_reg = rv$indicator_reg,
                         show_context = FALSE)
    })

    output$select_indicator <- shiny::renderUI({
      shiny::req(input$indicator_registry)
      shiny::selectInput(
        ns("indicator"), "Velg indikator:",
        choices = get_registry_indicators(pool, input$indicator_registry)
      )
    })

    output$set_include <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::checkboxInput(
        ns("include"), "Vis i Sykehusviseren",
        value = as.logical(rv$ind_data$include)
      )
    })

    output$set_level_direction <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::checkboxInput(
        ns("level_direction"),
        "H\u00f8y verdi gir h\u00f8y m\u00e5loppn\u00e5else",
        value = as.logical(rv$ind_data$level_direction)
      )
    })

    output$set_level_green <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::numericInput(
        ns("level_green"),
        paste("H\u00f8y m\u00e5loppn\u00e5else", rv$level_logi),
        value = rv$ind_data$level_green,
        min = 0,
        max = 1
      )
    })

    output$set_level_yellow <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::numericInput(
        ns("level_yellow"),
        paste("Middels m\u00e5loppn\u00e5else", rv$level_logi),
        value = rv$ind_data$level_yellow,
        min = 0,
        max = 1
      )
    })

    output$set_min_denominator <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::numericInput(
        ns("min_denominator"), "Minste antall observasjoner:",
        value = rv$ind_data$min_denominator, min = 0
      )
    })

    output$set_type <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::selectInput(
        ns("type"), "Indikatortype:", choices = unique(rv$ind_data$type)
      )
    })

    output$update_indicator_val <- shiny::renderUI({
      shiny::req(
        input$min_denominator
      )
      if (identical(input$min_denominator, rv$ind_data$min_denominator) &&
          input$include == as.logical(rv$ind_data$include) &&
          input$level_direction == as.logical(rv$ind_data$level_direction) &&
          identical(input$level_green, rv$ind_data$level_green) &&
          identical(input$level_yellow, rv$ind_data$level_yellow)) {
        NULL
      } else {
        shiny::actionButton(ns("update_val"), "Oppdat\u00e9r verdier")
      }
    })

    output$edit_ind_title <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::textAreaInput(
        ns("ind_title"), "Indikatortittel (maks 255 tegn)",
        value = rv$ind_data$title, width = "90%", rows = 2
      )
    })

    output$title_oversize <- shiny::renderUI({
      if (rv$title_oversize) {
        shiny::HTML(oversize_message)
      } else {
        NULL
      }
    })

    output$edit_ind_short <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::textAreaInput(
        ns("ind_short"), "Kort indikatorbeskrivelse (maks 1023 tegn)",
        value = rv$ind_data$short_description, width = "90%", rows = 8
      )
    })

    output$short_oversize <- shiny::renderUI({
      if (rv$short_oversize) {
        shiny::HTML(oversize_message)
      } else {
        NULL
      }
    })

    output$edit_ind_long <- shiny::renderUI({
      shiny::req(input$indicator)
      shiny::textAreaInput(
        ns("ind_long"), "Lang indikatorbeskrivelse (maks 2047 tegn)",
        value = rv$ind_data$long_description, width = "90%", rows = 16
      )
    })

    output$long_oversize <- shiny::renderUI({
      if (rv$long_oversize) {
        shiny::HTML(oversize_message)
      } else {
        NULL
      }
    })

    output$update_indicator_txt <- shiny::renderUI({
      if (any(c(rv$title_oversize, rv$short_oversize, rv$long_oversize))) {
        NULL
      } else {
        shiny::actionButton(ns("update_txt"), "Oppdat\u00e9r tekster")
      }
    })

  })
}

#' @rdname mod_indicator
#' @export
indicator_app <- function(pool) {

  ui <- shiny::fluidPage(
    indicator_input("ind")
  )

  server <- function(input, output, sessjon) {
    indicator_server("ind", pool)
  }

  shiny::shinyApp(ui, server)
}