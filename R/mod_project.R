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
        shiny::uiOutput(ns("select_project_registry")),
        shiny::uiOutput(ns("select_project_indicator")),
        shiny::uiOutput(ns("select_project")),
        shiny::uiOutput(ns("add_new_project")),
        shiny::hr(),
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
    rv <- shiny::reactiveValues()


    ########################
    ##### Sidebar menu #####
    ########################

    output$select_project_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("project_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    output$select_project_indicator <- shiny::renderUI({
      shiny::req(input$project_registry)
      shiny::selectInput(
        ns("project_indicator"), "Velg indikator:",
        choices = get_registry_indicators(pool_verify, input$project_registry)$id,
      )
    })

    output$select_project <- shiny::renderUI({
      shiny::req(input$project_registry, input$project_indicator, input$project_indicator)
      shiny::selectInput(
        ns("project"), "Velg prosjekt:",
        choices = rv$project_data$id,
      )
    })

    # When you select a registry
    shiny::observeEvent(input$project_registry, {
      rv_return$registry_id <- input$indicator_registry
    })

    shiny::observeEvent(input$project_indicator, {
      rv$project_data <- get_registry_projects(pool_verify, input$project_registry, input$project_indicator)
    })

    output$add_new_project <- shiny::renderUI({
      shiny::req(input$project_indicator)
      shiny::actionButton(ns("new_project"), "Lag helt nytt prosjekt")
    })

    ######################
    ##### Main panel #####
    ######################

    output$edit_title <- shiny::renderUI({
      shiny::req(input$project)
      shiny::textAreaInput(
        ns("title"), "Prosjekttittel (maks 255 tegn)",
        value = rv$project_data |>
          dplyr::filter(.data$id == input$project) |>
          dplyr::pull(.data$title), width = "90%", rows = 2
      )
    })

    output$edit_short <- shiny::renderUI({
      shiny::req(input$project)
      shiny::textAreaInput(
        ns("short_description"), "Kort indikatorbeskrivelse (maks 1023 tegn)",
        value = rv$project_data |>
          dplyr::filter(.data$id == input$project) |>
          dplyr::pull(.data$short_description), width = "90%", rows = 8
      )
    })

    output$edit_long <- shiny::renderUI({
      shiny::req(input$project)
      shiny::textAreaInput(
        ns("long_description"), "Lang indikatorbeskrivelse (maks 2047 tegn)",
        value = rv$project_data |>
          dplyr::filter(.data$id == input$project) |>
          dplyr::pull(.data$long_description), width = "90%", rows = 16
      )
    })

    return(rv_return)
  })
}
