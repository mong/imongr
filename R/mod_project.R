#' Shiny module providing GUI and server logic for the indicator tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#' @param pool_verify A database pool object
#' @param registry_tracker Integer defining registry id
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_project
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
        bslib::layout_columns(
          shiny::uiOutput(ns("enter_start_year")),
          shiny::uiOutput(ns("enter_end_year")),
        ),
        shiny::uiOutput(ns("add_hospitals")),
        shiny::br(),
        shiny::uiOutput(ns("update_values_button")),
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("edit_title")),
        shiny::uiOutput(ns("title_oversize")),
        shiny::uiOutput(ns("edit_short")),
        shiny::uiOutput(ns("short_oversize")),
        shiny::uiOutput(ns("edit_long")),
        shiny::uiOutput(ns("long_oversize")),
        shiny::uiOutput(ns("update_text_button"))
      )
    )
  )
}

#' @rdname mod_project
#' @return A named list of hospital organization numbers with their short names as names
#' @export
get_hospitals_orgnr <- function(pool) {
  hospitals_df <- get_hospitals(pool)
  hospitals_orgnr <- hospitals_df$orgnr
  names(hospitals_orgnr) <- hospitals_df$short_name

  return(hospitals_orgnr)
}

#' @rdname mod_project
#' @export
project_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- get_config()

    rv_return <- shiny::reactiveValues()
    rv <- shiny::reactiveValues()

    project_data <- shiny::reactive({
      get_registry_projects(pool_verify, input$project_registry, input$project_indicator) |>
        dplyr::filter(.data$id == input$project)
    })

    hospitals_orgnr <- get_hospitals_orgnr(pool_verify)

    validateProjectName <- function(x) {
      existing_project_ids <- pool::dbGetQuery(pool_verify, "SELECT id FROM project")$id

      return(validateName(x, existing_project_ids))
    }

    validatePopupStartYear <- function(x) {
      if (is.numeric(x)) {
        return(NULL)
      } else {
        return("Skriv inn et \u00e5rstall")
      }
    }

    validateStartYear <- function(x) {
      if (!shiny::isTruthy(x) || !shiny::isTruthy(input$end_year) || x <= input$end_year) {
        return(NULL)
      } else {
        return("Start\u00e5r kan ikke være større enn slutt\u00e5r")
      }
    }

    validateEndYear <- function(x) {
      if (!shiny::isTruthy(x) || !shiny::isTruthy(input$start_year) || x >= input$start_year) {
        return(NULL)
      } else {
        return("")
      }
    }

    inputValidator <- shinyvalidate::InputValidator$new(session = session)
    inputValidator$add_rule("new_project_name", validateProjectName)
    inputValidator$add_rule("new_project_start_year", validatePopupStartYear)
    inputValidator$add_rule("start_year", validateStartYear)
    inputValidator$add_rule("end_year", validateEndYear)
    inputValidator$enable()

    # Event listener for the new indicator pop up
    popUpListener <- shiny::reactive({
      list(input$new_project_name, input$new_project_start_year)
    })

    # Check if the years are consistent
    years_consistent <- shiny::reactive({
      if (is.na(input$end_year)) {
        # Allow empty value
        TRUE
      } else {
        input$end_year >= input$start_year
      }

    })

    ########################
    ##### Sidebar menu #####
    ########################

    ##### UI elements #####

    # Select registry UI
    output$select_project_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("project_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    # Select indicator UI
    output$select_project_indicator <- shiny::renderUI({
      shiny::req(input$project_registry)
      shiny::selectInput(
        ns("project_indicator"), "Velg indikator:",
        choices = get_registry_indicators(pool_verify, input$project_registry)$id,
      )
    })

    # Select project UI
    output$select_project <- shiny::renderUI({
      rv$new_project_counter
      shiny::req(input$project_registry, input$project_indicator, input$project_indicator)
      shiny::selectInput(
        ns("project"), "Velg prosjekt:",
        choices = get_registry_projects(pool_verify, input$project_registry, input$project_indicator)$id,
        selected = rv$new_project_name, # Switch to the new project when it is made
      )
    })

    # The button for making a new project
    output$add_new_project <- shiny::renderUI({
      shiny::req(input$project_indicator)
      shiny::actionButton(ns("new_project"), "Lag helt nytt prosjekt")
    })

    # Set start year UI
    output$enter_start_year <- shiny::renderUI({
      shiny::req(input$project)
      shiny::numericInput(
        ns("start_year"),
        "Start\u00e5r",
        value = project_data()$start_year,
      )
    })

    # Set end year UI
    output$enter_end_year <- shiny::renderUI({
      shiny::req(input$project)
      shiny::numericInput(
        ns("end_year"),
        "Slutt\u00e5r",
        value = project_data()$end_year,
      )
    })

    # Add hospitals UI
    output$add_hospitals <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("hospitals"),
        label = "Velg sykehus",
        choices = hospitals_orgnr,
        selected = get_project_hospitals(pool_verify, input$project)$hospital_orgnr,
        multiple = TRUE
      )
    })

    # Update values button
    output$update_values_button <- shiny::renderUI({
      # Make an action button with id update_values
      update_project_val_check(input, conf, ns, rv, years_consistent)
    })





    ##### Event observers #####

    # When you push the update values button
    shiny::observeEvent(input$update_values, {

      # Update the projects table
      rv$project_data$start_year <- input$start_year
      rv$project_data$end_year <- input$end_year

      update_project_val(pool_verify, rv$project_data)

      # Update the project_hospital table
      if (!is.null(input$hospitals)) {
        new_data <- data.frame(project_id = input$project, hospital_orgnr = input$hospitals)
      } else {
        new_data <- data.frame()
      }

      update_project_hospitals(pool_verify, input$project, new_data)

      rv$selected_hospitals <- get_project_hospitals(pool_verify, input$project)$hospital_orgnr
    })

    # When you select a registry
    shiny::observeEvent(input$project_registry, {
      rv_return$registry_id <- input$indicator_registry
    })

    # When you select a project
    shiny::observeEvent(input$project, {
      rv$project_data <- project_data()
      rv$selected_hospitals <- get_project_hospitals(pool_verify, input$project)$hospital_orgnr
    })

    # When you push the new project button
    shiny::observeEvent(input$new_project, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$h3("Velg navn p\u00e5 nytt prosjekt"),
          shiny::textInput(ns("new_project_name"), "Prosjektnavn"),
          shiny::numericInput(ns("new_project_start_year"),
            label = "Velg start\u00e5r",
            value = NA
          ),
          footer = shiny::tagList(
            shiny::actionButton(ns("new_project_submit"), "OK"),
            shiny::modalButton("Avbryt")
          )
        )
      )
      shinyjs::disable("new_project_submit")
    })

    # When you write a new project name in the popup
    shiny::observeEvent(popUpListener(), {
      shiny::req("new_project_submit")

      # Validate input
      validName <- is.null(validateProjectName(input$new_project_name))
      validYear <- is.null(validateStartYear(input$new_project_start_year))

      if (validName && validYear) {
        shinyjs::enable("new_project_submit")
      } else {
        shinyjs::disable("new_project_submit")
      }
    })

    # When you press "OK" in the new project popup
    shiny::observeEvent(input$new_project_submit, {
      shiny::removeModal()
      rv$new_project_name <- input$new_project_name
    })

    shiny::observeEvent(rv$new_project_name, {
      add_project(input, rv, pool, pool_verify)
      add_project_to_indicator(pool_verify, rv$new_project_name, input$project_indicator)

      default_text <- data.frame(
        title = "Tittel",
        short_description = "Kort beskrivelse",
        long_description = "Lang beskrivelse",
        id = rv$new_project_name
      )

      update_project_text(pool, default_text)
      update_project_text(pool_verify, default_text)

      # Reactive value to trigger updates of UI elements
      rv$new_project_counter <- rv$new_project_counter + 1
    })





    ######################
    ##### Main panel #####
    ######################

    ##### UI elements #####

    # Project title text input
    output$edit_title <- shiny::renderUI({
      shiny::req(input$project)
      shiny::textAreaInput(
        ns("title"), "Prosjekttittel (maks 255 tegn)",
        value = project_data()$title, width = "90%", rows = 2
      )
    })

    # Project short description text input
    output$edit_short <- shiny::renderUI({
      shiny::req(input$project)
      shiny::textAreaInput(
        ns("short_description"), "Kort prosjektbeskrivelse (maks 1023 tegn)",
        value = project_data()$short_description, width = "90%", rows = 8
      )
    })

    # Project long description text input
    output$edit_long <- shiny::renderUI({
      shiny::req(input$project)
      shiny::textAreaInput(
        ns("long_description"), "Lang prosjektbeskrivelse (maks 2047 tegn)",
        value = project_data()$long_description, width = "90%", rows = 16
      )
    })

    # Update button
    output$update_text_button <- shiny::renderUI({
      # Make an action button with id update_text
      update_project_txt_check(input, conf, ns, rv)
    })

    ##### Oversize warnings #####
    output$title_oversize <- shiny::renderUI({
      shiny::req(rv$title_oversize)
      oversize_check(rv$title_oversize, conf)
    })

    output$short_oversize <- shiny::renderUI({
      shiny::req(rv$short_oversize)
      oversize_check(rv$short_oversize, conf)
    })

    output$long_oversize <- shiny::renderUI({
      shiny::req(rv$long_oversize)
      oversize_check(rv$long_oversize, conf)
    })





    ##### Event observers #####

    # Button click observer
    shiny::observeEvent(input$update_text, {
      # Update local data
      rv$project_data <- project_data()
      rv$project_data$title <- input$title
      rv$project_data$short_description <- input$short_description
      rv$project_data$long_description <- input$long_description

      # Update database
      update_project_text(pool_verify, rv$project_data)
    })

    ###### Oversize observers #####
    shiny::observeEvent(input$title, {
      shiny::req(input$title)
      rv$title_oversize <- ifelse(nchar(input$title) > 255, TRUE, FALSE)
    })

    shiny::observeEvent(input$short_description, {
      shiny::req(input$short_description)
      rv$short_oversize <- ifelse(nchar(input$short_description) > 1023, TRUE, FALSE)
    })

    shiny::observeEvent(input$long_description, {
      shiny::req(input$long_description)
      rv$long_oversize <- ifelse(nchar(input$long_description) > 2047, TRUE, FALSE)
    })

    return(rv_return)
  })
}
