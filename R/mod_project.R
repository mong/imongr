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
        bslib::layout_columns(
          shiny::uiOutput(ns("enter_start_year")),
          shiny::uiOutput(ns("enter_end_year")),
        ),
        shiny::uiOutput(ns("add_hospitals"))
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

    hospitals_orgnr <- get_hospitals_orgnr(pool_verify)

    validateProjectName <- function(x) {
      existing_project_ids <- pool::dbGetQuery(pool_verify, "SELECT id FROM project")$id

      return(validateName(x, existing_project_ids))
    }
    validateStartYear <- function(x) {
      if (is.numeric(x)) {
        return(NULL)
      } else {
        return("Skriv inn et årstall")
      }
    }
    inputValidator <- shinyvalidate::InputValidator$new(session = session)
    inputValidator$add_rule("new_project_name", validateProjectName)
    inputValidator$add_rule("new_project_start_year", validateStartYear)
    inputValidator$enable()

    # Event listener for the new indicator pop up
    popUpListener <- shiny::reactive({
      list(input$new_project_name, input$new_project_start_year)
    })

    ########################
    ##### Sidebar menu #####
    ########################

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
      shiny::req(input$project_registry, input$project_indicator, input$project_indicator)
      shiny::selectInput(
        ns("project"), "Velg prosjekt:",
        choices = rv$indicator_projects_data$id,
        selected = rv$new_poject_name, # Switch to the new project when it is made
      )
    })

    # Set start year UI
    output$enter_start_year <- shiny::renderUI({
      shiny::req(input$project)
      shiny::numericInput(
        ns("start_year"),
        "Startår",
        value = rv$project_data$start_year,
      )
    })

    # Set end year UI
    output$enter_end_year <- shiny::renderUI({
      shiny::req(input$project)
      shiny::numericInput(
        ns("end_year"),
        "Sluttår",
        value = rv$project_data$end_year,
      )
    })

    # Add hospitals UI
    output$add_hospitals <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("hospitals"),
        label = "Velg sykehus",
        choices = hospitals_orgnr,
        selected = rv$selected_hospitals,
        multiple = TRUE
      )
    })

    # When you select a registry
    shiny::observeEvent(input$project_registry, {
      rv_return$registry_id <- input$indicator_registry
    })

    # When you select an indicator
    shiny::observeEvent(input$project_indicator, {
      rv$indicator_projects_data <- get_registry_projects(pool_verify, input$project_registry, input$project_indicator)
    })

    # When you select a project
    shiny::observeEvent(input$project, {
      rv$project_data <- rv$indicator_projects_data |>
        dplyr::filter(.data$id == input$project)
      rv$selected_hospitals <- get_project_hospitals(pool_verify, input$project)$hospital_orgnr
    })

    # The button for making a new project
    output$add_new_project <- shiny::renderUI({
      shiny::req(input$project_indicator)
      shiny::actionButton(ns("new_project"), "Lag helt nytt prosjekt")
    })

    # When you push the new project button
    shiny::observeEvent(input$new_project, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$h3("Velg navn p\u00e5 nytt prosjekt"),
          shiny::textInput(ns("new_project_name"), "Prosjektnavn"),
          shiny::numericInput(ns("new_project_start_year"),
            label = "Velg startår",
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
      # TODO: Add the project to the database
      query <- paste0("INSERT INTO project (id, registry_id, start_year) VALUES ( '",
                      rv$new_project_name,
                      "', '",
                      input$project_registry,
                      "', '",
                      input$new_project_start_year,
                      "');")

      new_project_data <- data.frame(
        id = rv$new_project_name,
        registry_id = input$project_registry,
        start_year = NA,
        end_year = NA,
        title = "",
        short_description = "",
        long_description = ""
      )

      pool::dbExecute(pool, query)
      pool::dbExecute(pool_verify, query)

      # Reactive value to trigger updates of UI elements
      rv$new_project_counter <- rv$new_project_counter + 1
    })

    ######################
    ##### Main panel #####
    ######################

    # Project title text input
    output$edit_title <- shiny::renderUI({
      shiny::req(input$project)
      shiny::textAreaInput(
        ns("title"), "Prosjekttittel (maks 255 tegn)",
        value = rv$project_data$title, width = "90%", rows = 2
      )
    })

    # Project short description text input
    output$edit_short <- shiny::renderUI({
      shiny::req(input$project)
      shiny::textAreaInput(
        ns("short_description"), "Kort indikatorbeskrivelse (maks 1023 tegn)",
        value = rv$project_data$short_description, width = "90%", rows = 8
      )
    })

    # Project long description text input
    output$edit_long <- shiny::renderUI({
      shiny::req(input$project)
      shiny::textAreaInput(
        ns("long_description"), "Lang indikatorbeskrivelse (maks 2047 tegn)",
        value = rv$project_data$long_description, width = "90%", rows = 16
      )
    })

    return(rv_return)
  })
}
