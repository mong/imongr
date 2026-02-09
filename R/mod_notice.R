#' Shiny module providing GUI and server logic for the notice tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#' @param pool_verify A database pool object
#' @param registry_tracker Integer defining registry id
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_notice
#' @aliases notice_ui notice_server notice_app
NULL

#' @rdname mod_notice
#' @export
notice_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_registry")),
        shiny::uiOutput(ns("select_notice")),
        shiny::br(),
        shiny::uiOutput(ns("set_status")),
        shiny::uiOutput(ns("set_ref")),
        shiny::uiOutput(ns("save_changes_button")),
        shiny::br(),
        shiny::uiOutput(ns("add_event_button")),
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("event_list")),
        shiny::uiOutput(ns("update_list_button"))
      )
    )
  )

}

#' @rdname mod_notice
#' @export
notice_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- get_config()

    rv_return <- shiny::reactiveValues()
    rv <- shiny::reactiveValues()

    validate_notice_ref <- function(x) {
      if (!is.null(x) && x == "") {
        return("Skriv inn en gyldig referanse")
      }

      if (
          !is.null(x) &&
            !is.null(rv$all_notices_data) &&
            x %in% rv$all_notices_data$ref) {
        return("Referansen finnes allerede")
      }
    }

    validate_event_ref <- function(x) {
      if (!is.null(x) && x == "") {
        return("Skriv inn en gyldig referanse")
      }

      if (!is.null(x) && !is.null(rv$event_data) && x %in% rv$event_data$ref) {
        return("Referansen finnes allerede")
      }
    }

    inputValidator <- shinyvalidate::InputValidator$new(session = session)
    inputValidator$add_rule("new_ref", validate_notice_ref)
    inputValidator$enable()

    ########################
    ##### Sidebar menu #####
    ########################

    ##### UI elements #####

    # Select registry UI
    output$select_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    # Select notice
    output$select_notice <- shiny::renderUI({
      shiny::selectInput(
        ns("selected_year"),
        "Velg \u00e5r",
        rv$notice_data$year
      )
    })

    # Set status
    output$set_status <- shiny::renderUI({

      shiny::selectInput(
        ns("notice_status"),
        "Endre status",
        setNames(c("Open", "Closed"), c("Åpent", "Lukket")),
        selected = rv$notice_data$status[rv$notice_data$year == input$selected_year]
      )
    })

    # Reference text field
    output$set_ref <- shiny::renderUI({
      shiny::textInput(
        ns("new_ref"),
        "Referanse",
        value = rv$notice_data$ref[rv$notice_data$year == input$selected_year]
      )
    })

    # Save notice changes button
    output$save_changes_button <- shiny::renderUI({
      shiny::req(input$registry)
      shiny::req(is.null(validate_notice_ref(input$new_ref)))

      shiny::actionButton(
        ns("save_changes"),
        "Lagre",
        shiny::icon("floppy-disk"),
        style = conf$profile$action_button_style,
      )
    })

    # The button for adding a new event
    output$add_event_button <- shiny::renderUI({
      shiny::req(input$registry)
      shiny::actionButton(ns("new_event"), "Legg til en hendelse")
    })

    ##### Event handlers #####



    ######################
    ##### Main menu #####
    ######################

    # The event list
    output$event_list <- shiny::renderUI({
      DT::dataTableOutput(ns("event_table"))
    })

    output$event_table <- DT::renderDataTable(
      DT::datatable(rv$event_data,
        colnames = c("Tekst", "Dato", "type", "Referanse", "Bruker"),
        rownames = FALSE
      )
    )

    ##### Event observers #####

    shiny::observeEvent(input$registry, {
      rv$notice_data <- get_registry_notices(pool, input$registry)
      rv$all_notices_data <- get_all_notices(pool)
      rv_return$registry_id <- input$registry
    })

    shiny::observeEvent(input$selected_year, {
      shiny::req(input$registry, input$selected_year)
      rv$event_data <- get_notice_events(pool, get_notice_id(pool, input$registry, input$selected_year))
    })

    popUpListener <- shiny::reactive({
      list(input$new_event_ref)
    })

    # When you push the save changes button
    shiny::observeEvent(input$save_changes, {
      update_notice(input, get_notice_id(pool, input$registry, input$selected_year), pool)
      rv$notice_data <- get_registry_notices(pool, input$registry)
    })

    # When you push the new event button
    shiny::observeEvent(input$new_event, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$h3("Fritekst"),
          shiny::textInput(ns("new_event_text"), "Fritekst"),
          shiny::tags$h3("Legg inn referansen til hendelsen"),
          shiny::textInput(ns("new_event_ref"), "Referanse"),
          shiny::tags$h3("Legg inn referansen til hendelsen"),
          shiny::dateInput(ns("new_event_date"), "Dato"),
          shiny::tags$h3("Type"),
          shiny::selectInput(ns("new_event_type"), "Type", choices = c("a", "b", "c")),
          footer = shiny::tagList(
            shiny::actionButton(ns("new_event_submit"), "OK"),
            shiny::modalButton("Avbryt")
          )
        )
      )
      shinyjs::disable("new_event_submit")
    })

    # When you write a DOI in the popup
    shiny::observeEvent(popUpListener(), {
      shiny::req("new_event_submit")

      # Validate input
      valid_ref <- is.null(validate_event_ref(input$new_event_ref))

      if (valid_ref) {
        shinyjs::enable("new_event_submit")
      } else {
        shinyjs::disable("new_event_submit")
      }
    })

    # When you press "OK" in the new DOI popup
    shiny::observeEvent(input$new_event_submit, {

      shiny::removeModal()
      add_event(input, rv, pool)
      rv$event_data <- get_notice_events(pool, get_notice_id(pool, input$registry, input$selected_year))
    })

    return(rv_return)
  })
}
