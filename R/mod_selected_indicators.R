#' Shiny module providing GUI and server logic for the selected indicators tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#' @param pool_verify A database pool object
#' @param registry_tracker Integer defining registry id
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_selected_indicators
#' @aliases selected_indicators_ui selected_indicators_server
NULL

#' @rdname mod_selected_indicators
#' @export
selected_indicators_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_registry")),
        shiny::uiOutput(ns("select_indicator")),
        shiny::hr(),
        shiny::uiOutput(ns("update_button")),
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("add_rhfs")),
        shiny::uiOutput(ns("add_hfs")),
        shiny::uiOutput(ns("add_hospitals"))
      )
    )
  )
}

#' @rdname mod_selected_indicators
#' @export
selected_indicators_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- get_config()

    rv <- shiny::reactiveValues()
    rv_return <- shiny::reactiveValues()

    rhf_unit_names <- get_rhfs(pool_verify)$short_name
    hf_unit_names <- get_hfs(pool_verify)$short_name
    hospital_unit_names <- get_hospitals(pool_verify)$short_name

    # Select registry drop down menu
    output$select_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf,
        input_id = ns("selected_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    # When you select a registry
    shiny::observeEvent(input$selected_registry, {
      rv_return$registry_id <- input$selected_registry
    })

    # Select indicator drop down menu
    output$select_indicator <- shiny::renderUI({
      shiny::req(input$selected_registry)
      shiny::selectInput(
        ns("selected_indicator"), "Velg indikator:",
        choices = get_registry_indicators(pool_verify, input$selected_registry)$id
      )
    })

    # Update button
    output$update_button <- shiny::renderUI({
      update_ind_units_check(input, conf, ns, rv)
    })

    # When you select an indicator
    shiny::observeEvent(input$selected_indicator, {
      ind_units <- get_ind_units(pool_verify, input$selected_indicator)
      rv$hospitals <- ind_units$hospital_short_name
      rv$hfs <- ind_units$hf_short_name
      rv$rhfs <- ind_units$rhf_short_name

      # Remove NULL rows
      rv$hospitals <- rv$hospitals[!is.na(rv$hospitals)]
      rv$hfs <- rv$hfs[!is.na(rv$hfs)]
      rv$rhfs <- rv$rhfs[!is.na(rv$rhfs)]
    })

    # When you click the update button
    shiny::observeEvent(input$update_units, {
      hospital_count <- length(input$hospitals)
      hf_count <- length(input$hfs)
      rhf_count <- length(input$rhfs)

      if (hospital_count + hf_count + rhf_count > 0) {
        new_hospitals <- c(input$hospitals, rep(NA, hf_count + rhf_count))
        new_hfs <- c(rep(NA, hospital_count), input$hfs, rep(NA, rhf_count))
        new_rhfs <- c(rep(NA, hospital_count + hf_count), input$rhfs)

        new_data <- data.frame(ind_id = input$selected_indicator,
                               hospital_short_name = new_hospitals,
                               hf_short_name = new_hfs,
                               rhf_short_name = new_rhfs)
      } else {
        new_data <- data.frame()
      }
      update_ind_units(pool_verify, input$selected_indicator, new_data)

      # Reload data from the data base
      ind_units <- get_ind_units(pool_verify, input$selected_indicator)
      rv$hospitals <- ind_units$hospital_short_name
      rv$hfs <- ind_units$hf_short_name
      rv$rhfs <- ind_units$rhf_short_name

      # Remove NULL rows
      rv$hospitals <- rv$hospitals[!is.na(rv$hospitals)]
      rv$hfs <- rv$hfs[!is.na(rv$hfs)]
      rv$rhfs <- rv$rhfs[!is.na(rv$rhfs)]
    })

    # Add RHFs input field
    output$add_rhfs <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("rhfs"),
        label = "Velg RHF",
        choices = rhf_unit_names,
        selected = rv$rhfs,
        multiple = TRUE
      )
    })

    # Add hospitals input field
    output$add_hfs <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("hfs"),
        label = "Velg HF",
        choices = hf_unit_names,
        selected = rv$hfs,
        multiple = TRUE
      )
    })

    # Add hospitals input field
    output$add_hospitals <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("hospitals"),
        label = "Velg sykehus",
        choices = hospital_unit_names,
        selected = rv$hospitals,
        multiple = TRUE
      )
    })

    return(rv_return)
  })
}
