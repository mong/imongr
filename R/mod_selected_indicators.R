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
        shiny::uiOutput(ns("add_hospitals")),
        shiny::uiOutput(ns("add_hfs")),
        shiny::uiOutput(ns("add_rhfs")),
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

    rhfs_orgnr <- get_named_orgnr(pool_verify, "rhf")
    hfs_orgnr <- get_named_orgnr(pool_verify, "hf")
    hospitals_orgnr <- get_named_orgnr(pool_verify, "hospital")

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

    output$select_indicator <- shiny::renderUI({
      shiny::req(input$selected_registry)
      shiny::selectInput(
        ns("selected_indicator"), "Velg indikator:",
        choices = get_registry_indicators(pool_verify, input$selected_registry)$id
      )
    })

    output$update_button <-  shiny::renderUI({
      update_ind_units_check(input, conf, ns, rv)
    })

    # When you select an indicator
    shiny::observeEvent(input$selected_indicator, {
      ind_units <- get_ind_units(pool_verify, input$selected_indicator)
      rv$hospitals <- ind_units$hospital_orgnr
      rv$hfs <- ind_units$hf_orgnr
      rv$rhfs <- ind_units$rhf_orgnr

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

      new_hospitals <- c(input$hospitals, rep(NA, hf_count + rhf_count))
      new_hfs <- c(rep(NA, hospital_count), input$hfs, rep(NA, rhf_count))
      new_rhfs <- c(rep(NA, hospital_count + hf_count), input$rhfs)

      new_data <- data.frame(ind_id = input$selected_indicator, hospital_orgnr = new_hospitals, hf_orgnr = new_hfs, rhf_orgnr = new_rhfs)
      
      update_ind_units(pool_verify, input$selected_indicator, new_data)

      # Reload data from the data base
      ind_units <- get_ind_units(pool_verify, input$selected_indicator)
      rv$hospitals <- ind_units$hospital_orgnr
      rv$hfs <- ind_units$hf_orgnr
      rv$rhfs <- ind_units$rhf_orgnr
    })

    # Add RHFs UI
    output$add_rhfs <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("rhfs"),
        label = "Velg RHF",
        choices = rhfs_orgnr,
        selected = rv$rhfs,
        multiple = TRUE
      )
    })

    # Add hospitals UI
    output$add_hfs <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("hfs"),
        label = "Velg HF",
        choices = hfs_orgnr,
        selected = rv$hfs,
        multiple = TRUE
      )
    })

    # Add hospitals UI
    output$add_hospitals <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("hospitals"),
        label = "Velg sykehus",
        choices = hospitals_orgnr,
        selected = rv$hospitals,
        multiple = TRUE
      )
    })

    return(rv_return)
  })
}
