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

    validate_doi <- function(x) {
      if (!is.null(x) && x == "") {
        return("Skriv inn en gyldig DOI")
      }

      if (!is.null(x) && !is.null(rv$event_data) && x %in% rv$event_data$doi) {
        return("Publikasjonen finnes allerede")
      }

      url <- paste0("https://doi.org/", x)

      tryCatch(
        {
          response <- httr::GET(url)
          response_code <- response$all_headers[[1]]$status

          if (response_code != 302) {
            return("Skriv inn en gyldig DOI")
          } else {
            return(NULL)
          }
        },
        error = function(er) {
          return("Skriv inn en gyldig DOI")
        }
      )
    }

    inputValidator <- shinyvalidate::InputValidator$new(session = session)
    inputValidator$add_rule("new_doi", validate_doi)
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

    # The button for adding a new event
    output$add_event_button <- shiny::renderUI({
      shiny::req(input$registry)
      shiny::actionButton(ns("new_event"), "Legg til en publikasjon")
    })


    ######################
    ##### Main menu #####
    ######################

    # The event list
    output$event_list <- shiny::renderUI({
      DT::dataTableOutput(ns("event_table"))
    })

    output$event_table <- DT::renderDataTable(
      DT::datatable(rv$event_data,
        colnames = c("DOI", "Bruker", "Tidspunkt", "Referanse"),
        rownames = FALSE
      )
    )

    ##### Event observers #####

    shiny::observeEvent(input$registry, {
      rv$event_data <- get_publications(pool_verify, input$registry)
      rv_return$registry_id <- input$registry
    })

    popUpListener <- shiny::reactive({
      list(input$new_doi)
    })

    # When you push the new DOI button
    shiny::observeEvent(input$new_event, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$h3("Legg inn DOI-en til publikasjonen"),
          shiny::textInput(ns("new_doi"), "DOI"),
          shiny::textOutput(ns("doi_info")),
          footer = shiny::tagList(
            shiny::actionButton(ns("new_doi_submit"), "OK"),
            shiny::modalButton("Avbryt")
          )
        )
      )
      shinyjs::disable("new_doi_submit")
    })

    # When you write a DOI in the popup
    shiny::observeEvent(popUpListener(), {
      shiny::req("new_doi_submit")

      # Validate input
      valid_doi <- is.null(validate_doi(input$new_doi))

      if (valid_doi) {
        output$doi_info <- shiny::renderText(
          tryCatch(
            {
              rv$new_reference <- httr::content(
                httr::GET(
                  paste0("https://citation.doi.org/format?doi=", input$new_doi, "&style=apa&lang=nb-NO")
                )
              )
              rv$new_reference
            },
            error = function(er) {
              return(NULL)
            }
          )
        )
        shinyjs::enable("new_doi_submit")
      } else {
        shinyjs::disable("new_doi_submit")
      }
    })

    # When you press "OK" in the new DOI popup
    shiny::observeEvent(input$new_doi_submit, {
      shiny::removeModal()
      add_publication(input, rv, pool_verify)
      rv$event_data <- get_publications(pool_verify, input$registry)
    })

    return(rv_return)
  })
}
