#' Shiny module providing GUI and server logic for the publication tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#' @param pool_verify A database pool object
#' @param registry_tracker Integer defining registry id
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_publication
#' @aliases publication_ui publication_server publication_app
NULL

#' @rdname mod_publication
#' @export
publication_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_registry")),
        shiny::uiOutput(ns("add_publication_button")),
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("publication_list")),
        shiny::uiOutput(ns("update_list_button"))
      )
    )
  )

}#' @rdname mod_publication
#' @export
publication_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- get_config()

    rv_return <- shiny::reactiveValues()
    rv <- shiny::reactiveValues()

    validate_doi <- function(x) {
      if (!is.null(x) && x == "") {
        return("Skriv inn en gyldig DOI")
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

    # The button for adding a new publication
    output$add_publication_button <- shiny::renderUI({
      shiny::req(input$registry)
      shiny::actionButton(ns("new_publication"), "Legg til en publikasjon")
    })


    ######################
    ##### Main menu #####
    ######################

    # The publication list
    output$publication_list <- shiny::renderUI({
      DT::dataTableOutput(ns("publication_table"))
    })

    output$publication_table <- DT::renderDataTable(
      DT::datatable(rv$publication_data,
        colnames = c("DOI", "Bruker", "Tidspunkt"),
        rownames = FALSE
      )
    )

    ##### Event observers #####

    shiny::observeEvent(input$registry, {
      rv$publication_data <- get_publications(pool_verify, input$registry)
      rv_return$registry_id <- input$registry
    })

    popUpListener <- shiny::reactive({
      list(input$new_doi)
    })

    # When you push the new DOI button
    shiny::observeEvent(input$new_publication, {
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
        shinyjs::enable("new_doi_submit")
        output$doi_info <- shiny::renderText(
          tryCatch(
            {
              httr::content(
                httr::GET(
                  paste0("https://citation.doi.org/format?doi=", input$new_doi, "&style=apa&lang=nb-NO")
                )
              )
            },
            error = function(er) {
              return(NULL)
            }
          )
        )
      } else {
        shinyjs::disable("new_doi_submit")
      }
    })

    # When you press "OK" in the new DOI popup
    shiny::observeEvent(input$new_doi_submit, {
      shiny::removeModal()
      rv$new_doi <- input$new_doi
      add_publication(input, rv, pool_verify)
      rv$publication_data <- get_publications(pool_verify, input$registry)
    })

    return(rv_return)
  })
}