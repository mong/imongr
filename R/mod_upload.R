#' Shiny module providing GUI and server logic for the upload data tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_upload
#' @aliases upload_ui upload_server upload_app
NULL

#' @rdname mod_upload
#' @export
upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_registry")),
        shiny::uiOutput(ns("upload_file")),
        shiny::radioButtons(ns("sep"), "Kolonneseparator",
          choices = c(
            Semikolon = ";",
            Komma = ",",
            Tab = "\t"
          ),
          selected = ";"
        ),
        shiny::radioButtons(ns("dec_sep"), "Desimalseparator",
          choices = c(
            Punktum = ".",
            Komma = ","
          ),
          selected = ","
        ),
        shiny::numericInput(ns("sample_size"),
          "Antall rader vist:",
          20,
          min = 1,
          max = 50
        ),
        shiny::selectInput(
          ns("sample_type"),
          "Utvalg:",
          list(
            `toppen` = FALSE,
            `tilfeldig` = TRUE
          ),
          FALSE
        ),
        shiny::dateInput(ns("latest_update"),
          "Alle indikatorer er oppdatert per:",
          value = Sys.Date(),
          weekstart = 1,
          language = "no"
        ),
        shiny::dateInput(ns("latest_affirm"),
          "Merk alle indikatorer som forel\u00f8pig etter:",
          value = paste0(format(Sys.Date(), "%Y"), "-01-01"),
          weekstart = 1,
          language = "no"
        ),
        shinycssloaders::withSpinner(
          shiny::textOutput(ns("spinner")),
          color = "#18bc9c",
          color.background = "#ffffff",
          type = 7,
          proxy.height = 80
        ),
        shiny::uiOutput(ns("submit"))
      ),

      # Main panel for displaying outputs ----
      shiny::mainPanel(
        shiny::htmlOutput(ns("in_progress")),
        shiny::htmlOutput(ns("error_report")),
        shiny::htmlOutput(ns("warning_report")),
        shiny::titlePanel("Last opp fil"),
        shiny::htmlOutput(ns("upload_sample_text")),
        shiny::tableOutput(ns("upload_sample")),
        shiny::h3("Veiledning"),
        shiny::htmlOutput(ns("main_doc")),
        shiny::htmlOutput(ns("var_doc")),
        shiny::htmlOutput(ns("valid_ind")),
        shiny::tableOutput(ns("valid_ind_tab")),
        shiny::h4("Eksempeldata:"),
        shiny::tableOutput(ns("sample_data")),
      )
    )
  )
}


#' @rdname mod_upload
#' @export
upload_server <- function(id, registry_tracker, pool_verify) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conf <- get_config()

    rv <- shiny::reactiveValues(
      inv_data = 0
    )

    rv_return <- shiny::reactiveValues()
    
    ## observers
    shiny::observeEvent(input$registry, {
      rv_return$registry_id <- input$registry
      if (!is.null(input$upload_file)) {
        rv$inv_data <- rv$inv_data + 1
      }
    })
    shiny::observeEvent(input$submit, {
      insert_data_verify(
        pool = pool_verify,
        df = df(),
        update = input$latest_update,
        affirm = input$latest_affirm
      )
      insert_agg_data(pool_verify, df())
      rv$inv_data <- rv$inv_data + 1
      shinyalert::shinyalert(conf$upload$reciept$title,
        conf$upload$reciept$body,
        type = "success",
        showConfirmButton = FALSE,
        timer = 7000
      )
    })

    ## reactives
    df <- shiny::reactive({
      if (is.null(input$upload_file)) {
        data.frame()
      } else {
        csv_to_df(input$upload_file$datapath, input$sep, input$dec_sep)
      }
    })

    ind <- shiny::reactive({
      if (is.null(input$registry)) {
        data.frame()
      } else {
        get_registry_ind(pool_verify, input$registry)
      }
    })

    ## ui sidebar panel
    output$select_registry <- shiny::renderUI({
      select_registry_ui(pool_verify, conf, input_id = ns("registry"),
                         context = "verify", current_reg = registry_tracker$current_registry)

    })

    output$upload_file <- shiny::renderUI({
      shiny::fileInput(
        ns("upload_file"),
        "Velg csv-fil:",
        buttonLabel = "Velg fil...",
        placeholder = "Ingen fil er valgt",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      )
    })

    output$submit <- shiny::renderUI({
      rv$inv_data
      submit_ui(
        ns("submit"), conf, pool_verify, input$upload_file,
        input$registry, df(), ind(), "verify"
      )
    })

    output$spinner <- shiny::renderText({
      input$submit
      paste("")
    })


    ## ui main panel
    output$error_report <- shiny::renderText({
      rv$inv_data
      error_report_ui(
        pool_verify, df(), ind(),
        input$upload_file, input$registry
      )
    })

    output$warning_report <- shiny::renderText({
      rv$inv_data
      warning_report_ui(pool_verify, df(), input$upload_file, input$registry)
    })

    output$upload_sample_text <- shiny::renderText({
      shiny::req(input$registry)
      if (input$registry == "") {
        NULL
      } else {
        upload_sample_text_ui(pool_verify, conf, input$upload_file,
          input$registry,
          indicators = unique(df()$ind_id)
        )
      }
    })

    output$upload_sample <- shiny::renderTable({
      rv$inv_data
      upload_sample_ui(
        df(), input$upload_file, input$registry,
        input$sample_size, input$sample_type
      )
    })

    output$main_doc <- shiny::renderText(conf$upload$doc$main)

    output$var_doc <- shiny::renderText({
      var_doc_ui(conf)
    })

    output$valid_ind <- shiny::renderText({
      paste0(
        "<h4>", conf$upload$doc$valid_ind, " <i>",
        get_registry_name(pool_verify, shiny::req(input$registry),
          full_name = TRUE
        ),
        "</i>:</h4>"
      )
    })

    output$valid_ind_tab <- shiny::renderTable(
      get_registry_indicators(pool_verify, shiny::req(input$registry)),
      rownames = TRUE,
      colnames = FALSE
    )

    output$sample_data <- shiny::renderTable(
      get_table(pool_verify, "data",
        sample = 0.00001
      )[conf$db$tab$data$insert[conf$upload$data_var_ind]]
    )

    return(rv_return)
  })
}

#' @rdname mod_upload
#' @export
upload_app <- function(pool) {
  ui <- shiny::fluidPage(
    upload_ui("ind")
  )

  server <- function(input, output, sessjon) {
    upload_server("ind", pool)
  }

  shiny::shinyApp(ui, server)
}
