#' Shiny module providing GUI and server logic for the download data tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#' @param pool_verify A database pool object
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_download
#' @aliases download_ui download_server download_app
NULL

#' @rdname mod_download
#' @export
download_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectInput(
          ns("download_context"),
          "Velg datakilde for nedlasting:",
          list(
            `Kvalitetskontroll` = "verify",
            `Publiserte data` = "prod"
          )
        ),
        shiny::uiOutput(ns("select_download_registry")),
        shiny::uiOutput(ns("select_db_table")),
        shiny::tags$div(
          title = paste(
            "csv (nordisk): semikolon-delt csv med komma som",
            "desimalskilletegn"
          ),
          shiny::selectInput(
            ns("file_format"),
            "Filformat:",
            c("csv", "csv (nordisk)", "rds")
          )
        ),
        shiny::downloadButton(ns("download_db_table"), "Hent fra server")
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("ui_db_table"))
      )
    )
  )
}


#' @rdname mod_download
#' @export
download_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conf <- get_config()

    rv_return <- shiny::reactiveValues()

    shiny::observeEvent(input$download_registry, {
      rv_return$registry_id <- input$download_registry
    })

    pool_download <- shiny::reactive({
      if (is.null(input$download_context)) {
        pool_verify
      } else {
        if (input$download_context == "verify") {
          pool_verify
        } else {
          pool
        }
      }
    })

    db_table <- shiny::reactive({
      if (input$download_registry == "") {
        data.frame()
      } else {
        if (input$tab_set == "data") {
          get_registry_data(pool_download(), input$download_registry)
        } else if (input$tab_set == "ind") {
          get_registry_ind(pool_download(), input$download_registry)
        } else if (input$tab_set == "agg_data") {
          get_aggdata(pool_download(), input$download_registry)
        } else {
          get_table(pool_download(), input$tab_set)
        }
      }
    })

    output$select_download_registry <- shiny::renderUI({
      select_registry_ui(pool_download(),
      conf,
      input_id = ns("download_registry"),
      context = input$download_context,
      show_context = FALSE,
      current_reg = registry_tracker$current_registry)
    })

    output$select_db_table <- shiny::renderUI({
      shiny::selectInput(ns("tab_set"), "Velg tabell:", conf$download$tab,
        selected = conf$download$tab[1]
      )
    })

    output$download_db_table <- shiny::downloadHandler(
      filename = function() {
        if (input$file_format %in% c("csv", "csv (nordisk)")) {
          basename(tempfile(fileext = ".csv"))
        } else {
          basename(tempfile(fileext = ".rds"))
        }
      },
      content = function(file) {
        switch(input$file_format,
          `csv` = readr::write_csv(db_table(), file),
          `csv (nordisk)` = readr::write_csv2(db_table(), file),
          `rds` = readr::write_rds(db_table(), file)
        )
      }
    )

    output$db_table <- DT::renderDataTable(
      DT::datatable(db_table(),
        rownames = FALSE,
        options = list(
          dom = "lftp",
          language = list(
            lengthMenu = "Vis _MENU_ rader per side",
            search = "S\u00f8k:",
            info = "Rad _START_ til _END_ av totalt _TOTAL_",
            paginate = list(previous = "Forrige", `next` = "Neste")
          )
        )
      )
    )

    output$ui_db_table <- shiny::renderUI(
      DT::dataTableOutput(ns("db_table"))
    )

    return(rv_return)
  })
}

#' @rdname mod_download
#' @export
download_app <- function(pool, pool_verify) {
  ui <- shiny::fluidPage(
    download_ui("download")
  )

  server <- function(input, output, session) {
    download_server("download", pool, pool_verify)
  }

  shiny::shinyApp(ui, server)
}
