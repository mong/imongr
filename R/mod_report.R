#' Shiny module providing GUI and server logic for reports
#'
#' @param id Character string module namespace
#' @param pool A database pool object connecting to production data
#' @param pool_verify A database pool object connecting to staging data
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_report
#' @aliases report_ui report_server report_app
NULL

#' @rdname mod_report
#' @export
report_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(ns("report"), "Velg rapport:", c("Registerstatus")),
        shiny::tags$div(
          title = paste("csv (nordisk): semikolon-delt csv med komma som",
                        "desimalskilletegn"),
          shiny::selectInput(
            ns("file_format"),
            "Filformat for nedlasting:",
            c("csv", "csv (nordisk)", "rds")
          )
        ),
        shiny::downloadButton(ns("download_report"), "Last ned rapport")
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("report"))
      )
    )
  )
}

#' @rdname mod_report
#' @export
report_server <- function(id, pool, pool_verify) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      df <- shiny::reactive({
        if (input$report == "") {
          data.frame()
        } else {
          if (input$report == "Registerstatus") {
            registry_status_report(pool, pool_verify)
          } else {
            data.frame()
          }
        }
      })

      output$download_report <- shiny::downloadHandler(
        filename = function() {
          if (input$file_format %in% c("csv", "csv (nordisk)")) {
            basename(tempfile(fileext = ".csv"))
          } else {
            basename(tempfile(fileext = ".rds"))
          }
        },
        content = function(file) {
          switch(input$file_format,
                 `csv` = readr::write_excel_csv(df(), file),
                 `csv (nordisk)` = readr::write_excel_csv2(df(), file),
                 `rds` = readr::write_rds(df(), file)
          )
        }
      )

      output$report_table <- DT::renderDataTable(
        DT::datatable(
          df(),
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

      output$report <- shiny::renderUI(
        DT::dataTableOutput(ns("report_table"))
      )
    }
  )
}

#' @rdname mod_report
#' @export
report_app <- function(pool, pool_verify) {

  ui <- shiny::fluidPage(
    report_ui("report")
  )

  server <- function(input, output, sessjon) {
    report_server("report", pool, pool_verify)
  }

  shiny::shinyApp(ui, server)
}
