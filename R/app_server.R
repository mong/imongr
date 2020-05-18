#' Server logic for the imongr app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return An imongr shiny app server object
#' @export

app_server <- function(input, output, session) {

  conf <- get_config()
  pool <- make_pool()

  # loss
  db_table <- shiny::reactive({
    get_table(pool, input$tab_set)
  })

  output$select_db_table <- shiny::renderUI({
    shiny::selectInput("tab_set", "Velg tabell:", names(conf$db$tab),
                       selected = names(conf$db$tab)[1])
  })
  output$db_table <- DT::renderDataTable(
    db_table(), rownames = FALSE
  )

  output$ui_db_table <- shiny::renderUI(
    DT::dataTableOutput("db_table")
  )

  output$download_db_table <- shiny::downloadHandler(
    filename = function() {
      if (input$file_format %in% c("csv", "csv2", "excel-csv", "excel-csv2")) {
        basename(tempfile(fileext = ".csv"))
      } else {
        basename(tempfile(fileext = ".rds"))
      }
    },
    content = function(file) {
      switch (input$file_format,
        `csv` = readr::write_csv(db_table(), file),
        `csv2` = readr::write_csv2(db_table(), file),
        `excel-csv` = readr::write_excel_csv(db_table(), file),
        `excel-csv2` = readr::write_excel_csv2(db_table(), file),
        `rds` = readr::write_rds(db_table(), file)
      )
    }
  )

  # our db admin interface
  admin_url <- paste0(adminer_url(), "/?",
                      "server=", db_host(), "&",
                      "username=", db_username(), "&",
                      "db=", db_name())

  output$admin_frame <- shiny::renderUI({
    shiny::tags$iframe(src = admin_url, width = "100%", height = 1024,
                       frameborder = "no")
  })

}
