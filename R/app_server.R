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

  # last
  ## reactive exp
  encoding <- shiny::reactive({
    if (input$enc == "Annet") {
      input$other_encoding
    } else {
      input$enc
    }
  })

  df <- shiny::reactive({
    if (is.null(input$upload_file)) {
      data.frame()
    } else {
      csv_to_df(input$upload_file$datapath, input$sep, input$dec_sep,
                encoding())
    }
  })


  ## ui sidebar panel
  output$select_registry <- shiny::renderUI({
    shiny::selectInput("registry", "Velg register:", get_user_registries(pool))
  })

  output$error_report <- shiny::renderText({
    if(is.null(input$upload_file)) {
      NULL
    } else {
      check_report(df(), pool)
    }
  })

  output$other_encoding <- shiny::renderUI({
    if (input$enc == "Annet") {
      shiny::selectInput(inputId = "other_encoding", label = "Spesifiser:",
                         choices = iconvlist(), selected = "MS-ANSI")
    }
  })

  output$submitt <- shiny::renderUI({
    if (all(!check_upload(df(), pool)$fail)) {
      actionButton("submitt", "Send til server", icon("paper-plane"))
    } else {
      NULL
    }
  })


  ## ui main panel
  output$upload_sample_text <- shiny::renderText({
    if (is.null(input$upload_file)) {
      NULL
    } else {
      conf$upload$doc$sample
    }
  })

  output$upload_sample <- shiny::renderTable({
    if(is.null(input$upload_file)) {
      NULL
    } else {
      sample_df(df(), input$sample_size, input$sample_type)
    }
  })

  output$var_doc <- shiny::renderText({
    l <- "<ul>\n"
    for (i in conf$upload$data_var_ind) {
      var <- conf$db$tab$data$insert[i]
      l <- paste0(l, "\t<li><b>", var, "</b>: ",
                  conf$upload$doc[[var]], "</li>\n")
    }
    l
  })

  output$sample_data <- shiny::renderTable(
    get_data(pool, sample = 0.0001)[conf$db$tab$data$insert[conf$upload$data_var_ind]]
  )




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
