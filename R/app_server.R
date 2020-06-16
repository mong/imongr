#' Server logic for the imongr app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return An imongr shiny app server object
#' @export

app_server <- function(input, output, session) {

  iusr <- get_user_name()
  igrs <- get_user_groups()
  conf <- get_config()
  pool <- make_pool()
  rv <- shiny::reactiveValues(inv_data = 0)
  known_user <- nrow(get_all_user_data(pool)) > 0
  valid_user <- nrow(get_user_data(pool)) > 0

  # if unknown, add user as pendig in imongr
  if (!known_user) {
    insert_tab(pool, "user",
               data.frame(user_name = iusr, name = "", phone = "", email = "",
                          valid = 0))
  }

  # show/hide tabs by user profile
  shiny::hideTab("tabs", target = "Last")
  shiny::hideTab("tabs", target = "Loss")
  shiny::hideTab("tabs", target = "Sj\u00e6f")
  if (valid_user && conf$role$provider %in% igrs) {
    shiny::showTab("tabs", target = "Last")
    shiny::showTab("tabs", target = "Loss")
  }
  if (valid_user && conf$role$manager %in% igrs) {
    shiny::showTab("tabs", target = "Sj\u00e6f")
  }

  # app widget
  ## observers
  shiny::observeEvent(input$app_info,
    shinyalert::shinyalert(conf$app_text$info$title,
                           paste(version_info(),
                                 conf$app_text$info$help,
                                 conf$app_text$info$lisence,
                                 sep = "\n"),
                           type = "", imageUrl = "www/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE,
                           confirmButtonText = no_opt_out_ok())
  )

  # profil
  output$profile <- shiny::renderText({
    profile_ui(conf, pool, valid_user, iusr, igrs)
  })

  output$deliveries_table <- DT::renderDataTable(
    DT::datatable(get_user_deliveries(pool), rownames = FALSE,
                  options = list(dom = "tp", pageLength = 10, language = list(
                                 paginate = list(previous = "Forrige",
                                                 `next` = "Neste"))))
  )

  output$ui_deliveries_table <- shiny::renderUI(
    DT::dataTableOutput("deliveries_table")
  )

  # last
  ## observers
  shiny::observeEvent(input$registry, {
    if (!is.null(input$upload_file)) {
      rv$inv_data <- rv$inv_data + 1
    }
  })
  shiny::observeEvent(input$submit, {
    insert_data(pool, cbind(df(), data.frame(Register = input$registry)))
    rv$inv_data <- rv$inv_data + 1
    shinyalert::shinyalert(conf$upload$reciept$title, conf$upload$reciept$body,
                           type = "success", showConfirmButton = FALSE,
                           timer = 7000)
  })

  ## reactive exps
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
    select_registry_ui(conf, pool)
  })

  output$upload_file <- shiny::renderUI({
    shiny::fileInput("upload_file", "Velg csv-fil",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
    )
  })

  output$other_encoding <- shiny::renderUI({
    if (input$enc == "Annet") {
      shiny::selectInput(inputId = "other_encoding", label = "Spesifiser:",
                         choices = iconvlist(), selected = "MS-ANSI")
    }
  })

  output$submit <- shiny::renderUI({
    rv$inv_data
    submit_ui(conf, pool, input$upload_file, input$registry, df())
  })

  output$spinner <- shiny::renderText({
    input$submit
    paste("")
  })


  ## ui main panel
  output$error_report <- shiny::renderText({
    rv$inv_data
    error_report_ui(pool, df(), input$upload_file, input$registry)
  })

  output$upload_sample_text <- shiny::renderText({
    upload_sample_text_ui(conf, input$upload_file)
  })

  output$upload_sample <- shiny::renderTable({
    rv$inv_data
    upload_sample_ui(df(), input$upload_file, input$registry,
                     input$sample_size, input$sample_type)
  })

  output$main_doc <- shiny::renderText(conf$upload$doc$main)

  output$var_doc <- shiny::renderText({
    var_doc_ui(conf)
  })

  output$sample_data <- shiny::renderTable(
    get_data(pool,
             sample = 0.0001)[conf$db$tab$data$insert[conf$upload$data_var_ind]]
  )



  # loss
  db_table <- shiny::reactive({
    get_registry_data(pool, input$download_registry)
  })

  output$select_download_registry <- shiny::renderUI({
    regs <- get_user_registries(pool)
    if (length(regs) == 0) {
      regs <- c(conf$upload$fail)
    }
    shiny::selectInput("download_registry", "Velg register:", regs,
                       selected = regs[1])
  })

  output$select_db_table <- shiny::renderUI({
    shiny::selectInput("tab_set", "Velg tabell:", conf$download$tab,
                       selected = conf$download$tab[1])
  })

  output$download_db_table <- shiny::downloadHandler(
    filename = function() {
      if (input$file_format %in% c("csv", "csv2", "excel-csv", "excel-csv2")) {
        basename(tempfile(fileext = ".csv"))
      } else {
        basename(tempfile(fileext = ".rds"))
      }
    },
    content = function(file) {
      switch(input$file_format,
              `csv` = readr::write_csv(db_table(), file),
              `csv2` = readr::write_csv2(db_table(), file),
              `excel-csv` = readr::write_excel_csv(db_table(), file),
              `excel-csv2` = readr::write_excel_csv2(db_table(), file),
              `rds` = readr::write_rds(db_table(), file)
      )
    }
  )

  output$db_table <- DT::renderDataTable(
    DT::datatable(db_table(), rownames = FALSE,
                 options = list(
                   dom = "lftp",
                   language = list(
                     lengthMenu = "Vis _MENU_ rader per side",
                     search = "S\u00f8k:",
                     info = "Rad _START_ til _END_ av totalt _TOTAL_",
                     paginate = list(previous = "Forrige", `next` = "Neste")
                   )))
  )

  output$ui_db_table <- shiny::renderUI(
    DT::dataTableOutput("db_table")
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
