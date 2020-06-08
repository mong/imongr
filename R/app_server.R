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

  # show/hide tabs by user profile
  known_user <- nrow(get_user_data(pool)) > 0
  shiny::hideTab("tabs", target = "Last")
  shiny::hideTab("tabs", target = "Loss")
  shiny::hideTab("tabs", target = "Sj\u00e6f")
  if (known_user && "provider" %in% igrs) {
    shiny::showTab("tabs", target = "Last")
    shiny::showTab("tabs", target = "Loss")
  }
  if (known_user && "manager" %in% igrs) {
    shiny::showTab("tabs", target = "Sj\u00e6f")
  }

  # profil
  output$profile <- shiny::renderText({
    if (!known_user || "none" %in% igrs) {
      paste(conf$profile$pending, "<br>",
            "SHINYPROXY_USERNAME:", get_user_name(), "<br>",
            "SHINYPROXY_USERGROUPS:", paste(get_user_groups(),
                                            collapse = ", "))
    } else {
      df <- get_user_data(pool)
      paste(conf$profile$greeting, "<b>", iusr, "</b>", "<br>",
            conf$profile$userinfo, "<br>",
            "Navn:", df$name, "<br>",
            "Telefon:", df$phone, "<br>",
            "e-post:", df$email, "<br><br>",
            conf$profile$howto, "<br><br>",
            "SHINYPROXY_USERNAME:", get_user_name(), "<br>",
            "SHINYPROXY_USERGROUPS:", paste(get_user_groups(),
                                            collapse = ", "))
    }
  })

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
    regs <- get_user_registries(pool)
    shiny::selectInput("registry", "Velg register:", regs, selected = regs[1])
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
    if (!is.null(input$upload_file)) {
      if (all(!check_upload(cbind(df(), Register = input$registry),
                            pool)$fail)) {
        actionButton("submit", "Send til server", icon("paper-plane"))
      } else {
        NULL
      }
    } else {
      NULL
    }
  })


  ## ui main panel
  output$in_progress <- shiny::renderUI(NULL)

  output$error_report <- shiny::renderText({
    rv$inv_data
    if(is.null(input$upload_file)) {
      NULL
    } else {
      check_report(cbind(df(), Register = input$registry), pool)
    }
  })

  output$upload_sample_text <- shiny::renderText({
    if (is.null(input$upload_file)) {
      NULL
    } else {
      conf$upload$doc$sample
    }
  })

  output$upload_sample <- shiny::renderTable({
    rv$inv_data
    if(is.null(input$upload_file)) {
      NULL
    } else {
      sample_df(df = df(), skip = c(input$registry), n = input$sample_size,
                random = input$sample_type)
    }
  })

  output$main_doc <- shiny::renderText(conf$upload$doc$main)

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
