#' Server logic for the imongr app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return An imongr shiny app server object
#' @export

app_server <- function(input, output, session) {

  # set max size of uploaded file to 50 Mb
  options(shiny.maxRequestSize = 50 * 1024^2)

  iusr <- get_user_name()
  igrs <- get_user_groups()
  conf <- get_config()
  pool <- make_pool()
  rv <- shiny::reactiveValues(inv_data = 0,
                              medfield_summary = character())
  known_user <- nrow(get_all_user_data(pool)) > 0
  valid_user <- nrow(get_user_data(pool)) > 0

  # if unknown, add user as pendig in imongr
  if (!known_user) {
    insert_table(pool, "user",
               data.frame(user_name = iusr, name = "", phone = "", email = "",
                          valid = 0))
  }

  # show/hide tabs by user profile
  shiny::hideTab("tabs", target = "upload")
  shiny::hideTab("tabs", target = "download")
  shiny::hideTab("tabs", target = "medfield")
  shiny::hideTab("tabs", target = "adminer")
  shiny::hideTab("tabs", target = "mine_field")
  if (valid_user && conf$role$provider %in% igrs) {
    shiny::showTab("tabs", target = "upload")
    shiny::showTab("tabs", target = "download")
  }
  if (valid_user && conf$role$manager %in% igrs) {
    shiny::showTab("tabs", target = "medfield")
    shiny::showTab("tabs", target = "adminer")
    shiny::showTab("tabs", target = "mine_field")
  }

  # app widget
  ## observers
  shiny::observeEvent(input$app_info,
    shinyalert::shinyalert(conf$app_text$info$title,
                           paste(version_info(),
                                 conf$app_text$info$help,
                                 conf$app_text$info$lisence,
                                 sep = "\n"),
                           type = "",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE,
                           confirmButtonText = no_opt_out_ok())
  )

  # profil
  output$profile <- shiny::renderText({
    profile_ui(conf, pool, valid_user, iusr, igrs)
  })

  output$deliveries_table <- DT::renderDataTable(
    if (input$deliver_history) {
      DT::datatable(get_user_deliveries(pool), rownames = FALSE,
                    options = list(dom = "tp", pageLength = 10,
                      language = list(
                        paginate = list(previous = "Forrige",
                                        `next` = "Neste"))))
    } else {
      NULL
    }
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
    insert_data(pool, df())
    insert_agg_data(pool, df())
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
    select_registry_ui(pool, conf, input_id = "registry")
  })

  output$upload_file <- shiny::renderUI({
    shiny::fileInput("upload_file", "Velg csv-fil",
                     buttonLabel = "Velg fil...",
                     placeholder = "Ingen fil er valgt",
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
    shiny::req(input$registry)
    if (input$registry == "") {
      NULL
    } else {
      upload_sample_text_ui(pool, conf, input$upload_file, input$registry,
                            indicators = unique(df()$ind_id))
    }
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

  output$valid_ind <- shiny::renderText({
    paste0("<h4>", conf$upload$doc$valid_ind, " <i>",
          get_registry_name(pool, shiny::req(input$registry),
                            full_name = TRUE),
          "</i>:</h4>")
  })

  output$valid_ind_tab <- shiny::renderTable(
    get_registry_indicators(pool, shiny::req(input$registry)), rownames = TRUE,
    colnames = FALSE
  )

  output$sample_data <- shiny::renderTable(
    get_table(pool, "data",
      sample = 0.00001)[conf$db$tab$data$insert[conf$upload$data_var_ind]]
  )



  # loss
  db_table <- shiny::reactive({
    if (input$download_registry == "") {
      data.frame()
    } else {
      if (input$tab_set == "data") {
        get_registry_data(pool, input$download_registry)
      } else if (input$tab_set == "ind") {
        get_registry_ind(pool, input$download_registry)
      } else {
        get_table(pool, input$tab_set)
      }
    }
  })

  output$select_download_registry <- shiny::renderUI({
    select_registry_ui(pool, conf, input_id = "download_registry")
  })

  output$select_db_table <- shiny::renderUI({
    shiny::selectInput("tab_set", "Velg tabell:", conf$download$tab,
                       selected = conf$download$tab[1])
  })

  output$download_db_table <- shiny::downloadHandler(
    filename = function() {
      if (input$file_format %in% c("csv", "csv (nordisk)", "excel-csv",
                                   "excel-csv (nordisk)")) {
        basename(tempfile(fileext = ".csv"))
      } else {
        basename(tempfile(fileext = ".rds"))
      }
    },
    content = function(file) {
      switch(input$file_format,
              `csv` = utils::write.csv(db_table(), file,
                                       fileEncoding = input$loss_enc,
                                       row.names = FALSE),
              `csv (nordisk)` = utils::write.csv2(db_table(), file,
                                                 fileEncoding = input$loss_enc,
                                                 row.names = FALSE),
              `excel-csv` = readr::write_excel_csv(db_table(), file),
              `excel-csv (nordisk)` = readr::write_excel_csv2(db_table(), file),
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
        )
      )
    )
  )

  output$ui_db_table <- shiny::renderUI(
    DT::dataTableOutput("db_table")
  )

  # registry medfields
  medfield_data <- get_table(pool, "medfield")

  rv$medfield_summary <- medfield_summary_text_ui(pool, conf, medfield_data)

  shiny::observeEvent(input$update_medfield, {
    registry_medfield_update <- data.frame(
      registry_id = rep(input$medfield_registry,
                        length(input$select_medfield)),
      medfield_id = input$select_medfield
    )
    update_registry_medfield(pool, registry_medfield_update)
    rv$medfield_summary <- medfield_summary_text_ui(pool, conf, medfield_data)
  })

  output$select_medfield_registry <- shiny::renderUI({
    select_registry_ui(pool, conf, input_id = "medfield_registry")
  })
  output$select_registry_medfield <- shiny::renderUI({
    shiny::req(input$medfield_registry)
    if (dim(medfield_data)[1] > 0) {
      all_medfield <- medfield_data$id
      names(all_medfield) <- medfield_data$name
    } else {
      all_medfield <- list(`Not defined!` = 0)
    }
    medfield <- get_registry_medfield(pool, input$medfield_registry)
    if (!is.null(dim(medfield))) {
      current_medfield <- medfield$medfield_id
      #names(current_medfield) <- medfield$name
    } else {
      current_medfield <- NULL
    }
    shiny::selectInput(inputId = "select_medfield",
                       label = "Velg fagområde(r):",
                       choices = all_medfield,
                       selected = current_medfield,
                       multiple = TRUE)
  })
  output$registry_medfield_header <- shiny::renderText({
    paste0("<h2>", conf$medfield$text$heading, " <i>",
           get_registry_name(pool, shiny::req(input$medfield_registry),
                             full_name = TRUE),
           "</i>:</h2><br>", conf$medfield$text$body)
  })
  output$registry_medfield_summary <- shiny::renderText({
    rv$medfield_summary
  })

  # our db admin interface
  admin_url <- paste0(adminer_url(), "/?",
                      "server=", db_host(), "&",
                      "username=", db_username(), "&",
                      "db=", db_name())

  output$admin_frame <- shiny::renderUI({
    shiny::tags$iframe(src = admin_url, width = "100%", height = 1024,
                       frameborder = "no")
  })

  # mine field
  shiny::observeEvent(input$agg_all, {
    withCallingHandlers({
      shinyjs::html("sysMessage", "")
      shinyjs::html("funMessage", "")
      shinyjs::html("funMessage", agg_all_data(pool))
    },
    message = function(m) {
      shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
    })
  })
  shiny::observeEvent(input$clean_agg, {
    withCallingHandlers({
      shinyjs::html("sysMessage", "")
      shinyjs::html("funMessage", "")
      shinyjs::html("funMessage", clean_agg_data(pool))
    },
    message = function(m) {
      shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
    })
  })

}
