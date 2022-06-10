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
  pool_verify <- make_pool(context = "verify")
  oversize_message <- "<i style='color:red;'>Teksten er for lang!</i><br><br>"
  rv <- shiny::reactiveValues(
    context = "verify",
    inv_data = 0,
    inv_publish = 0,
    medfield_data = get_table(pool, "medfield"),
    medfield_summary = medfield_summary_text_ui(pool, conf,
                                                get_table(pool, "medfield")),
    user_data = get_users(pool),
    user_summary =
      reguser_summary_text_ui(pool, conf, get_users(pool)),
    upload_reg = character(),
    publish_reg = character(),
    download_reg = character(),
    indicator_reg = character(),
    indicator_data = data.frame(),
    title_oversize = FALSE,
    short_oversize = FALSE,
    long_oversize = FALSE,
    pool = make_pool(context = "verify"),
    admin_url = paste0(adminer_url(), "/?",
                        "server=", db_host(context = "verify"), "&",
                        "username=", db_username(), "&",
                        "db=", db_name())
  )

  # always from default db, never selectable by user
  known_user <- nrow(get_all_user_data(pool)) > 0
  valid_user <- nrow(get_user_data(pool)) > 0

  # if unknown, add user as pending in default db
  if (!known_user) {
    insert_table(
      pool, "user",
      data.frame(
        user_name = iusr, name = "", phone = "", email = "", valid = 0)
      )
  }

  # show/hide tabs by user profile
  shiny::hideTab("tabs", target = "upload")
  shiny::hideTab("tabs", target = "publish")
  shiny::hideTab("tabs", target = "download")
  shiny::hideTab("tabs", target = "indicator")
  shiny::hideTab("tabs", target = "Administrative verkt\u00f8y")
  shiny::hideTab("tabs", target = "settings")
  shiny::hideTab("tabs", target = "medfield")
  shiny::hideTab("tabs", target = "reguser")
  shiny::hideTab("tabs", target = "adminer")
  shiny::hideTab("tabs", target = "mine_field")
  if (valid_user && conf$role$provider %in% igrs) {
    shiny::showTab("tabs", target = "upload")
    shiny::showTab("tabs", target = "download")
    shiny::showTab("tabs", target = "indicator")
  }
  if (valid_user && conf$role$manager %in% igrs) {
    shiny::showTab("tabs", target = "Administrative verkt\u00f8y")
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

  shiny::observeEvent(input$context, {
    rv$context <- input$context
    drain_pool(rv$pool)
    rv$upload_reg <- input$registry
    rv$download_reg <- input$download_registry
    rv$indicator_reg <- input$indicator_registry
    rv$admin_url <- paste0(adminer_url(), "/?",
                           "server=", db_host(context = rv$context), "&",
                           "username=", db_username(), "&",
                           "db=", db_name())
    rv$pool <- make_pool(context = rv$context)
    rv$medfield_data <- get_table(rv$pool, "medfield")
    rv$medfield_summary <-
      medfield_summary_text_ui(rv$pool, conf, get_table(rv$pool, "medfield"))
    rv$user_data <- get_users(rv$pool)
    rv$user_summary <-
      reguser_summary_text_ui(rv$pool, conf, get_users(rv$pool))
    rv$inv_data <- rv$inv_data + 1
  })


  # profil
  output$profile <- shiny::renderText({
    profile_ui(conf, pool, valid_user, iusr, igrs)
  })

  output$upload_table <- DT::renderDataTable(
    if (input$upload_history) {
      DT::datatable(get_user_deliveries(pool_verify), rownames = FALSE,
                    options = list(dom = "tp", pageLength = 10,
                      language = list(
                        paginate = list(previous = "Forrige",
                                        `next` = "Neste"))))
    } else {
      NULL
    }
  )

  output$publish_table <- DT::renderDataTable(
    if (input$publish_history) {
      DT::datatable(get_user_deliveries(pool), rownames = FALSE,
                    options = list(dom = "tp", pageLength = 10,
                                   language = list(
                                     paginate = list(previous = "Forrige",
                                                     `next` = "Neste"))))
    } else {
      NULL
    }
  )

  output$ui_upload_table <- shiny::renderUI(
    DT::dataTableOutput("upload_table")
  )

  output$ui_publish_table <- shiny::renderUI(
    DT::dataTableOutput("publish_table")
  )

  # last
  ## observers
  shiny::observeEvent(input$registry, {
    if (!is.null(input$upload_file)) {
      rv$inv_data <- rv$inv_data + 1
    }
  })
  shiny::observeEvent(input$submit, {
    insert_data(
      pool = pool_verify,
      df = df(),
      update = input$latest_update,
      affirm = input$latest_affirm
    )
    insert_agg_data(pool_verify, df())
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
    select_registry_ui(pool_verify, conf, input_id = "registry",
                       context = "verify", current_reg = rv$upload_reg)
  })

  output$upload_file <- shiny::renderUI({
    shiny::fileInput("upload_file", "Velg csv-fil:",
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
    submit_ui(conf, pool_verify, input$upload_file, input$registry, df(),
              "verify")
  })

  output$spinner <- shiny::renderText({
    input$submit
    paste("")
  })


  ## ui main panel
  output$error_report <- shiny::renderText({
    rv$inv_data
    error_report_ui(pool_verify, df(), input$upload_file, input$registry)
  })

  output$upload_sample_text <- shiny::renderText({
    shiny::req(input$registry)
    if (input$registry == "") {
      NULL
    } else {
      upload_sample_text_ui(pool_verify, conf, input$upload_file,
                            input$registry, indicators = unique(df()$ind_id))
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
          get_registry_name(pool_verify, shiny::req(input$registry),
                            full_name = TRUE),
          "</i>:</h4>")
  })

  output$valid_ind_tab <- shiny::renderTable(
    get_registry_indicators(pool_verify, shiny::req(input$registry)),
    rownames = TRUE,
    colnames = FALSE
  )

  output$sample_data <- shiny::renderTable(
    get_table(pool_verify, "data",
      sample = 0.00001)[conf$db$tab$data$insert[conf$upload$data_var_ind]]
  )


  # publish
  ## observers
  shiny::observeEvent(input$view_terms, {
    f <- rmarkdown::render(input = system.file("terms.Rmd", package = "imongr"),
                           output_format = "html_fragment",
                           output_file = tempfile())
    shiny::showModal(shiny::modalDialog(
      shiny::HTML(readLines(f)),
      footer = shiny::tagList(
        shiny::downloadButton("downloadTerms", "Last ned vilk\u00e5r"),
        shiny::modalButton("Lukk")
      )
    ))
  })
  shiny::observeEvent(input$publish_registry, {
    if (!is.null(input$publish_registry)) {
      rv$inv_publish <- rv$inv_publish + 1
    }
  })
  shiny::observeEvent(input$publish, {
    update_ind_text(pool, publish_ind())
    insert_data(
      pool = pool,
      df = publish_data(),
      update = publish_delivery()$latest_update,
      affirm = publish_delivery()$latest_affirm,
      terms_version = version_info(newline = "")
    )
    insert_agg_data(pool, publish_data())
    rv$inv_publish <- rv$inv_publish + 1
    shinyalert::shinyalert(
      conf$publish$reciept$title, conf$publish$reciept$body, type = "success",
      showConfirmButton = FALSE, timer = 7000
    )
  })
  ## reactives
  publish_data <- shiny::reactive({
    if (is.null(input$publish_registry)) {
      data.frame()
    } else {
      get_registry_data(pool_verify, input$publish_registry)
    }
  })
  publish_ind <- shiny::reactive({
    if (is.null(input$publish_registry)) {
      data.frame()
    } else {
      get_registry_ind(pool_verify, input$publish_registry)
    }
  })
  publish_delivery <- shiny::reactive({
    if (is.null(input$publish_registry)) {
      data.frame()
    } else {
      get_registry_latest_delivery(pool_verify, input$publish_registry)
    }
  })

  ## ui sidebar panel
  output$select_publish_registry <- shiny::renderUI({
    select_registry_ui(pool, conf, input_id = "publish_registry",
                       context = "verify", show_context = TRUE,
                       pool0 = pool_verify)
  })
  output$publish_liability <- shiny::renderUI({
    shiny::checkboxInput(
      "liability",
      shiny::HTML(paste(
        get_registry_name(pool_verify, input$publish_registry, TRUE),
        conf$publish$liability,
        as.character(shiny::actionLink("view_terms", "vilk\u00e5rene."))
      ))
    )
  })
  output$downloadTerms <- shiny::downloadHandler(
    filename = basename(
      tempfile(pattern = "VilkaarPubliseringSKDE", fileext = ".pdf")
    ),
    content = function(file) {
      fn <- rmarkdown::render(
        input = system.file("terms.Rmd", package = "imongr"),
        output_format = "pdf_document",
        params = list(output = "pdf")
      )
      file.rename(fn, file)
    }
  )
  output$publish <- shiny::renderUI({
    rv$inv_publish
    if (!is.null(input$publish_registry) &&
        !(conf$upload$fail %in% input$publish_registry) &&
        all(!check_upload(input$publish_registry, publish_data(), pool)$fail) &&
        input$liability) {
      shiny::tagList(
        shiny::actionButton("publish", "Publiser", shiny::icon("paper-plane")),
        shiny::p(paste(conf$upload$doc$submit$warning,
                       get_registry_name(pool, input$publish_registry)))
      )
    } else {
      NULL
    }
  })
  output$publishing <- shiny::renderText({
    input$publish
    paste("")
  })

  ## ui main panel
  output$error_report_publish <- shiny::renderText({
    shiny::req(input$publish_registry)
    rv$inv_publish
    error_report_ui(
      pool = pool,
      df = publish_data(),
      upload_file = "none",
      registry = input$publish_registry
    )
  })
  output$publish_verify_doc <- shiny::renderText({
    verify_hypertext <- paste0(
      "<a href='https://verify.skde.no/kvalitetsregistre/",
      get_registry_name(pool_verify, shiny::req(input$publish_registry),
                        full_name = FALSE),
      "/sykehus'>her.</a>"
    )
    paste(
      get_registry_name(pool_verify, shiny::req(input$publish_registry), TRUE),
      conf$publish$doc$verify,
      verify_hypertext
    )
  })
  output$publish_main_doc <- shiny::renderText(conf$publish$doc$main)

  # loss
  pool_download <- shiny::reactive({
    if (is.null(input$download_context)) {
      pool_verify
    } else {
      if(input$download_context == "verify") {
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
      } else {
        get_table(pool_download(), input$tab_set)
      }
    }
  })

  output$select_download_registry <- shiny::renderUI({
    select_registry_ui(pool_download(), conf, input_id = "download_registry",
                       context = input$download_context,
                       show_context = FALSE)
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



  # indicator
  ## observers
  shiny::observeEvent(input$indicator, {
    rv$ind_data <- get_registry_ind(pool_verify, input$indicator_registry) %>%
      dplyr::filter(.data$id == input$indicator)
  })

  shiny::observeEvent(input$ind_title, {
    if (nchar(input$ind_title) > 255) {
      rv$title_oversize <- TRUE
    } else {
      rv$title_oversize <- FALSE
    }
  })

  shiny::observeEvent(input$ind_short, {
    if (nchar(input$ind_short) > 1023) {
      rv$short_oversize <- TRUE
    } else {
      rv$short_oversize <- FALSE
    }
  })

  shiny::observeEvent(input$ind_long, {
    if (nchar(input$ind_long) > 2047) {
      rv$long_oversize <- TRUE
    } else {
      rv$long_oversize <- FALSE
    }
  })

  shiny::observeEvent(input$update_ind, {
    rv$ind_data$title <- input$ind_title
    rv$ind_data$short_description <- input$ind_short
    rv$ind_data$long_description <- input$ind_long
    update_ind_text(pool_verify, rv$ind_data)
    rv$ind_data <- get_registry_ind(pool_verify, input$indicator_registry) %>%
      dplyr::filter(.data$id == input$indicator)
  })

  output$select_indicator_registry <- shiny::renderUI({
    select_registry_ui(pool_verify, conf, input_id = "indicator_registry",
                       context = "verify", current_reg = rv$indicator_reg,
                       show_context = TRUE)
  })

  output$select_indicator <- shiny::renderUI({
    shiny::req(input$indicator_registry)
    shiny::selectInput(
      "indicator", "Velg indikator:",
      choices = get_registry_indicators(pool_verify, input$indicator_registry)
    )
  })

  output$edit_ind_title <- shiny::renderUI({
    shiny::req(input$indicator)
    shiny::textAreaInput(
      "ind_title", "Indikatortittel (maks 255 tegn)",
      value = rv$ind_data$title, width = "90%", rows = 2
    )
  })

  output$title_oversize <- shiny::renderUI({
    if (rv$title_oversize) {
      shiny::HTML(oversize_message)
    } else {
      NULL
    }
  })

  output$edit_ind_short <- shiny::renderUI({
    shiny::req(input$indicator)
    shiny::textAreaInput(
      "ind_short", "Kort indikatorbeskrivelse (maks 1023 tegn)",
      value = rv$ind_data$short_description, width = "90%", rows = 8
    )
  })

  output$short_oversize <- shiny::renderUI({
    if (rv$short_oversize) {
      shiny::HTML(oversize_message)
    } else {
      NULL
    }
  })

  output$edit_ind_long <- shiny::renderUI({
    shiny::req(input$indicator)
    shiny::textAreaInput(
      "ind_long", "Lang indikatorbeskrivelse (maks 2047 tegn)",
      value = rv$ind_data$long_description, width = "90%", rows = 16
    )
  })

  output$long_oversize <- shiny::renderUI({
    if (rv$long_oversize) {
      shiny::HTML(oversize_message)
    } else {
      NULL
    }
  })

  output$update_indicator <- shiny::renderUI({
    if (any(c(rv$title_oversize, rv$short_oversize, rv$long_oversize))) {
      NULL
    } else {
      shiny::actionButton("update_ind", "Oppdat\u00e9r tekster")
    }
  })


  # manager settings
  output$select_context <- shiny::renderUI({
    if (valid_user) {
      shiny::selectInput("context", "Velg milj\u00f8:",
                         choices = list(Produksjon = "prod",
                                        Kvalitetskontroll = "verify",
                                        QA = "qa"),
                         selected = "verify")
    } else {
      NULL
    }
  })


  # registry medfields
  shiny::observeEvent(input$update_medfield, {
    registry_medfield_update <- data.frame(
      registry_id = rep(input$medfield_registry,
                        length(input$select_medfield)),
      medfield_id = input$select_medfield
    )
    update_registry_medfield(rv$pool, registry_medfield_update)
    rv$medfield_summary <-
      medfield_summary_text_ui(rv$pool, conf, rv$medfield_data)
  })

  output$select_medfield_registry <- shiny::renderUI({
    select_registry_ui(rv$pool, conf, input_id = "medfield_registry",
                       context = rv$context)
  })
  output$select_registry_medfield <- shiny::renderUI({
    shiny::req(input$medfield_registry)
    if (dim(rv$medfield_data)[1] > 0) {
      all_medfield <- rv$medfield_data$id
      names(all_medfield) <- rv$medfield_data$name
    } else {
      all_medfield <- list(`Not defined!` = 0)
    }
    medfield <- get_registry_medfield(rv$pool, input$medfield_registry)
    if (!is.null(dim(medfield))) {
      current_medfield <- medfield$medfield_id
    } else {
      current_medfield <- NULL
    }
    shiny::selectInput(inputId = "select_medfield",
                       label = "Velg fagomr\u00e5de(r):",
                       choices = all_medfield,
                       selected = current_medfield,
                       multiple = TRUE)
  })
  output$registry_medfield_header <- shiny::renderText({
    paste0("<h2>", conf$medfield$text$heading, " <i>",
           get_registry_name(rv$pool, shiny::req(input$medfield_registry),
                             full_name = TRUE),
           "</i>:</h2><br>", conf$medfield$text$body)
  })
  output$registry_medfield_summary <- shiny::renderText({
    rv$medfield_summary
  })

  # user registries
  shiny::observeEvent(input$update_reguser, {
    registry_user_update <- data.frame(
      registry_id = rep(input$user_registry,
                        length(input$select_user)),
      user_id = input$select_user
    )
    update_registry_user(rv$pool, registry_user_update)
    rv$user_summary <-
      reguser_summary_text_ui(rv$pool, conf, rv$user_data)
  })

  output$select_user_registry <- shiny::renderUI({
    select_registry_ui(rv$pool, conf, input_id = "user_registry",
                       context = rv$context)
  })
  output$select_registry_user <- shiny::renderUI({
    shiny::req(input$user_registry)
    if (dim(rv$user_data)[1] > 0) {
      all_user <- rv$user_data$id
      names(all_user) <- rv$user_data$user_name
    } else {
      all_user <- list(`Not defined!` = 0)
    }
    reguser <- get_registry_user(rv$pool, input$user_registry)
    if (!is.null(dim(reguser))) {
      current_reguser <- reguser$user_id
    } else {
      current_reguser <- NULL
    }
    shiny::selectInput(inputId = "select_user",
                       label = "Velg bruker(e):",
                       choices = all_user,
                       selected = current_reguser,
                       multiple = TRUE)
  })
  output$registry_user_header <- shiny::renderText({
    paste0("<h2>", conf$reguser$text$heading, " <i>",
           get_registry_name(rv$pool, shiny::req(input$user_registry),
                             full_name = TRUE),
           "</i>:</h2><br>", conf$reguser$text$body)
  })
  output$registry_user_summary <- shiny::renderText({
    rv$user_summary
  })

  # our db admin interface
  output$admin_frame <- shiny::renderUI({
    shiny::tags$iframe(src = rv$admin_url, width = "100%", height = 1024,
                       frameborder = "no")
  })

  # mine field
  shiny::observeEvent(input$agg_all, {
    withCallingHandlers({
      shinyjs::html("sysMessage", "")
      shinyjs::html("funMessage", "")
      shinyjs::html("funMessage", agg_all_data(rv$pool))
    },
    message = function(m) {
      shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
    })
  })
  shiny::observeEvent(input$clean_agg, {
    withCallingHandlers({
      shinyjs::html("sysMessage", "")
      shinyjs::html("funMessage", "")
      shinyjs::html("funMessage", clean_agg_data(rv$pool))
    },
    message = function(m) {
      shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
    })
  })
  output$mine_field_uc <- shiny::renderUI({
    shiny::tagList(
      shiny::HTML(
        paste0("<h3 style='color:",
               switch(rv$context,
                      prod = "green;'>Produksjon</h3>",
                      verify = "orange;'>Kvalitetskontroll</h3>",
                      qa = "red;'>QA</h3>"))
      ),
      shiny::p("Tr\u00e5 forsiktig!"),
      shiny::actionButton("agg_all", "Aggreger alle data",
                          icon = shiny::icon("skull")),
      shiny::hr(),
      shiny::actionButton("clean_agg", "Rydd aggregerte data")
    )
  })
  # Heartbeat every 5 seconds, to avoid app to die when user is inactive.
  output$clock <- shiny::renderText({
   shiny::invalidateLater(5000)
    Sys.time()
  })
}
