#' Shiny module providing GUI and server logic for the publish indicator tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#' @param pool_verify A database pool object
#' @param tab_tracker String defining tab
#' @param registry_tracker Integer defining registry id
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_publish
#' @aliases publish_ui publish_server publish_app
NULL

#' @rdname mod_publish
#' @export
publish_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_publish_registry")),
        shiny::uiOutput(ns("publish_liability")),
        shinycssloaders::withSpinner(
          shiny::textOutput(ns("publishing")),
          color = "#18bc9c",
          color.background = "#ffffff",
          type = 7,
          proxy.height = 80
        ),
        shiny::uiOutput(ns("publish"))
      ),
      shiny::mainPanel(
        shiny::htmlOutput(ns("error_report_publish")),
        shiny::titlePanel("Publiser data"),
        shiny::h3("Kvalitetskontroll"),
        shiny::htmlOutput(ns("publish_verify_doc")),
        shiny::h3("Veiledning"),
        shiny::htmlOutput(ns("publish_main_doc")),
        shiny::p(shiny::em("Logg:")),
        shiny::verbatimTextOutput(ns("sysMessagePublish"))
      )
    )
  )
}

#' @rdname mod_publish
#' @export
publish_server <- function(id, tab_tracker, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conf <- get_config()

    # Reactive value to tie the app together and update renderings
    # The value is never used
    rv <- shiny::reactiveValues(
      inv_publish = 0
    )

    rv_return <- shiny::reactiveValues()

    ####################
    ##### Basic UI #####
    ####################

    # Registry selection drop down menu
    output$select_publish_registry <- shiny::renderUI({
      select_registry_ui(pool, conf,
        input_id = ns("publish_registry"),
        context = "verify", show_context = TRUE,
        pool0 = pool_verify,
        current_reg = registry_tracker$current_registry
      )
    })

    # Liability text next to checkbox
    output$publish_liability <- shiny::renderUI({
      shiny::checkboxInput(
        ns("liability"),
        shiny::HTML(paste(
          get_registry_name(pool_verify, input$publish_registry, TRUE),
          conf$publish$liability,
          as.character(shiny::actionLink(ns("view_terms"), "vilk\u00e5rene."))
        ))
      )
    })

    # Download terms handler
    output$downloadTerms <- shiny::downloadHandler(
      filename = basename(
        tempfile(pattern = "VilkaarPubliseringSKDE", fileext = ".html")
      ),
      content = function(file) {
        fn <- rmarkdown::render(
          input = system.file("terms.Rmd", package = "imongr"),
          output_format = "html_document"
        )
        file.rename(fn, file)
      }
    )

    # Loading animation
    output$publishing <- shiny::renderText({
      input$publish
      paste("")
    })

    # Main page text, heading "Kvalitetskontroll"
    output$publish_verify_doc <- shiny::renderText({
      verify_hypertext <- paste0(
        "<a href='https://verify.skde.no/behandlingskvalitet/",
        get_registry_name(pool_verify, shiny::req(input$publish_registry),
          full_name = FALSE
        ),
        "/' target='_blank'>her.</a>"
      )
      paste(
        get_registry_name(
          pool_verify,
          shiny::req(input$publish_registry),
          TRUE
        ),
        conf$publish$doc$verify,
        verify_hypertext
      )
    })

    # Heading "Veiledning"
    output$publish_main_doc <- shiny::renderText(conf$publish$doc$main)

    #####################################
    ##### Database getter functions #####
    #####################################

    publish_data <- shiny::reactive({
      rv$inv_publish
      if (is.null(input$publish_registry)) {
        data.frame()
      } else {
        get_registry_data(pool_verify, input$publish_registry)
      }
    })

    publish_ind <- shiny::reactive({
      rv$inv_publish
      if (is.null(input$publish_registry)) {
        data.frame()
      } else {
        get_registry_ind(pool_verify, input$publish_registry)
      }
    })

    ##############################################
    ##### Reactive value increment observers #####
    ##############################################

    # Reevaluate publish logic on selecting a registry
    shiny::observeEvent(input$publish_registry, {
      if (!is.null(input$publish_registry)) {
        rv$inv_publish <- rv$inv_publish + 1
        rv_return$registry_id <- input$publish_registry
      }
    })

    # Reevaluate publish logic on selecting the publish tab
    shiny::observeEvent(tab_tracker$current_tab, {
      if (tab_tracker$current_tab == "publish") {
        rv$inv_publish <- rv$inv_publish + 1
      }
    })

    # New window on view terms click
    shiny::observeEvent(input$view_terms, {
      f <- rmarkdown::render(
        input = system.file("terms.Rmd",
          package = "imongr"
        ),
        output_format = "html_fragment",
        output_file = tempfile()
      )
      shiny::showModal(shiny::modalDialog(
        shiny::HTML(readLines(f)),
        footer = shiny::tagList(
          shiny::downloadButton(ns("downloadTerms"), "Last ned vilk\u00e5r"),
          shiny::modalButton("Lukk")
        )
      ))
    })

    ##############################
    ##### Publish data logic #####
    ##############################

    # ui main panel
    output$error_report_publish <- shiny::renderText({
      shiny::req(input$publish_registry)
      rv$inv_publish
      error_report_ui(
        pool = pool,
        df = publish_data(),
        ind = publish_ind(),
        upload_file = "none",
        registry = input$publish_registry
      )
    })

    # Publish button
    output$publish <- shiny::renderUI({
      rv$inv_publish
      if (!is.null(input$publish_registry) &&
        !(conf$upload$fail %in% input$publish_registry) &&
        input$liability &&
        all(
          !check_upload(
            input$publish_registry,
            publish_data(),
            publish_ind(),
            pool
          )$fail
        )
      ) {
        shiny::tagList(
          shiny::actionButton(
            ns("publish"),
            "Publiser",
            shiny::icon("paper-plane"),
            style = conf$profile$action_button_style
          ),
          shiny::p(paste(
            conf$upload$doc$submit$warning,
            get_registry_name(pool, input$publish_registry)
          ))
        )
      } else {
        NULL
      }
    })

    # Publish new data
    shiny::observeEvent(input$publish, {

      withCallingHandlers(
        tryCatch(
          update_ind_text(pool, publish_ind()),
          error = function(e) {
            message(paste0("<font color=\"#FF0000\">", e$message, "</font><br>"))
          }
        ),

        message = function(m) {
          shinyjs::html(id = "sysMessagePublish", html = m$message, add = TRUE)
        }
      )

      withCallingHandlers(
        tryCatch(
          update_ind_val(pool, publish_ind()),
          error = function(e) {
            message(paste0("<font color=\"#FF0000\">", e$message, "</font><br>"))
          }
        ),

        message = function(m) {
          shinyjs::html(id = "sysMessagePublish", html = m$message, add = TRUE)
        }
      )

      withCallingHandlers(
        tryCatch(
          insert_data_prod(
            pool_verify = pool_verify,
            pool_prod = pool,
            df = publish_data(),
            terms_version = version_info(newline = "")
          ),

          error = function(e) {
            message(paste0("<font color=\"#FF0000\">", e$message, "</font><br>"))
          }
        ),

        message = function(m) {
          shinyjs::html(id = "sysMessagePublish", html = m$message, add = TRUE)
        }
      )

      withCallingHandlers(
        tryCatch(
          insert_agg_data(pool, publish_data()),
          error = function(e) {
            message(paste0("<font color=\"#FF0000\">", e$message, "</font>"))
          }
        ),

        message = function(m) {
          shinyjs::html(id = "sysMessagePublish", html = m$message, add = TRUE)
        }
      )

      withCallingHandlers(
        tryCatch(
          invalidate_cache(),
          error = function(e) {
            message(paste0("<font color=\"#FF0000\">", e$message, "</font>"))
          }
        ),

        message = function(m) {
          shinyjs::html(id = "sysMessagePublish", html = m$message, add = TRUE)
        }
      )

      rv$inv_publish <- rv$inv_publish + 1
      shinyalert::shinyalert(
        conf$publish$reciept$title, conf$publish$reciept$body,
        type = "success",
        showConfirmButton = FALSE, timer = 7000
      )


    })

    return(rv_return)
  })
}

#' @rdname mod_publish
#' @export
publish_app <- function(pool, pool_verify) {
  ui <- shiny::fluidPage(
    publish_ui("publish")
  )

  server <- function(input, output, sessjon) {
    publish_server("publish", NULL, pool, pool_verify)
  }

  shiny::shinyApp(ui, server)
}
