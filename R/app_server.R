#' Server logic for the imongr app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return An imongr shiny app server object
#' @export

app_server <- function(input, output, session) {
  message("Starting app")
  # set max size of uploaded file to 50 Mb
  options(shiny.maxRequestSize = 50 * 1024^2)

  iusr <- get_user_name()
  igrs <- get_user_groups()
  conf <- get_config()
  pool <- make_pool()
  pool_verify <- make_pool(context = "verify")
  rv <- shiny::reactiveValues(
    context = "verify",
    medfield_data = get_table(pool, "medfield"),
    medfield_summary = medfield_summary_text_ui(
      pool, conf,
      get_table(pool, "medfield")
    ),
    user_data = get_users(pool),
    user_summary =
      reguser_summary_text_ui(pool, conf, get_users(pool)),
    publish_reg = character(),
    download_reg = character(),
    indicator_data = data.frame(),
    pool = make_pool(context = "verify"),
    admin_url = paste0(
      adminer_url(), "/?",
      "server=", db_host(context = "verify"), "&",
      "username=", db_username(), "&",
      "db=", db_name()
    )
  )

  # Tab tracker for linking modules
  tab_tracker <- shiny::reactiveValues()

  shiny::observeEvent(input$tabs, {
    tab_tracker$previous_tab <- tab_tracker$current_tab
    tab_tracker$current_tab <- input$tabs
  })

  # Registry tracker for linking modules
  registry_tracker <- shiny::reactiveValues()

  # always from default db, never selectable by user
  known_user <- nrow(get_all_user_data(pool)) > 0
  valid_user <- nrow(get_user_data(pool)) > 0

  # if unknown, add user as pending in default db
  if (!known_user) {
    insert_table(
      pool, "user",
      data.frame(
        user_name = iusr,
        name = "",
        phone = "",
        email = iusr,
        valid = 0
      )
    )
    insert_table(
      pool_verify, "user",
      data.frame(
        user_name = iusr,
        name = "",
        phone = "",
        email = iusr,
        valid = 0
      )
    )
  }

  # Find out which roles the user has
  is_provider <- valid_user && conf$role$provider %in% igrs
  is_manager <- valid_user && conf$role$manager %in% igrs
  is_reviewer <- valid_user && conf$role$reviewer %in% igrs

  roles <- c(is_provider, is_reviewer, is_manager)

  # Make a vector of functions to display tabs based on roles
  show_provider <- function() {
    shiny::showTab("tabs", target = "upload")
    shiny::showTab("tabs", target = "publish")
    shiny::showTab("tabs", target = "download")
    shiny::showTab("tabs", target = "indicator")
  }

  show_reviewer <- function() {
    shiny::showTab("tabs", target = "review")
  }

  show_manager <- function() {
    shiny::showTab("tabs", target = "Administrative verkt\u00f8y")
  }

  show_tabs <- c(show_provider, show_reviewer, show_manager)

  # Hide all tabs
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
  shiny::hideTab("tabs", target = "report")
  shiny::hideTab("tabs", target = "status")
  shiny::hideTab("tabs", target = "review")

  # Show the tabs that are appropriate for the user's roles
  lapply(show_tabs[which(roles)], FUN = function(fun) {
    fun()
  })

  # clean up when app ends
  shiny::onStop(
    function() {
      drain_pool(pool)
      drain_pool(pool_verify)
    },
    session = NULL
  )

  # app widget
  ## observers
  shiny::observeEvent(
    input$app_info,
    shinyalert::shinyalert(conf$app_text$info$title,
      paste(version_info(),
        conf$app_text$info$help,
        conf$app_text$info$lisence,
        sep = "\n"
      ),
      type = "",
      closeOnEsc = TRUE, closeOnClickOutside = TRUE,
      html = TRUE,
      confirmButtonText = no_opt_out_ok()
    )
  )

  shiny::observeEvent(input$context, {
    rv$context <- input$context
    drain_pool(rv$pool)
    rv$download_reg <- input$download_registry
    rv$admin_url <- paste0(
      adminer_url(), "/?",
      "server=", db_host(context = rv$context), "&",
      "username=", db_username(), "&",
      "db=", db_name()
    )
    rv$pool <- make_pool(context = rv$context)
    rv$medfield_data <- get_table(rv$pool, "medfield")
    rv$medfield_summary <-
      medfield_summary_text_ui(rv$pool, conf, get_table(rv$pool, "medfield"))
    rv$user_data <- get_users(rv$pool)
    rv$user_summary <-
      reguser_summary_text_ui(rv$pool, conf, get_users(rv$pool))
  })


  ##### Tabs #####

  # profile
  profile_server("profile", pool, pool_verify)


  # last
  rv_upload <- upload_server("upload", registry_tracker, pool_verify)

  shiny::observeEvent(rv_upload$registry_id, {
    registry_tracker$current_registry <- rv_upload$registry_id
  })


  # publish
  rv_publish <- publish_server("publ", tab_tracker, registry_tracker, pool, pool_verify)

  shiny::observeEvent(rv_publish$registry_id, {
    registry_tracker$current_registry <- rv_publish$registry_id
  })


  # loss
  rv_download <- download_server("download", registry_tracker, pool, pool_verify)

  shiny::observeEvent(rv_download$registry_id, {
    registry_tracker$current_registry <- rv_download$registry_id
  })


  # indicator
  rv_indicator <- indicator_server("ind", registry_tracker, pool, pool_verify)

  shiny::observeEvent(rv_indicator$registry_id, {
    registry_tracker$current_registry <- rv_indicator$registry_id
  })

  rv_review <- review_server("review", registry_tracker, pool)

  shiny::observeEvent(rv_review$registry_id, {
    registry_tracker$current_registry <- rv_review$registry_id
  })

  ##### Admin #####

  # manager settings
  output$select_context <- shiny::renderUI({
    if (valid_user) {
      shiny::selectInput("context", "Velg milj\u00f8:",
        choices = list(
          Produksjon = "prod",
          Kvalitetskontroll = "verify",
          QA = "qa"
        ),
        selected = "verify"
      )
    } else {
      NULL
    }
  })

  # registry medfields
  shiny::observeEvent(input$update_medfield, {
    registry_medfield_update <- data.frame(
      registry_id = rep(
        input$medfield_registry,
        length(input$select_medfield)
      ),
      medfield_id = input$select_medfield
    )
    update_registry_medfield(rv$pool, input$medfield_registry, registry_medfield_update)
    rv$medfield_summary <-
      medfield_summary_text_ui(rv$pool, conf, rv$medfield_data)
  })

  output$select_medfield_registry <- shiny::renderUI({
    select_registry_ui(rv$pool, conf,
      input_id = "medfield_registry",
      context = rv$context,
      current_reg = registry_tracker$current_registry
    )
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
    shiny::selectInput(
      inputId = "select_medfield",
      label = "Velg fagomr\u00e5de(r):",
      choices = all_medfield,
      selected = current_medfield,
      multiple = TRUE
    )
  })

  output$registry_medfield_header <- shiny::renderText({
    paste0(
      "<h2>", conf$medfield$text$heading, " <i>",
      get_registry_name(rv$pool, shiny::req(input$medfield_registry),
        full_name = TRUE
      ),
      "</i>:</h2><br>", conf$medfield$text$body
    )
  })

  output$registry_medfield_summary <- shiny::renderText({
    rv$medfield_summary
  })

  shiny::observeEvent(input$medfield_registry, {
    registry_tracker$current_registry <- input$medfield_registry
  })

  # user registries
  shiny::observeEvent(input$update_reguser, {
    registry_user_update <- data.frame(
      registry_id = rep(
        input$user_registry,
        length(input$select_user)
      ),
      user_id = input$select_user
    )
    update_registry_user(rv$pool, registry_user_update)
    rv$user_summary <-
      reguser_summary_text_ui(rv$pool, conf, rv$user_data)
  })

  output$select_user_registry <- shiny::renderUI({
    select_registry_ui(rv$pool, conf,
      input_id = "user_registry",
      context = rv$context,
      current_reg = registry_tracker$current_registry
    )
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
    shiny::selectInput(
      inputId = "select_user",
      label = "Velg bruker(e):",
      choices = all_user,
      selected = current_reguser,
      multiple = TRUE
    )
  })
  output$registry_user_header <- shiny::renderText({
    paste0(
      "<h2>", conf$reguser$text$heading, " <i>",
      get_registry_name(rv$pool, shiny::req(input$user_registry),
        full_name = TRUE
      ),
      "</i>:</h2><br>", conf$reguser$text$body
    )
  })
  output$registry_user_summary <- shiny::renderText({
    rv$user_summary
  })

  shiny::observeEvent(input$user_registry, {
    registry_tracker$current_registry <- input$user_registry
  })

  # our db admin interface
  output$admin_frame <- shiny::renderUI({
    shiny::tags$iframe(
      src = rv$admin_url, width = "100%", height = 1024,
      frameborder = "no"
    )
  })

  # mine field
  shiny::observeEvent(input$agg_all, {
    withCallingHandlers(

      tryCatch(
        agg_all_data(rv$pool),
        error = function(e) {
          message(paste0("<font color=\"#FF0000\">", e$message, "</font><br>"))
        }
      ),

      message = function(m) {
        shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
      }
    )
  })

  shiny::observeEvent(input$clean_agg, {
    withCallingHandlers(

      tryCatch(
        clean_agg_data(rv$pool),
        error = function(e) {
          message(paste0("<font color=\"#FF0000\">", e$message, "</font><br>"))
        }
      ),

      message = function(m) {
        shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
      }
    )
  })

  output$mine_field_uc <- shiny::renderUI({
    shiny::tagList(
      shiny::HTML(
        paste0(
          "<h3 style='color:",
          switch(rv$context,
            prod = "green;'>Produksjon</h3>",
            verify = "orange;'>Kvalitetskontroll</h3>",
            qa = "red;'>QA</h3>"
          )
        )
      ),
      shiny::p("Tr\u00e5 forsiktig!"),
      shiny::actionButton("agg_all", "Aggreger alle data",
        icon = shiny::icon("skull")
      ),
      shiny::hr(),
      shiny::actionButton("clean_agg", "Rydd aggregerte data")
    )
  })

  # reports
  report_server("report", pool, pool_verify)

  status_server("status", pool, pool_verify)

  # Heartbeat every 5 seconds, to avoid app to die when user is inactive.
  output$clock <- shiny::renderText({
    shiny::invalidateLater(5000)
    Sys.time()
  })
}
