#' Shiny module providing GUI and server logic for (user) profile
#'
#' @param id Character string module namespace
#' @param pool A database pool object connecting to production data
#' @param pool_verify A database pool object connecting to staging data
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_profile
#' @aliases profile_ui profile_server profile_app
NULL

#' @rdname mod_profile
#' @export
profile_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::htmlOutput(ns("profile"))
    ),
    shiny::mainPanel(
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(
            style = "background-color: transparent; border: 1px solid #c8c8c8; border-radius:
            6px; padding: 16px; margin-bottom: 10px;",
            shiny::htmlOutput(ns("welcome_text"))
          )
        )
      ),
      shiny::uiOutput(ns("ui_table"))
    )
  )
}


#' @rdname mod_profile
#' @export
profile_server <- function(id, pool, pool_verify) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      conf <- get_config()

      is_provider <- conf$role$provider %in% get_user_groups()

      output$history_type <- shiny::renderUI({
        if (is_provider) {
          shiny::radioButtons(
            ns("history_type"),
            label = NULL,
            choices = stats::setNames(
              c("upload", "publish"),
              c(
                conf$profile$delivery$status$upload,
                conf$profile$delivery$status$publish
              )
            ),
            selected = "upload",
            inline = TRUE
          )
        } else {
          NULL
        }
      })

      profile <- shiny::reactive({
        if (!nrow(get_user_data(pool)) > 0 || conf$role$none %in% get_user_groups()) {
          conf$profile$pending
        } else {
          df <- get_user_data(pool)
          if (df$id %in% get_table(pool, "delivery")$user_id) {
            delivery_history <- ""
          } else {
            delivery_history <- conf$profile$delivery$none
          }

          is_provider <- conf$role$provider %in% get_user_groups()
          is_reviewer <- conf$role$reviewer %in% get_user_groups()

          welcome_text <- paste(
            conf$profile$greeting, "<b>", get_user_name(), "</b>", "<br>",
            conf$profile$userinfo, "<br>",
            "Navn:", df$name, "<br>",
            "Telefon:", df$phone, "<br>",
            "e-post:", df$email, "<br><br>"
          )


          welcome_text

        }
      })

      welcome_text <- shiny::reactive({
        if (!nrow(get_user_data(pool)) > 0 || conf$role$none %in% get_user_groups()) {
          conf$profile$pending
        } else {
          df <- get_user_data(pool)
          if (df$id %in% get_table(pool, "delivery")$user_id) {
            delivery_history <- ""
          } else {
            delivery_history <- conf$profile$delivery$none
          }
          welcome_text <- ""
          is_provider <- conf$role$provider %in% get_user_groups()
          is_reviewer <- conf$role$reviewer %in% get_user_groups()
          if (is_provider) {
            welcome_text <- paste(welcome_text, conf$profile$provider_howto)
          }

          if (is_provider & is_reviewer) {
            welcome_text <- paste(welcome_text, "<br><br>")
          }

          if (is_reviewer) {
            welcome_text <- paste(welcome_text, conf$profile$reviewer_howto)
          }

          welcome_text <- paste(welcome_text, conf$profile$support_howto)

          if (is_provider) {
            welcome_text <- paste(welcome_text, "<br><br>", delivery_history, "<br><br>")
          }
          welcome_text
        }
      })

      history_table <- shiny::reactive({
        if (!is_provider) {
          return(NULL)
        }

        shiny::req(input$history_type)

        delivery_data <- switch(
          input$history_type,
          upload = get_user_deliveries(pool_verify),
          publish = get_user_deliveries(pool),
          get_user_deliveries(pool_verify)
        )

        DT::datatable(
          delivery_data,
          rownames = FALSE,
          options = list(
            dom = "tp",
            pageLength = 10,
            language = list(
              paginate = list(
                previous = "Forrige",
                `next` = "Neste"
              )
            )
          )
        )
      })

      output$profile <- shiny::renderText({
        profile()
      })

      output$welcome_text <- shiny::renderText({
        welcome_text()
      })

      output$history_table <- DT::renderDataTable(
        history_table()
      )

      output$ui_table <- shiny::renderUI(
        if (is_provider) {
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                style = "background-color: transparent; border: 1px solid #c8c8c8; border-radius: 6px; padding: 16px;",
                shiny::div(
                  shiny::uiOutput(ns("history_type"))
                ),
                DT::dataTableOutput(ns("history_table"))
              )
            )
          )
        } else {
          NULL
        }
      )
    }
  )
}


#' @rdname mod_profile
#' @export
profile_app <- function(pool, pool_verify) {
  ui <- shiny::fluidPage(
    profile_ui("profile")
  )

  server <- function(input, output, sessjon) {
    profile_server("profile", pool, pool_verify)
  }

  shiny::shinyApp(ui, server)
}
