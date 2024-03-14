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

  shiny::tagList(
    shiny::htmlOutput(ns("profile")),
    shiny::uiOutput(ns("upload_history")),
    shiny::uiOutput(ns("ui_upload_table")),
    shiny::uiOutput(ns("publish_history")),
    shiny::uiOutput(ns("ui_publish_table"))
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

      output$upload_history <- shiny::renderUI({
        if (is_provider) {
          shiny::checkboxInput(
            ns("upload_history"),
            conf$profile$delivery$status$upload
          )
        } else {
          NULL
        }
      })

      output$publish_history <- shiny::renderUI({
        if (is_provider) {
          shiny::checkboxInput(
            ns("publish_history"),
            conf$profile$delivery$status$publish
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

          if (is_provider) {
            welcome_text <- paste(welcome_text, conf$profile$provider_howto)
          }

          if (is_provider & is_reviewer) {
            welcome_text <- paste(welcome_text, "<br><br>")
          }

          if (is_reviewer) {
            welcome_text <- paste(welcome_text, conf$profile$reviewer_howto)
          }

          if (is_provider) {
            welcome_text <- paste(welcome_text, "<br><br>", delivery_history, "<br><br>")
          }

          welcome_text

        }
      })

      upload_history <- shiny::reactive({
        if (is_provider & input$upload_history) {
          DT::datatable(
            get_user_deliveries(pool_verify),
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
        } else {
          NULL
        }
      })

      publish_history <- shiny::reactive({
        if (is_provider & input$publish_history) {
          DT::datatable(
            get_user_deliveries(pool),
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
        } else {
          NULL
        }
      })

      output$profile <- shiny::renderText({
        profile()
      })

      output$upload_table <- DT::renderDataTable(
        if (is_provider) {
          upload_history()
        } else {
          NULL
        }
      )

      output$publish_table <- DT::renderDataTable(
        if (is_provider) {
          publish_history()
        } else {
          NULL
        }
      )

      output$ui_upload_table <- shiny::renderUI(
        DT::dataTableOutput(ns("upload_table"))
      )

      output$ui_publish_table <- shiny::renderUI(
        DT::dataTableOutput(ns("publish_table"))
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
