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

  conf <- get_config()

  shiny::tagList(
    shiny::htmlOutput(ns("profile")),
    shiny::checkboxInput(
      ns("upload_history"),
      conf$profile$delivery$status$upload
    ),
    shiny::uiOutput(ns("ui_upload_table")),
    shiny::checkboxInput(
      ns("publish_history"),
      conf$profile$delivery$status$publish
    ),
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

      profile <- shiny::reactive({
        if (!nrow(get_user_data(pool)) > 0 ||
            conf$role$none %in% get_user_groups()) {
          conf$profile$pending
        } else {
          df <- get_user_data(pool)
          if (df$id %in% get_table(pool, "delivery")$user_id) {
            delivery_history <- ""
          } else {
            delivery_history <- conf$profile$delivery$none
          }
          paste(conf$profile$greeting, "<b>", get_user_name(), "</b>", "<br>",
                conf$profile$userinfo, "<br>",
                "Navn:", df$name, "<br>",
                "Telefon:", df$phone, "<br>",
                "e-post:", df$email, "<br><br>",
                conf$profile$howto, "<br><br>",
                delivery_history)
        }
      })

      upload_history <- shiny::reactive({
        if (input$upload_history) {
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
        if (input$publish_history) {
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
        upload_history()
      )

      output$publish_table <- DT::renderDataTable(
        publish_history()
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
