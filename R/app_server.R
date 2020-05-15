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

  # loss
  output$select_db_table <- renderUI({
    selectInput("tab_set", "Velg tabell:", names(conf$db$tab),
                selected = names(conf$db$tab)[1])
  })
  output$db_table <- DT::renderDataTable(
    get_table(pool, input$tab_set), rownames = FALSE
  )

  output$ui_db_table <- renderUI(
    DT::dataTableOutput("db_table")
  )

  # our db admin interface
  admin_url <- paste0(adminer_url(), "/?",
                      "server=", db_host(), "&",
                      "username=", db_username(), "&",
                      "db=", db_name())

  output$admin_frame <- renderUI({
    tags$iframe(src = admin_url, width = "100%", height = 1024,
                      frameborder = "no")
  })

}
