#' Client (ui) for the imonger app
#'
#' @return An imonger shiny app ui object
#' @export

app_ui <- function() {

  shiny::addResourcePath("www", system.file("www", package = "imongr"))
  app_title <- "imongr"

  shiny::tagList(
    shiny::navbarPage(
      title = shiny::div(
        shiny::a(
          shiny::includeHTML(
            system.file("www/logo.svg", package = "imongr")
          )
        ), app_title
      ),
      windowTitle = app_title,
      theme = "www/bootstrap.css",
      id = "tabs",

      shiny::tabPanel("Last",
        shiny::h2("last data")
      ),
      shiny::tabPanel("Loss",
        shiny::sidebarLayout(
          shiny::sidebarPanel(width = 3,
            shiny::uiOutput("select_db_table"),
            shiny::selectInput("file_format",
                               "Filformat:", c("csv", "csv2", "rds",
                                               "excel-csv",
                                               "excel-csv2")),
            shiny::downloadButton("download_db_table", "Loss!")
          ),
          shiny::mainPanel(
            shiny::uiOutput("ui_db_table")
          )
        )
      ),
      shiny::tabPanel("Sj\u00e6f",
        shiny::mainPanel(width = 12,
          shiny::htmlOutput("admin_frame"))
      )
    )
  )
}
