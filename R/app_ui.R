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
        sidebarLayout(
          sidebarPanel(width = 3,
            uiOutput("select_db_table"),
            selectInput("file_format",
                        "Filformat:", c("csv", "csv2", "rds", "excel-csv",
                                        "excel-csv2")),
            downloadButton("download_db_table", "Loss!")
          ),
          mainPanel(
            uiOutput("ui_db_table")
          )
        )
      ),
      shiny::tabPanel("Admin",
        mainPanel(width = 12,
          htmlOutput("admin_frame"))
      )
    )
  )
}
