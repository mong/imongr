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
        shiny::h2("loss data")
      ),
      shiny::tabPanel("Profil",
        shiny::h2("profil bruker")
      )
    )
  )
}
