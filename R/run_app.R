#' Run the imongr Shiny Application
#'
#' @return An object representing the imongr app
#' @export

run_app <- function() {
  shiny::shinyApp(ui = app_ui, server = app_server, options = list(launch.browser = Sys.getenv("USER") == "rstudio"))
}
