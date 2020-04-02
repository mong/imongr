#' Run the imongr Shiny Application
#'
#' @return An object representeing the imongr app
#' @export
#'
#' @examples
#' run_app()

run_app <- function() {

  shiny::shinyApp(ui = app_ui, server = app_server)
}
