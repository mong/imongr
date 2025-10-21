#' Shiny module providing GUI and server logic for the publication tab
#'
#' @param id Character string module namespace
#' @param pool A database pool object
#' @param pool_verify A database pool object
#' @param registry_tracker Integer defining registry id
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_publication
#' @aliases publication_ui publication_server publication_app
NULL

#' @rdname mod_publication
#' @export
publication_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_project_registry")),
        shiny::uiOutput(ns("add_new_publication")),
        shiny::uiOutput(ns("update_values_button"))
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("publication_list")),
        shiny::uiOutput(ns("update_list_button"))
      )
    )
  )

}#' @rdname mod_publication
#' @export
publication_server <- function(id, registry_tracker, pool, pool_verify) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- get_config()

    rv_return <- shiny::reactiveValues()
    rv <- shiny::reactiveValues()

    return(rv_return)
  })
}