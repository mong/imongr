#' Shiny module providing UI and server functions for registry status overview
#'
#' @param id Character string module namespace
#' @param pool A database pool object connecting to production data
#' @param pool_verify A database pool object connecting to staging data
#'
#' @return Shiny objects for the imongr app
#'
#' @name mod_status
#' @aliases status_ui status_server status_app
NULL

format_data <- function(dat) {
  # Initialise data frame with column names
  years <- sort(unique(dat$year))
  registries <- sort(unique(dat$short_name))
  dat_format <- data.frame(matrix(ncol = length(years) + 1, nrow = length(registries)))
  colnames(dat_format) <- c("Register", years)

  dat_format$Register <- registries



  # Pass 1: assign values
  for (i in seq_along(registries)) {
    for (j in seq_along(years)) {
      verdict <- dat[dat$short_name == registries[i] & dat$year == years[j], ]

      if (ncol(verdict) > 0 && nrow(verdict) > 0) {
        dat_format[i, j + 1] <- verdict$verdict
      } else {
        dat_format[i, j + 1] <- NA
      }
    }
  }

  # Pass 2: mark changes

  return(dat_format)
}

#' @rdname mod_status
#' @export
add_arrows <- function(dat_format) {
  arrow_up <- "\U2197"
  arrow_down <- "\U2198"

  for (i in seq_len(nrow(dat_format))) {
    for (j in 3:ncol(dat_format)) {

      # No score, move on
      if (is.na(dat_format[i, j]) || is.na(dat_format[i, j - 1])) {
        next
      }

      current_score <- strsplit(dat_format[i, j], "") |> unlist()
      previous_score <- strsplit(dat_format[i, j - 1], "") |> unlist()

      if (current_score[1] > previous_score[1]) {
        dat_format[i, j] <- paste(dat_format[i, j], arrow_up)
      }

      if (current_score[1] < previous_score[1]) {
        dat_format[i, j] <- paste(dat_format[i, j], arrow_down)
      }
    }
  }

  return(dat_format)
}

#' @rdname mod_status
#' @export
status_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    DT::dataTableOutput(ns("status_table"))
  )
}

#' @rdname mod_status
#' @export
status_server <- function(id, pool, pool_verify) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      query <-
        "SELECT 
          evaluation.registry_id, evaluation.year, evaluation.verdict, registry.short_name
         FROM
           evaluation
         LEFT JOIN
           registry
         ON
           evaluation.registry_id = registry.id"

      dat <- pool::dbGetQuery(pool, query)

      rv <- shiny::reactiveValues(
        dat_format = dat |> format_data() |> add_arrows()
      )

      output$status_table <- DT::renderDataTable(
        DT::datatable(
                      rv$dat_format) |> DT::formatStyle(
          3:ncol(rv$dat_format),
          backgroundColor = DT::JS("(/\U2197/).test(value) ? 'green' : (/\U2198/).test(value) ? 'red' : 'white'")
        )
      )
    }
  )
}

#' @rdname mod_status
#' @export
status_app <- function(pool, pool_verify) {
  ui <- shiny::fluidPage(
    status_ui("status")
  )

  server <- function(input, output, session) {
    status_server("status", pool, pool_verify)
  }

  shiny::shinyApp(ui, server)
}
