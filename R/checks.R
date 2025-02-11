#' Functions for checks in imongr
#'
#' @param input A shiny input object
#' @param conf The data from the get_config function
#' @param ns A shiny::NS namespace function
#' @param rv A shiny::reactiveValues object
#'
#' @name checks
NULL

############################
##### Indicator checks #####
############################

#' Replace "Ingen" with NA in a string
#'
#' This function is intended for use in indicator_server.
#' We would like the select_dg_id input to return NA,
#' but shiny::selectInput turns NAs into strings.
#' The input should therefore go through this function.
#'
#' @param s string
check_no_dg <- function(s) {
  dplyr::case_when(s == "Ingen" ~ NA, .default = s)
}

#' Display an oversize warning if there are too many characters in the input text
#'
#' This function is intended for use in indicator_server.
#' If the input text for indicator title, short description
#' or long description is too long according to the specification
#' in the config file, then an error message is displayed.
#'
#' @param oversize Logical
#' @param conf The data from the get_config function
#' @param conf get_config() output
oversize_check <- function(oversize, conf) {
  if (oversize) {
    shiny::HTML(conf$indicator$oversize_message)
  } else {
    NULL
  }
}

#' Check that the accomplishment thresholds are consistent
#'
#' This function is intended for use in indicator_server.
#' Threshold are set for high accomplishment (green)
#' and medium accomplihment (yellow). The green threshold
#' should be higher than the yellow. If not, and error message
#' is displayed.
#'
#' @rdname checks
#' @internal
#' @param input Shiny input object
#' @param conf get_config() output
levels_consistent_check <- function(input, conf) {
  if (!is.na(input$level_green) && !is.na(input$level_yellow)) {
    if (input$level_direction) {
      if (input$level_green >= input$level_yellow) {
        shinyjs::html("message", "")
        return(TRUE)
      } else {
        shinyjs::html("message", "")
        shinyjs::html(
          "message",
          conf$indicator$level_inconsistent_message
        )
        return(FALSE)
      }
    } else {
      if (input$level_yellow >= input$level_green) {
        shinyjs::html("message", "")
        return(TRUE)
      } else {
        shinyjs::html("message", "")
        shinyjs::html(
          "message",
          conf$indicator$level_inconsistent_message
        )
        return(FALSE)
      }
    }
  } else {
    shinyjs::html("message", "")
    return(TRUE)
  }
}

#' Check for updated values in the indicator sidebar meny
#' @param input A shiny input object
#' @param conf The data from the get_config function
#' @param ns A shiny::NS namespace function
#' @param rv A shiny::reactiveValues object
#'
#' @rdname checks
#' @internal
update_check <- function(input, conf, ns, rv, level_consistent) {
  if (any(c(
    is.null(input$indicator),
    is.null(input$include),
    is.null(input$level_direction),
    nrow(rv$ind_data) == 0
  ))) {
    NULL
  } else {
    no_new_values <- c(
      identical(input$include, as.logical(rv$ind_data$include)),
      identical(check_no_dg(input$dg_id), rv$ind_data$dg_id),
      identical(
        input$level_direction,
        as.logical(rv$ind_data$level_direction)
      ),
      identical(
        as.numeric(input$level_green),
        as.numeric(rv$ind_data$level_green)
      ),
      identical(
        as.numeric(input$level_yellow),
        as.numeric(rv$ind_data$level_yellow)
      ),
      identical(
        as.numeric(input$min_denominator),
        as.numeric(rv$ind_data$min_denominator)
      ),
      identical(input$type, rv$ind_data$type),
      identical(input$format, rv$sformat$format),
      identical(
        as.numeric(input$digits),
        as.numeric(rv$sformat$digits)
      )
    )
    if (all(no_new_values)) {
      return(NULL)
    } else {
      if (level_consistent()) {
        return(
          shiny::actionButton(
            ns("update_val"),
            "Oppdat\u00e9r verdier",
            style = conf$profile$action_button_style
          )
        )
      } else {
        return(NULL)
      }
    }
  }
}

#' Check for updated indicator descriptions in the main panel of the indicator tab
#'
#' @param input A shiny input object
#' @param conf The data from the get_config function
#' @param ns A shiny::NS namespace function
#' @param rv A shiny::reactiveValues object
#'
#' @rdname checks
#' @internal
update_indicator_txt_check <- function(input, conf, ns, rv) {
  if (any(c(rv$title_oversize, rv$short_oversize, rv$long_oversize))) {
    NULL
  } else {
    no_new_text <- c(
      identical(input$ind_short, rv$ind_data$short_description),
      identical(input$ind_title, rv$ind_data$title),
      identical(input$ind_long, rv$ind_data$long_description)
    )
    if (all(no_new_text)) {
      return(NULL)
    } else if (nrow(rv$ind_data != 0)) {
      shiny::actionButton(
        ns("update_txt"),
        "Oppdat\u00e9r tekster",
        style = conf$profile$action_button_style
      )
    }
  }
}
