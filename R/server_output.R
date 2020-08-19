#' Functions that provide server output
#'
#' @param pool Data base pool object
#' @param conf List of configuration
#' @param input_id Character string with shiny ui id
#' @param valid_user Logical valid user
#' @param iusr Character string username
#' @param igrs Character string comma separated user groups
#' @param df Data frame
#' @param upload_file Character string with file to upload
#' @param registry Character string definig registry
#' @param sample_size Numeric sample size
#' @param sample_type Character string of sampling type
#'
#' @return shiny ui objects to be provided by the shiny server function
#' @name server_output
#' @aliases select_registry_ui submit_ui error_report_ui upload_sample_text_ui
#' upload_sample_ui var_doc_ui
NULL


#' @rdname server_output
#' @export
profile_ui <- function(conf, pool, valid_user, iusr, igrs) {
  if (!valid_user || conf$role$none %in% igrs) {
    conf$profile$pending
  } else {
    df <- get_user_data(pool)
    if (dim(get_user_deliveries(pool))[1] < 1) {
      delivery_history <- conf$profile$delivery$none
    } else {
      delivery_history <- conf$profile$delivery$status
    }
    paste(conf$profile$greeting, "<b>", iusr, "</b>", "<br>",
          conf$profile$userinfo, "<br>",
          "Navn:", df$name, "<br>",
          "Telefon:", df$phone, "<br>",
          "e-post:", df$email, "<br><br>",
          conf$profile$howto, "<br><br>",
          delivery_history)
  }
}

#' @rdname server_output
#' @export
select_registry_ui <- function(pool, conf, input_id) {
  regs <- get_user_registry_select(pool)
  label <- conf$app_text$select$registry$ok
  if (length(regs) == 0) {
    label <- conf$app_text$select$registry$missing
  } else {

  }
  shiny::selectInput(input_id, label, regs)
}




#' @rdname server_output
#' @export
submit_ui <- function(conf, pool, upload_file, registry, df) {

  if (!is.null(upload_file) && !"denominator" %in% names(df)) {
    df <- cbind(df, denominator = 1L)
  }
  if (!is.null(upload_file) &&
      !(conf$upload$fail %in% registry) &&
      all(!check_upload(registry, df, pool)$fail)) {
    shiny::tagList(
    shiny::actionButton("submit", "Send til server",
                        shiny::icon("paper-plane")),
    shiny::p(paste(conf$upload$doc$submit$warning,
                   get_registry_name(pool, registry)))
    )
  } else {
    NULL
  }

}

#' @rdname server_output
#' @export
error_report_ui <- function(pool, df, upload_file, registry) {
  if (is.null(upload_file)) {
    NULL
  } else {
    if (!"denominator" %in% names(df)) {
      df <- cbind(df, denominator = NA)
    }
    check_report(registry, df, pool)
  }
}

#' @rdname server_output
#' @export
upload_sample_text_ui <- function(pool, conf, upload_file, registry) {
  if (is.null(upload_file)) {
    NULL
  } else {
    paste0(conf$upload$doc$sample, " ", get_registry_name(pool, registry,
                                                          full_name = TRUE),
           ": <i>",
          paste(get_registry_indicators(pool, registry)$id,
                collapse = ", "),
          "</i>.")
  }
}

#' @rdname server_output
#' @export
upload_sample_ui <- function(df, upload_file, registry, sample_size,
                             sample_type) {
  if (is.null(upload_file)) {
    NULL
  } else {
    sample_df(df = df, skip = c(registry), n = sample_size,
              random = sample_type)
  }
}

#' @rdname server_output
#' @export
var_doc_ui <- function(conf) {
  l <- "<ul>\n"
  for (i in conf$upload$data_var_ind) {
    var <- conf$db$tab$data$insert[i]
    l <- paste0(l, "\t<li><b>", var, "</b>: ",
                conf$upload$doc[[var]], "</li>\n")
  }
  l
}
