#' Functions that provide server output
#'
#' @param pool Data base pool object
#' @param conf List of configuration
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
select_registry_ui <- function(conf, pool) {
  regs <- get_user_registries(pool)
  if (length(regs) == 0) {
    regs <- c(conf$upload$fail)
  }
  shiny::selectInput("registry", "Velg register:", regs, selected = regs[1])
}




#' @rdname server_output
#' @export
submit_ui <- function(conf, pool, upload_file, registry, df) {

  if (!is.null(upload_file) && !"nevner" %in% names(df)) {
    df <- cbind(df, nevner = NA)
  }
  if (!is.null(upload_file) &&
      !(conf$upload$fail %in% registry) &&
      all(!check_upload(cbind(df, Register = registry), pool)$fail)) {
    shiny::actionButton("submit", "Send til server",
                        shiny::icon("paper-plane"))
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
    if (!"nevner" %in% names(df)) {
      df <- cbind(df, nevner = NA)
    }
    check_report(cbind(df, Register = registry), pool)
  }
}

#' @rdname server_output
#' @export
upload_sample_text_ui <- function(conf, upload_file) {
  if (is.null(upload_file)) {
    NULL
  } else {
    conf$upload$doc$sample
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