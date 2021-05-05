#' Functions that provide server output
#'
#' @param pool Data base pool object
#' @param conf List of configuration
#' @param input_id Character string with shiny ui id
#' @param context Character string with user selected (db) context
#' @param current_reg Character string defining previously selected registry
#' @param valid_user Logical valid user
#' @param iusr Character string username
#' @param igrs Character string comma separated user groups
#' @param df Data frame
#' @param upload_file Character string with file to upload
#' @param registry Character string defining registry
#' @param indicators Character vector of indicators
#' @param sample_size Numeric sample size
#' @param sample_type Character string of sampling type
#'
#' @return shiny ui objects to be provided by the shiny server function
#' @name server_output
#' @aliases select_registry_ui submit_ui error_report_ui upload_sample_text_ui
#' upload_sample_ui var_doc_ui medfield_summary_text_ui reguser_summary_text_ui
NULL


#' @rdname server_output
#' @export
profile_ui <- function(conf, pool, valid_user, iusr, igrs) {
  if (!valid_user || conf$role$none %in% igrs) {
    conf$profile$pending
  } else {
    df <- get_user_data(pool)
    if (df$id %in% get_table(pool, "delivery")$user_id) {
      delivery_history <- ""
    } else {
      delivery_history <- conf$profile$delivery$none
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
select_registry_ui <- function(pool, conf, input_id, context,
                               current_reg = character()) {

  if (conf$role$manager %in% get_user_groups()) {
    regs <- get_table(pool, "registry") %>%
      dplyr::transmute(.data$name, .data$id) %>%
      tibble::deframe()
  } else {
    regs <- get_user_registry_select(pool)
  }

  label <- conf$app_text$select$registry$ok
  if (length(regs) == 0) {
    label <- conf$app_text$select$registry$missing
    reg <- NULL
  } else {
    if (length(current_reg) == 0) {
      reg <- regs[1]
    } else {
      reg <- current_reg
    }
  }
  shiny::tagList(
    shiny::HTML(
      paste0("<h3 style='color:",
             switch(context,
                    prod = "green;'>Produksjon</h3>",
                    verify = "orange;'>Dataverifisering</h3>",
                    qa = "red;'>QA</h3>"))
    ),
    shiny::selectInput(input_id, label, regs, selected = reg)
  )
}




#' @rdname server_output
#' @export
submit_ui <- function(conf, pool, upload_file, registry, df, context) {

  if (!is.null(upload_file) && !"denominator" %in% names(df)) {
    df <- cbind(df, denominator = 1L)
  }
  if (!is.null(upload_file) &&
      !(conf$upload$fail %in% registry) &&
      all(!check_upload(registry, df, pool)$fail)) {
    shiny::tagList(
    shiny::actionButton("submit",
                        paste("Send til",
                              switch(context,
                                     prod = "produksjonsserver",
                                     verify = "verifiseringsserver",
                                     qa = "QA-server")),
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
upload_sample_text_ui <- function(pool, conf, upload_file, registry,
                                  indicators) {

  if (is.null(upload_file)) {
    NULL
  } else {
    # prep indicatior printing
    all_indicators <- get_registry_indicators(pool, registry)$id
    i <- all_indicators %in% indicators
    start_tag <- rep("<i>", length(all_indicators))
    end_tag <- rep("</i>", length(all_indicators))
    start_tag[i] <- "<i><mark>"
    end_tag[i] <- "</mark></i>"
    indicators <- paste0(start_tag, all_indicators, end_tag)

    paste0(conf$upload$doc$sample, " <b>", get_registry_name(pool, registry,
                                                          full_name = TRUE),
           "</b>: ",
          paste(indicators, collapse = ", "),
          ".")
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

medfield_summary_text_ui <- function(pool, conf, df) {

  if (dim(df)[1] > 0) {
    txt <- paste0("<h2>", conf$medfield$text$summary, "</h2>\n")
    for (i in seq_len(length(df$id))) {
      txt <- paste0(txt, "<h3>", df$full_name[i], "</h3>\n")
      regs <- get_medfield_registry(pool, df$id[i])
      if (dim(regs)[1] > 0) {
        regtxt <- paste0("<ul>\n\t<li>",
                         paste(get_registry_name(pool,
                                                 registry = regs$registry_id,
                                                 full_name = FALSE),
                               collapse = "</li>\n\t<li>"),
                         "</li>\n</ul>")
      } else {
        regtxt <- conf$medfield$text$missing
      }
      txt <- paste0(txt, regtxt)
    }
    txt
  } else {
    NULL
  }
}

reguser_summary_text_ui <- function(pool, conf, df) {

  if (dim(df)[1] > 0) {
    txt <- paste0("<h2>", conf$reguser$text$summary, "</h2>\n")
    for (i in seq_len(length(df$id))) {
      txt <- paste0(txt, "<h3>", df$user_name[i], "</h3>\n")
      regs <- get_user_registry(pool, df$id[i])
      if (dim(regs)[1] > 0) {
        regtxt <- paste0("<ul>\n\t<li>",
                         paste(get_registry_name(pool,
                                                 registry = regs$registry_id,
                                                 full_name = FALSE),
                               collapse = "</li>\n\t<li>"),
                         "</li>\n</ul>")
      } else {
        regtxt <- conf$reguser$text$missing
      }
      txt <- paste0(txt, regtxt)
    }
    txt
  } else {
    NULL
  }
}
