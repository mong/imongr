#' Functions that provide server output
#'
#' @param pool Database pool object
#' @param pool0 Alternative database pool object to be used when an intersect of
#'   values are to be returned.
#' @param conf List of configuration
#' @param input_id Character string with shiny ui id
#' @param context Character string with user selected (db) context
#' @param current_reg Character string defining previously selected registry
#' @param show_context Logical stating if context is to be shown in GUI. TRUE by
#'   default.
#' @param df Data frame containing indicator data
#' @param ind Data frame containing indicators
#' @param upload_file Character string with file to upload
#' @param registry Character string defining registry
#' @param indicators Character vector of indicators
#' @param sample_size Numeric sample size
#' @param sample_type Character string of sampling type
#'
#' @return shiny ui objects to be provided by the shiny server function
#' @name server_output
#' @aliases select_registry_ui submit_ui error_report_ui
#'   upload_sample_text_ui upload_sample_ui var_doc_ui medfield_summary_text_ui
#'   reguser_summary_text_ui
NULL


#' @rdname server_output
#' @export
select_registry_ui <- function(pool, conf, input_id, context,
                               current_reg = character(), show_context = TRUE,
                               pool0 = NULL) {
  if (conf$role$manager %in% get_user_groups()) {
    regs <- get_table(pool, "registry") |>
      dplyr::transmute(.data$short_name, .data$id) |>
      tibble::deframe()
    if (!is.null(pool0)) {
      regs0 <- get_table(pool0, "registry") |>
        dplyr::transmute(.data$short_name, .data$id) |>
        tibble::deframe()
    }
  } else {
    regs <- get_user_registry_select(pool)
    if (!is.null(pool0)) {
      regs0 <- get_user_registry_select(pool0)
    }
  }

  if (!is.null(pool0)) {
    regs <- c(regs, regs0)[intersect(names(regs), names(regs0))]
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

  context_ui <- NULL
  if (show_context) {
    context_ui <- shiny::HTML(
      paste0(
        "<h3 style='color:",
        switch(context,
          prod = "green;'>Produksjon</h3>",
          verify = "orange;'>Kvalitetskontroll</h3>",
          qa = "red;'>QA</h3>"
        )
      )
    )
  }

  shiny::tagList(
    context_ui,
    shiny::selectInput(input_id, label, regs, selected = reg)
  )
}

#' @rdname server_output
#' @export
submit_ui <- function(input_id, conf, pool, upload_file,
                      registry, df, ind, context) {
  if (!is.null(upload_file) && !"denominator" %in% names(df)) {
    df <- cbind(df, denominator = 1L)
  }

  if (
    !is.null(upload_file) &&
      !(conf$upload$fail %in% registry) &&
      all(!check_upload(registry, df, ind, pool)$fail)
  ) {
    shiny::tagList(
      shiny::actionButton(
        input_id,
        paste(
          "Send til",
          switch(context,
            prod = "produksjonsserver",
            verify = "verifiseringsserver",
            qa = "QA-server"
          )
        ),
        shiny::icon("paper-plane"),
        style = conf$profile$action_button_style
      ),
      shiny::p(paste(
        conf$upload$doc$submit$warning,
        get_registry_name(pool, registry)
      ))
    )
  } else {
    NULL
  }
}

#' @rdname server_output
#' @export
error_report_ui <- function(pool, df, ind, upload_file, registry) {
  if (is.null(upload_file) || is.null(registry)) {
    NULL
  } else {
    if (!"denominator" %in% names(df)) {
      df <- cbind(df, denominator = NA)
    }
    check_report(registry, df, ind, pool)
  }
}

#' @rdname server_output
#' @export
warning_report_ui <- function(pool, df, upload_file, registry) {
  if (is.null(upload_file) || is.null(registry)) {
    NULL
  } else {
    df_delivery <- pool::dbGetQuery(pool, "SELECT md5_checksum, user_id, time FROM delivery ORDER BY time ASC")
    checksums <- df_delivery$md5_checksum
    current_checksum <- md5_checksum(df)

    if (current_checksum %in% checksums) {
      df_users <- pool::dbGetQuery(pool, "SELECT id, user_name FROM user")
      colnames(df_users) <- c("user_id", "user_name")

      same_data_deliveries <- df_delivery[which(current_checksum == checksums), ] |>
        dplyr::left_join(df_users, by = "user_id")

      msg_dates <- paste0(same_data_deliveries$time, " av ", same_data_deliveries$user_name) |>
        paste(collapse = "<br/>")

      paste(
        "<font color=\"#b5a633\">",
        "En identisk fil har blitt lastet opp tidligere:<br/>",
        msg_dates, "</font>"
      )
    } else {
      NULL
    }
  }
}

#' @rdname server_output
#' @export
upload_sample_text_ui <- function(pool, conf, upload_file, registry,
                                  indicators) {
  if (is.null(upload_file)) {
    NULL
  } else {
    # prep indicator printing
    all_indicators <- get_registry_indicators(pool, registry)$id
    i <- all_indicators %in% indicators
    start_tag <- rep("<i>", length(all_indicators))
    end_tag <- rep("</i>", length(all_indicators))
    start_tag[i] <- "<i><mark>"
    end_tag[i] <- "</mark></i>"
    indicators <- paste0(start_tag, all_indicators, end_tag)

    paste0(
      conf$upload$doc$sample,
      " <b>",
      get_registry_name(pool, registry, full_name = TRUE),
      "</b>: ",
      paste(indicators, collapse = ", "),
      "."
    )
  }
}

#' @rdname server_output
#' @export
upload_sample_ui <- function(df, upload_file, registry, sample_size,
                             sample_type) {
  if (is.null(upload_file)) {
    NULL
  } else {
    sample_df(
      df = df, skip = c(registry), n = sample_size,
      random = sample_type
    )
  }
}

#' @rdname server_output
#' @export
var_doc_ui <- function(conf) {
  l <- "<ul>\n"
  for (i in conf$upload$data_var_ind) {
    var <- conf$db$tab$data$insert[i]
    l <- paste0(
      l, "\t<li><b>", var, "</b>: ",
      conf$upload$doc[[var]], "</li>\n"
    )
  }
  l
}

#' @rdname server_output
#' @export
medfield_summary_text_ui <- function(pool, conf, df) {
  if (dim(df)[1] > 0) {
    txt <- paste0("<h2>", conf$medfield$text$summary, "</h2>\n")
    for (i in seq_len(length(df$id))) {
      txt <- paste0(txt, "<h3>", df$full_name[i], "</h3>\n")
      regs <- get_medfield_registry(pool, df$id[i])
      if (dim(regs)[1] > 0) {
        regtxt <- paste0(
          "<ul>\n\t<li>",
          paste(
            get_registry_name(pool,
              registry = regs$registry_id,
              full_name = FALSE
            ),
            collapse = "</li>\n\t<li>"
          ),
          "</li>\n</ul>"
        )
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
