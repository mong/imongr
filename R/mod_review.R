#' @rdname mod_review
#' @eksport
review_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .tooltip > .tooltip-inner {
          font-size: 20px;
          text-align: left;
          max-width: 1000px;
          background-color: #003087;
        }
      "))
    ),
    shinyjs::useShinyjs(),
    shiny::titlePanel("Vurdering av \u00e5rsrapporter"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("select_registry")),
        shiny::uiOutput(ns("registry_url")),
        shiny::br(),
        shiny::uiOutput(ns("select_year")),
        shiny::numericInput(
          ns("reported_dg"),
          "DG i % oppgitt av regionene utfra \u00e5rsrapporter",
          value = 0,
          min = 0,
          max = 100,
          step = 1
        ),
        shiny::br(),
        shiny::hr(),
        shiny::h5("Hovedansvarlig:"),
        shiny::uiOutput(ns("collaborators_main")),
        shiny::br(),
        shiny::h5("Leseansvarlige:"),
        shiny::uiOutput(ns("collaborators_read")),
        shiny::hr(),
        shiny::br(),
        shiny::h5(style = "text-align: center;", "Registeret vurderes til:"),
        shiny::uiOutput(ns("verdict")),
        shiny::br(),
        shiny::hr(),
        shiny::br(),
        shiny::uiOutput(ns("save")),
        shiny::br(),
        shiny::br(),
        shiny::uiOutput(ns("save_override")),
        width = 3
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(6,
            shiny::h3("Stadium 2"),
            shiny::uiOutput(ns("checkbox1")),
            shiny::uiOutput(ns("checkbox2")),
            shiny::uiOutput(ns("checkbox3")),
            shiny::uiOutput(ns("checkbox4")),
            shiny::uiOutput(ns("checkbox5")),
            shiny::actionButton(ns("togglestadium2"), "Stadium 2 oppfylt"),
            shiny::br(),
            shiny::br(),
            shiny::br(),
            shiny::h3("Stadium 3"),
            shiny::uiOutput(ns("checkbox6")),
            shiny::uiOutput(ns("checkbox7")),
            shiny::uiOutput(ns("checkbox8")),
            shiny::uiOutput(ns("checkbox9")),
            shiny::uiOutput(ns("checkbox10")),
            shiny::uiOutput(ns("checkbox11")),
            shiny::actionButton(ns("togglestadium3"), "Stadium 3 oppfylt"),
            shiny::br(),
            shiny::br(),
            shiny::br(),
            shiny::h3("Stadium 4"),
            shiny::uiOutput(ns("checkbox12")),
            shiny::uiOutput(ns("checkbox13")),
            shiny::uiOutput(ns("checkbox14")),
            shiny::uiOutput(ns("checkbox15")),
            shiny::uiOutput(ns("checkbox16")),
            shiny::actionButton(ns("togglestadium4"), "Stadium 4 oppfylt"),
            shiny::br(),
            shiny::br(),
            shiny::br(),
            shiny::h3("Niv\u00e5 A"),
            shiny::uiOutput(ns("checkbox17")),
            shiny::uiOutput(ns("level_A_comment")),
            shiny::br(),
            shiny::br(),
            shiny::h3("Niv\u00e5 B"),
            shiny::uiOutput(ns("checkbox18")),
            shiny::uiOutput(ns("level_B_comment")),
            shiny::br(),
            shiny::br(),
            shiny::h3("Ekspertgruppens vurdering"),
            shiny::uiOutput(ns("evaluation_text")),
          ),
          shiny::column(6,
            shiny::uiOutput(ns("table")),
            shiny::br(),
            shiny::br(),
            shiny::uiOutput(ns("graph")),
            shiny::br(),
            shiny::uiOutput(ns("plotcontrol"))
          )
        )
      )
    )
  )
}

#'@rdname mod_review
get_last_year <- function() {
  (as.numeric(format(Sys.Date(), "%Y")) - 1)
}

#'@rdname mod_review
update_graph_data <- function(input, pool, rv) {
  dat <- pool::dbGetQuery(pool, "SELECT * FROM evaluation")

  graph_data <- dat[dat$registry_id == input$selected_registry, ] %>%
    dplyr::select("year", "verdict", "reported_dg")

  return(graph_data)
}

#' @rdname mod_review
toggle_button <- function(input, session, rv, event, requirements) {
  shiny::observeEvent(input[[event]], {
    shiny::req(input$selected_registry)

    toggled <- rv$evaluation[requirements]

    if (all(toggled) | all(!toggled)) {
      lapply(X = requirements, FUN = function(i) {
        col_name <- paste0("requirement_", i)
        shiny::updateCheckboxInput(session, col_name, value = !(input[[col_name]]))
      })
    } else {
      lapply(X = requirements, FUN = function(i) {
        col_name <- paste0("requirement_", i)
        shiny::updateCheckboxInput(session, col_name, value = TRUE)
      })
    }
  })
}

#' @rdname mod_review
render_checkboxes <- function(input, output, df_requirements, ns, id_numbers) {
  lapply(X = id_numbers, FUN = function(i) {
    output[[paste0("checkbox", i)]] <- shiny::renderUI({

      shiny::req((as.numeric(input$selected_year) %>%
                    dplyr::between(df_requirements$introduction_year[i], df_requirements$last_year[i])))

      shiny::checkboxInput(ns(paste0("requirement_", i)), shiny::HTML(df_requirements$criteria[i]), width = "100%") |>
        bslib::tooltip(
          shiny::HTML(paste0(df_requirements$guide[i], "<br><br>", df_requirements$section[i])),
          options = list(html = TRUE, delay = 100, trigger = "hover")
        )
    })
  })
}

#' @rdname mod_review
#' @export
review_server <- function(id, registry_tracker, pool) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- get_config()

    is_manager <- conf$role$manager %in% get_user_groups()

    df_requirements <- pool::dbGetQuery(pool, "SELECT * FROM requirements")

    # Hvilke krav må være oppfylt for ulike stadier og nivå?
    n_requirements <- 18
    stage_4 <- 12:16
    stage_3 <- 6:11
    stage_2 <- 1:5
    level_a <- 17:18
    level_b <- 18

    rv <- shiny::reactiveValues(
      stage = NA,
      level = NA,
      evaluation = (rep(FALSE, n_requirements)),
      table_data = data.frame(user_id = get_user_id(pool)),
      registry_url = NULL,
      graph_data = NULL,
      collaborators = NULL
    )

    verdict <- shiny::reactive({
      # Nivåer ble introdusert i 2019
      shiny::req(input$selected_year)
      if (input$selected_year >= 2019) {
        paste0(rv$stage, rv$level)
      } else {
        rv$stage
      }
    })

    rv_return <- shiny::reactiveValues()

    update_form <- shiny::reactive({
      list(input$selected_registry, input$selected_year)
    })

    # Hold oversikt over valgt register
    shiny::observeEvent(input$indicator_registry, {
      rv_return$registry_id <- input$indicator_registry
    })

    #######################
    ##### UI sidemeny #####
    #######################

    output$select_registry <- shiny::renderUI({
      select_registry_ui(pool, conf,
        input_id = ns("selected_registry"),
        context = "verify",
        show_context = FALSE,
        current_reg = registry_tracker$current_registry
      )
    })

    shiny::observeEvent(input$selected_registry, {
      shiny::req(input$selected_registry)
      query <- paste0("SELECT url FROM registry WHERE id = ", input$selected_registry)
      dat <- pool::dbGetQuery(pool, query)

      if (!is.na(dat$url)) {
        rv$registry_url <- shiny::a("Hjemmeside til registeret", href = dat$url, target = "_blank")
      } else {
        rv$registry_url <- NULL
      }

      rv$graph_data <- update_graph_data(input, pool, rv)
      rv$collaborators <- get_review_collaborators(pool, input$selected_registry)
    })

    output$registry_url <- shiny::renderUI({
      shiny::tagList(rv$registry_url)
    })

    output$select_year <- shiny::renderUI({
      shiny::selectInput(
        ns("selected_year"),
        "Velg \u00e5r",
        c(2013L : (as.numeric(format(Sys.Date(), "%Y")) - 1)),
        selected = 2023
      )
    })

    output$collaborators_main <- shiny::renderUI({
      collaborators <- rv$collaborators[rv$collaborators$role == "main", ]$name

      lapply(collaborators, FUN = function(s) {
        shiny::div(style = "font-size: 100%;", s)
      })
    })

    output$collaborators_read <- shiny::renderUI({
      collaborators <- rv$collaborators[rv$collaborators$role == "read", ]$name

      lapply(collaborators, FUN = function(s) {
        shiny::div(style = "font-size: 100%;", s)
      })
    })

    output$verdict <- shiny::renderUI({
      shiny::div(style = "font-size: 200%; text-align: center", verdict())
    })

    # Gjem knapp hvis årstall ikke er fjoråret
    output$save <- shiny::renderUI({
      shiny::req(input$selected_registry)

      shiny::validate(
        shiny::need(dplyr::between(input$reported_dg, 0, 100), "Dekningsgrad må være fra 0 til 100\n"),
        shiny::need(input$selected_year == as.numeric(format(Sys.Date(), "%Y")) - 1,
                    "Redigering tillates kun på aktuelt rapporteringsår")
      )

      shiny::actionButton(
        ns("save"),
        "Lagre",
        shiny::icon("floppy-disk")
      )
    })

    output$save_override <- shiny::renderUI({
      shiny::req(input$selected_registry, input$selected_year)
      if (is_manager & !(input$selected_year == as.numeric(format(Sys.Date(), "%Y")) - 1)) {
        shiny::validate(
          shiny::need(dplyr::between(input$reported_dg, 0, 100), "Dekningsgrad må være fra 0 til 100\n")
        )

        shiny::actionButton(
          ns("save_override"),
          "Lagre likevel",
          shiny::icon("user-secret")
        )
      }
    })

    #######################
    ##### UI tekstfelt ####
    #######################

    output$evaluation_text <- shiny::renderUI({
      shiny::textAreaInput(
        ns("evaluation_text"), "Vurdering av \u00e5rsrapporten",
        value = "", width = "90%", rows = 8
      )
    })

    output$level_A_comment <- shiny::renderUI({
      shiny::req(input$selected_year >= 2019)
      shiny::textAreaInput(
        ns("level_A_comment"), "Kommentarer til niv\u00e5 A",
        value = "", width = "90%", rows = 4
      )
    })

    output$level_B_comment <- shiny::renderUI({
      shiny::req(input$selected_year >= 2019)
      shiny::textAreaInput(
        ns("level_B_comment"), "Kommentarer til niv\u00e5 B",
        value = "", width = "90%", rows = 4
      )
    })

    ################################
    ##### Reaktivitet sidemeny #####
    ################################

    shiny::observeEvent(input$selected_registry, {
      rv$table_data$registry_id <- input$selected_registry
    })

    shiny::observeEvent(input$selected_year, {
      rv$table_data$year <- input$selected_year
    })

    shiny::observeEvent(input$reported_dg, {
      rv$table_data$reported_dg <- input$reported_dg
    })

    shiny::observeEvent(input$level_A_comment, {
      rv$table_data$level_A_comment <- input$level_A_comment
    })

    shiny::observeEvent(input$level_B_comment, {
      rv$table_data$level_B_comment <- input$level_B_comment
    })

    shiny::observeEvent(input$evaluation_text, {
      rv$table_data$evaluation_text <- input$evaluation_text
    })

    # Regn ut stadium og nivå
    shiny::observeEvent(rv$evaluation, {
      rv$stage <- 1

      if (all(rv$evaluation[c(stage_2, stage_3, stage_4)])) {
        rv$stage <- 4
      } else if (all(rv$evaluation[c(stage_2, stage_3)])) {
        rv$stage <- 3
      } else if (all(rv$evaluation[stage_2])) {
        rv$stage <- 2
      }

      rv$level <- "C"

      if (all(rv$evaluation[level_a])) {
        rv$level <- "A"
      } else if (rv$evaluation[level_b]) {
        rv$level <- "B"
      }
    })

    shiny::observeEvent(verdict(), {
      rv$table_data$verdict <- verdict()
    })

    ##### Lagre #####
    shiny::observeEvent(input$save, {

      update_review(pool, rv$table_data, input$selected_registry, input$selected_year)

      shinyalert::shinyalert("Ferdig",
        "Dine data er n\u00e5 lagret",
        type = "success",
        showConfirmButton = FALSE,
        timer = 2000
      )

      rv$graph_data <- update_graph_data(input, pool, rv)
    })

    shiny::observeEvent(input$save_override, {

      update_review(pool, rv$table_data, input$selected_registry, input$selected_year)

      shinyalert::shinyalert("Ferdig",
        "Dine data er n\u00e5 lagret",
        type = "success",
        showConfirmButton = FALSE,
        timer = 2000
      )

      rv$graph_data <- update_graph_data(input, pool, rv)
    })

    #####################
    ##### UI skjema #####
    #####################

    render_checkboxes(input, output, df_requirements, ns, c(stage_2, stage_3, stage_4))

    shiny::observeEvent(input$selected_year, {
      if (input$selected_year >= 2019) {
        render_checkboxes(input, output, df_requirements, ns, level_a)
      } else {
        lapply(X = level_a, FUN = function(i) {
          output[[paste0("checkbox", i)]] <- NULL
        })
      }
    })

    ##############################
    ##### Reaktivitet skjema #####
    ##############################

    lapply(X = 1:n_requirements, FUN = function(i) {

      col_name <- paste0("requirement_", i)

      shiny::observeEvent(input[[col_name]], {
        rv$evaluation[i] <- input[[col_name]]
        rv$table_data[[col_name]] <- as.numeric(input[[col_name]])
      })
    }
    )

    # Knapper for sjekking av alle bokser i et stadium
    toggle_button(input, session, rv, "togglestadium2", stage_2)
    toggle_button(input, session, rv, "togglestadium3", stage_3)
    toggle_button(input, session, rv, "togglestadium4", stage_4)

    # Oppdater skjema ved valg av år og register
    shiny::observeEvent(update_form(), {

      dat <- pool::dbGetQuery(pool, "SELECT * FROM evaluation")

      # Filtrer på år og register og oppdater skjema
      dat <- dat[(dat$year == input$selected_year) & (dat$registry_id == input$selected_registry), ]

      if (nrow(dat) == 1) {
        lapply(X = 1:n_requirements, FUN = function(i) {
          col_name <- paste0("requirement_", i)
          shiny::updateCheckboxInput(session, col_name, value = dat[[col_name]][1])
        })

        shiny::updateTextInput(session, "level_A_comment", value = dat$level_A_comment)
        shiny::updateTextInput(session, "level_B_comment", value = dat$level_B_comment)
        shiny::updateTextInput(session, "evaluation_text", value = dat$evaluation_text)

        shiny::updateNumericInput(session, "reported_dg", value = dat$reported_dg)

      } else {
        lapply(X = 1:n_requirements, FUN = function(i) {
          shiny::updateCheckboxInput(session, paste0("requirement_", i), value = FALSE)
        })

        shiny::updateTextInput(session, "level_A_comment", value = "")
        shiny::updateTextInput(session, "level_B_comment", value = "")
        shiny::updateTextInput(session, "evaluation_text", value = "")

        shiny::updateNumericInput(session, "reported_dg", value = 0)
      }
    })

    # Tabell og graf på høyre side
    output$table <- shiny::renderUI({
      table_data <- rv$graph_data
      shiny::req(table_data)
      if (nrow(table_data) > 0) {
        shiny::renderTable({
          colnames(table_data) <- c("År", "Stadium", "Dekningsgrad")
          table_data
        }, width = "100%", align = "c")
      } else {
        shiny::h3("Ingen data registrert")
      }
    })

    output$plotcontrol <- shiny::renderUI({
      shiny::req(rv$graph_data)
      shiny::req(nrow(rv$graph_data) > 0)

      if (!is.null(input$show_dg_plot)) {
        current_choice <- input$show_dg_plot
      } else {
        current_choice <- FALSE
      }
      shiny::checkboxInput(ns("show_dg_plot"),
                           label = "Vis oppgitt dekningsgrad istedenfor stadium",
                           value = current_choice)
    })

    output$graph <- shiny::renderUI({
      plot_data <- rv$graph_data
      shiny::req(plot_data)
      shiny::req(nrow(plot_data) > 0)
      shiny::req(!is.null(input$show_dg_plot))

      shiny::renderPlot({

        stage_level <- strsplit(plot_data$verdict, split = "")

        plot_data$stage <- as.numeric(lapply(stage_level, FUN = function(x) {
          return(x[1])
        }))

        plot_data$level <- unlist(lapply(stage_level, FUN = function(x) {
          if (length(x) < 2) {
            return(NA)
          } else {
            return(x[2])
          }
        }))

        colour_map <- c("#648FFF", "#DC267F", "#FE6100", "#AAAAAA")
        names(colour_map) <- c("A", "B", "C", NA)

        base_plot <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = year)) + # nolint: object_usage_linter.
          ggplot2::scale_x_continuous("År", 2013:get_last_year(), limits = c(2013, get_last_year())) +
          ggplot2::scale_colour_manual(values = colour_map, limits = c("A", "B", "C", NA)) +
          ggplot2::theme_classic() +
          ggplot2::theme(text = ggplot2::element_text(size = 14)) +
          ggplot2::guides(colour = ggplot2::guide_legend(title = "Niv\u00e5"))

        if (!input$show_dg_plot) {
          output_plot <- base_plot +
            ggplot2::geom_line(ggplot2::aes(y = stage)) + # nolint: object_usage_linter.
            ggplot2::geom_point(ggplot2::aes(y = stage, colour = factor(level)), # nolint: object_usage_linter.
                                size = 3,
                                show.legend = c(colour = TRUE)) +
            ggplot2::scale_y_continuous("Stadium", 1:4, limits = c(1, 4))
        } else {
          output_plot <- base_plot +
            ggplot2::geom_line(ggplot2::aes(y = reported_dg)) + # nolint: object_usage_linter.
            ggplot2::geom_point(ggplot2::aes(y = reported_dg, colour = factor(level)), # nolint: object_usage_linter.
                                size = 3,
                                show.legend = c(colour = TRUE)) +
            ggplot2::scale_y_continuous("Oppgitt dekningsgrad", limits = c(0, 100))
        }

        return(output_plot)
      })
    })


    return(rv_return)
  })
}

#' @rdname mod_review
#' @eksport
review_app <- function(pool) {
  ui <- shiny::fluidPage(
    review_ui("review")
  )

  server <- function(input, output, session) {
    review_server("review", NA, pool)
  }

  shiny::shinyApp(ui, server)
}
