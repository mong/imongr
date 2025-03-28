#' Client (ui) for the imongr app
#'
#' @return An imongr shiny app ui object
#' @export

app_ui <- function() {
  conf <- get_config()
  shiny::addResourcePath("www", system.file("www", package = "imongr"))
  app_title <- "Data | SKDE"

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      # Heartbeat every 5 seconds, to avoid app to die when user is inactive.
      # Will be out of sight on the webpage.
      shiny::tags$div(
        style = "position: absolute; top: -100px;",
        shiny::textOutput("clock")
      )
    ),
    shiny::navbarPage(
      theme = bslib::bs_theme(
        version = 5
      ),
      title = app_title,
      windowTitle = app_title,
      id = "tabs",
      shiny::tabPanel(
        value = "profile",
        shiny::span("Profil",
          title = conf$app_text$tooltip$profile
        ),
        profile_ui("profile")
      ),
      shiny::tabPanel(
        value = "upload",
        shiny::span("Last opp data",
          title = conf$app_text$tooltip$upload
        ),
        upload_ui("upload")
      ),
      shiny::tabPanel(
        value = "publish",
        shiny::span(
          "Publiser indikatorer",
          title = conf$app_text$tooltip$publish
        ),
        publish_ui("publ")
      ),
      shiny::tabPanel(
        value = "download",
        shiny::span("Last ned data", title = conf$app_text$tooltip$download),
        download_ui("download")
      ),
      shiny::tabPanel(
        value = "indicator",
        shiny::span("Indikator",
          title = conf$app_text$tooltip$indicator,
          id = "indicator"
        ),
        indicator_ui("ind")
      ),
      shiny::tabPanel(
        value = "review",
        shiny::span("Ekspertgruppen",
          title = conf$app_text$tooltip$review
        ),
        review_ui("review")
      ),
      shiny::tabPanel(
        value = "project",
        shiny::span("Kvalitetsforbedring",
          title = conf$app_text$tooltip$project
        ),
        project_ui("project")
      ),
      shiny::tabPanel(
        value = "selected_indicators",
        shiny::span("Utvalgte indikatorer",
          title = conf$app_text$tooltip$selected_indicators
        ),
        selected_indicators_ui("selected_indicators")
      ),
      shiny::navbarMenu(
        "Administrative verkt\u00f8y",
        shiny::tabPanel(
          value = "medfield",
          shiny::span("Fagomr\u00e5der",
            title = conf$app_text$tooltip$medfield,
            id = "medfield"
          ),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              width = 3,
              shiny::uiOutput("select_medfield_context"),
              shiny::uiOutput("select_medfield_registry"),
              shiny::uiOutput("select_registry_medfield"),
              shiny::actionButton("update_medfield",
                label = "Oppdat\u00e9r",
                icon = shiny::icon("paper-plane")
              )
            ),
            shiny::mainPanel(
              shiny::uiOutput("registry_medfield_header"),
              shiny::uiOutput("registry_medfield_summary")
            )
          )
        ),
        shiny::tabPanel(
          value = "reguser",
          shiny::span("Brukere",
            title = conf$app_text$tooltip$reguser,
            id = "reguser"
          ),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              width = 3,
              shiny::uiOutput("select_user_context"),
              shiny::uiOutput("select_user_registry"),
              shiny::uiOutput("select_registry_user"),
              shiny::actionButton("update_reguser",
                label = "Oppdat\u00e9r",
                icon = shiny::icon("paper-plane")
              )
            ),
            shiny::mainPanel(
              shiny::uiOutput("registry_user_header"),
              shiny::uiOutput("registry_user_summary")
            )
          )
        ),
        shiny::tabPanel(
          value = "adminer",
          shiny::span("Adminer",
            title = conf$app_text$tooltip$adminer,
            id = "adminer"
          ),
          shiny::mainPanel(width = 12, shiny::htmlOutput("admin_frame"))
        ),
        shiny::tabPanel(
          value = "mine_field",
          shiny::span("Minefelt!", title = conf$app_text$tooltip$mine_field),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::uiOutput("select_minefield_context"),
              shiny::uiOutput("mine_field_uc")
            ),
            shiny::mainPanel(
              shiny::p(shiny::em("System message:")),
              shiny::verbatimTextOutput("sysMessage"),
            )
          )
        ),
        shiny::tabPanel(
          value = "report",
          shiny::span("Rapport"),
          report_ui("report")
        ),
        shiny::tabPanel(
          value = "status",
          shiny::span("Registerstatus"),
          status_ui("status")
        )
      ),
      bslib::nav_spacer(),
      user_widget()
    )
  )
}
