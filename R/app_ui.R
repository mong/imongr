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
      shiny::tags$div(style = "position: absolute; top: -100px;",
                      shiny::textOutput("clock"))
    ),
    shiny::navbarPage(
      theme = bslib::bs_theme(
        version = 4,
        bg = "#FFFFFF",
        fg = "#1E1E1E",
        primary = "#007bff",
        secondary = "#D5D3D3",
        base_font = bslib::font_collection("Arial", "Helvetica",
                                           "sans-serif"),
        heading_font = bslib::font_collection("Arial",
                                              "Helvetica", "sans-serif"),
        font_scale = 0.95,
        spacer = "0.5rem",
        `enable-shadows` = TRUE
      ),
      title = shiny::div(app_title),
      windowTitle = app_title,
      id = "tabs",

      shiny::tabPanel(
        value = "profile",
        shiny::span("Profil",
                    title = conf$app_text$tooltip$profile),
        shiny::htmlOutput("profile"),
        shiny::checkboxInput(
          "upload_history",
          conf$profile$delivery$status$upload
        ),
        shiny::uiOutput("ui_upload_table"),
        shiny::checkboxInput(
          "publish_history",
          conf$profile$delivery$status$publish
        ),
        shiny::uiOutput("ui_publish_table")
      ),

      shiny::tabPanel(
        value = "upload",
        shiny::span("Last opp data",
                    title = conf$app_text$tooltip$upload),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::uiOutput("select_registry"),
            shiny::uiOutput("upload_file"),
            shiny::radioButtons("sep", "Kolonneseparator",
                                choices = c(Semikolon = ";",
                                            Komma = ",",
                                            Tab = "\t"),
                                selected = ";"),
            shiny::radioButtons("dec_sep", "Desimalseparator",
                                choices = c(Punktum = ".",
                                            Komma = ","),
                                selected = ","),
            shiny::radioButtons("enc", "Tegnsetting",
                                choices = c("LATIN1",
                                            "UTF-8",
                                            "Annet"),
                                selected = "UTF-8"),
            shiny::uiOutput(outputId = "other_encoding"),
            shiny::numericInput("sample_size",
                                "Antall rader vist:",
                                20,
                                min = 1,
                                max = 50),
            shiny::selectInput("sample_type",
                               "Utvalg:",
                               list(`toppen` = FALSE,
                                    `tilfeldig` = TRUE),
                               FALSE),
            shiny::dateInput("latest_update",
                             "Alle indikatorer er oppdatert per:",
                             value = Sys.Date(),
                             weekstart = 1,
                             language = "no"),
            shiny::dateInput("latest_affirm",
                             "Merk alle indikatorer som forel\u00f8pig etter:",
                             value = paste0(format(Sys.Date(), "%Y"), "-01-01"),
                             weekstart = 1,
                             language = "no"),
            shinycssloaders::withSpinner(
              shiny::textOutput("spinner"),
              color = "#18bc9c",
              color.background = "#ffffff",
              type = 7,
              proxy.height = 80),
            shiny::uiOutput("submit")
          ),

          # Main panel for displaying outputs ----
          shiny::mainPanel(
            shiny::htmlOutput("in_progress"),
            shiny::htmlOutput("error_report"),
            shiny::titlePanel("Last opp fil"),
            shiny::htmlOutput("upload_sample_text"),
            shiny::tableOutput("upload_sample"),
            shiny::h3("Veiledning"),
            shiny::htmlOutput("main_doc"),
            shiny::htmlOutput("var_doc"),
            shiny::htmlOutput("valid_ind"),
            shiny::tableOutput("valid_ind_tab"),
            shiny::h4("Eksempeldata:"),
            shiny::tableOutput("sample_data"),
          )
        )
      ),

      shiny::tabPanel(
        value = "publish",
        shiny::span(
          "Publiser indikatorer", title = conf$app_text$tooltip$publish
        ),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::uiOutput("select_publish_registry"),
            shiny::uiOutput("publish_liability"),
            shinycssloaders::withSpinner(
              shiny::textOutput("publishing"),
              color = "#18bc9c",
              color.background = "#ffffff",
              type = 7,
              proxy.height = 80),
            shiny::uiOutput("publish")
          ),
          shiny::mainPanel(
            shiny::htmlOutput("error_report_publish"),
            shiny::titlePanel("Publiser data"),
            shiny::h3("Kvalitetskontroll"),
            shiny::htmlOutput("publish_verify_doc"),
            shiny::h3("Veiledning"),
            shiny::htmlOutput("publish_main_doc")
          )
        )
      ),

      shiny::tabPanel(
        value = "download",
        shiny::span("Last ned data", title = conf$app_text$tooltip$download),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 3,
            shiny::selectInput(
              "download_context",
              "Velg datakilde for nedlasting:",
              list(
                `Kvalitetskontroll` = "verify",
                `Publiserte data` = "prod"
              )
            ),
            shiny::uiOutput("select_download_registry"),
            shiny::uiOutput("select_db_table"),
            shiny::tags$div(
              title = paste("csv (nordisk): semikolon-delt csv med komma som",
                            "desimalskilletegn"),
              shiny::selectInput(
                "file_format",
                "Filformat:",
                c("csv", "csv (nordisk)", "rds")
              )
            ),
            shiny::downloadButton("download_db_table", "Hent fra server")
          ),
          shiny::mainPanel(
            shiny::uiOutput("ui_db_table")
          )
        )
      ),
      shiny::tabPanel(
        value = "indicator",
        shiny::span("Indikator",
                    title = conf$app_text$tooltip$indicator,
                    id = "indicator"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::uiOutput("select_indicator_registry"),
            shiny::uiOutput("select_indicator"),
            shiny::uiOutput("update_indicator")
          ),
          shiny::mainPanel(
            shiny::uiOutput("edit_ind_title"),
            shiny::uiOutput("title_oversize"),
            shiny::uiOutput("edit_ind_short"),
            shiny::uiOutput("short_oversize"),
            shiny::uiOutput("edit_ind_long"),
            shiny::uiOutput("long_oversize")
          )
        )
      ),

      shiny::navbarMenu(
        "Administrative verkt\u00f8y",
        shiny::tabPanel(
          value = "settings",
          shiny::span("Innstillinger",
                      title = conf$app_text$tooltip$settings,
                      id = "settings"),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              width = 3,
              shiny::uiOutput("select_context")
            ),
            shiny::mainPanel(

            )
          )
        ),
        shiny::tabPanel(
          value = "medfield",
          shiny::span("Fagomr\u00e5der",
                      title = conf$app_text$tooltip$medfield,
                      id = "medfield"),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              width = 3,
              shiny::uiOutput("select_medfield_registry"),
              shiny::uiOutput("select_registry_medfield"),
              shiny::actionButton("update_medfield", label = "Oppdat\u00e9r",
                                  icon = shiny::icon("paper-plane"))
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
                      id = "reguser"),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              width = 3,
              shiny::uiOutput("select_user_registry"),
              shiny::uiOutput("select_registry_user"),
              shiny::actionButton("update_reguser", label = "Oppdat\u00e9r",
                                  icon = shiny::icon("paper-plane"))
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
                      id = "adminer"),
          shiny::mainPanel(width = 12, shiny::htmlOutput("admin_frame"))
        ),
        shiny::tabPanel(
          value = "mine_field",
          shiny::span("Minefelt!", title = conf$app_text$tooltip$mine_field),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::uiOutput("mine_field_uc")
            ),
            shiny::mainPanel(
              shiny::p(shiny::em("System message:")),
              shiny::verbatimTextOutput("sysMessage"),
              shiny::p(shiny::em("Function message:")),
              shiny::verbatimTextOutput("funMessage")
            )
          )
        )
      ),
      bslib::nav_spacer(),
      user_widget()
    )
  )
}
