#' Client (ui) for the imonger app
#'
#' @return An imonger shiny app ui object
#' @export

app_ui <- function() {

  conf <- get_config()
  shiny::addResourcePath("www", system.file("www", package = "imongr"))
  app_title <- "Data | SKDE"

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(
        type = "text/css",
        paste0("body {",
               "font-family: Arial, Helvetica, sans-serif;",
               "color: #3b3b3b;}")),
      shiny::tags$style(
        type = "text/css",
        paste0(".navbar-default { background-color: #f2f2f2;}")),
      shiny::tags$style(
        type = "text/css",
        paste0(".navbar-default .navbar-brand {",
               "color: #1b1b1b;line-height: 1;}")),
      shiny::tags$style(
        type = "text/css",
        paste0(".navbar-default .navbar-nav > li > a {",
               "color: #1b1b1b;}")),
      shiny::tags$style(
        type = "text/css",
        paste0(".navbar-default .navbar-nav > .active > a:focus {",
               "color: #000000;",
               "background-color: #ffffff;}")),
      shiny::tags$style(
        type = "text/css",
        paste0(".navbar-brand  a:hover, .navbar-brand  a:focus {",
               "text-decoration: underline;}")),
      # Heartbeat every 5 seconds, to avoid app to die when user is inactive.
      # Will be out of sight on the webpage.
      shiny::tags$div(style = "position: absolute; top: -100px;",
                      shiny::textOutput("clock"))
    ),
    shiny::navbarPage(
      title = shiny::div(app_title),
      windowTitle = app_title,
      id = "tabs",

      shiny::tabPanel(
        value = "profile",
        shiny::span("Profil",
                    title = conf$app_text$tooltip$profile),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::uiOutput("select_context")
          ),
          shiny::mainPanel(
            shiny::htmlOutput("profile"),
            shiny::checkboxInput("deliver_history",
                                 conf$profile$delivery$status)
          )
        ),
        shiny::uiOutput("ui_deliveries_table")
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
        value = "download",
        shiny::span("Last ned data", title = conf$app_text$tooltip$download),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 3,
            shiny::uiOutput("select_download_registry"),
            shiny::uiOutput("select_db_table"),
            shiny::selectInput("file_format",
                               "Filformat:",
                               c("csv", "csv (nordisk)",
                                 "excel-csv",
                                 "excel-csv (nordisk)",
                                 "rds")),
            shiny::radioButtons("loss_enc",
                                "Tegnsetting",
                                choices = c("LATIN1", "UTF-8"),
                                selected = "UTF-8"),
            shiny::downloadButton("download_db_table", "Hent fra server")
          ),
          shiny::mainPanel(
            shiny::uiOutput("ui_db_table")
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
      ),
      navbar_widget(),
      shinyalert::useShinyalert()
    )
  )
}
