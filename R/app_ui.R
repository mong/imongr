#' Client (ui) for the imonger app
#'
#' @return An imonger shiny app ui object
#' @export

app_ui <- function() {

  conf <- get_config()
  shiny::addResourcePath("www", system.file("www", package = "imongr"))
  app_title <- "Data i Sykehusviseren"

  shiny::tagList(
    shiny::navbarPage(
      title = shiny::div(
        app_title
      ),
      windowTitle = app_title,
      theme = "www/bootstrap.css",
      id = "tabs",

      shiny::tabPanel(value = "profile",
        shiny::span("Profil", title = conf$app_text$tooltip$profile),
        shiny::mainPanel(width = 12,
          shiny::htmlOutput("profile")),
          shiny::uiOutput("ui_deliveries_table")
      ),

      shiny::tabPanel(value = "upload",
        shiny::span("Last opp data", title = conf$app_text$tooltip$upload),
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
                                choices = c("LATIN1", "UTF-8", "Annet"),
                                selected = "UTF-8"),
            shiny::uiOutput(outputId = "other_encoding"),
            shiny::numericInput("sample_size", "Antall rader vist:", 20,
                                min = 1, max = 50),
            shiny::selectInput("sample_type", "Utvalg:",
                               list(`toppen` = FALSE, `tilfeldig` = TRUE),
                               FALSE),
            shinycssloaders::withSpinner(shiny::textOutput("spinner"),
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

      shiny::tabPanel(value = "download",
        shiny::span("Last ned data", title = conf$app_text$tooltip$download),
        shiny::sidebarLayout(
          shiny::sidebarPanel(width = 3,
            shiny::uiOutput("select_download_registry"),
            shiny::uiOutput("select_db_table"),
            shiny::selectInput("file_format",
                               "Filformat:", c("csv", "csv (nordisk)",
                                               "excel-csv",
                                               "excel-csv (nordisk)", "rds")),
            shiny::radioButtons("loss_enc", "Tegnsetting",
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
        value = "adminer",
        shiny::span("Adminer", title = conf$app_text$tooltip$adminer,
                    id = "adminer"),
        shiny::mainPanel(width = 12,
          shiny::htmlOutput("admin_frame"))
      ),
      navbar_widget(),
      shinyalert::useShinyalert()
    )
  )
}
