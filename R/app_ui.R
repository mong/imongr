#' Client (ui) for the imonger app
#'
#' @return An imonger shiny app ui object
#' @export

app_ui <- function() {

  shiny::addResourcePath("www", system.file("www", package = "imongr"))
  app_title <- "imongr"

  shiny::tagList(
    shiny::navbarPage(
      title = shiny::div(
        shiny::a(
          shiny::includeHTML(
            system.file("www/logo.svg", package = "imongr")
          )
        ), app_title
      ),
      windowTitle = app_title,
      theme = "www/bootstrap.css",
      id = "tabs",

      shiny::tabPanel("Profil",
        shiny::mainPanel(width = 12,
          shiny::htmlOutput("profile")),
          shiny::uiOutput("ui_deliveries_table")
      ),

      shiny::tabPanel("Last",
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
                                choices = c(Puktum = ".",
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
            shiny::h4("Eksempeldata:"),
            shiny::tableOutput("sample_data"),
          )
        )
      ),

      shiny::tabPanel("Loss",
        shiny::sidebarLayout(
          shiny::sidebarPanel(width = 3,
            shiny::uiOutput("select_download_registry"),
            shiny::uiOutput("select_db_table"),
            shiny::selectInput("file_format",
                               "Filformat:", c("csv", "csv2", "rds",
                                               "excel-csv",
                                               "excel-csv2")),
            shiny::downloadButton("download_db_table", "Hent fra server")
          ),
          shiny::mainPanel(
            shiny::uiOutput("ui_db_table")
          )
        )
      ),
      shiny::tabPanel("Sj\u00e6f",
        shiny::mainPanel(width = 12,
          shiny::htmlOutput("admin_frame"))
      ),
      navbar_widget(),
      shinyalert::useShinyalert()
    )
  )
}
