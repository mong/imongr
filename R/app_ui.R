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

      shiny::tabPanel("Last",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::fileInput("file1", "Velg csv-fil",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv"),
            ),
            shiny::tags$hr(),
            shiny::radioButtons("sep", "Kolonneseparator",
                                choices = c(Semikolon = ";",
                                            Komma = ",",
                                            Tab = "\t"),
                                selected = ";"),

            shiny::radioButtons("dec_sep", "Desimalseparator",
                                choices = c(Puktum = ".",
                                            Komma = ","),
                                selected = ","),

            shiny::radioButtons("tegnsett", "Tegnsetting",
                                choices = c("LATIN1", "UTF-8", "Annet"),
                                selected = "UTF-8"),

            shiny::uiOutput(outputId = "other_encoding"),

            # Input: Select number of rows to display ----
            shiny::radioButtons("disp", "Vis",
                                choices = c("Første 20 rader" = "head",
                                            "20 tilfeldige rader" = "tilf20",
                                            "Alle rader" = "all"),
                                selected = "head"),

            shiny::tags$hr(),

            shinyjs::disabled(
              actionButton(inputId = "lastopp", label = "Send til server",
                           icon("paper-plane"),
                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            shinyjs::hidden(p(id = "text1", "Fiks følgende problemer med datasettet for å aktivisere opplastingsknapp: ")),
                          shiny::uiOutput("feilmeld")
          ),

          # Main panel for displaying outputs ----
          shiny::mainPanel(
            shiny::titlePanel("Last opp fil"),
            shiny::h4(conf$upload$doc$main),

            shiny::htmlOutput("var_doc"),
            shiny::br(),
            shiny::h4("Eksempeldata:"),
            # Output: Data file ----
            shiny::tableOutput("eksempel"),
            # Horizontal line ----
            shiny::tags$hr(),
            shiny::h4("Opplastet datasett:"),
            shiny::h5("Her vises en utsnitt av datasett du har lastet opp. Juster parametrene i menyen på venstresiden
         slik at norske tegn (æøå) representeres riktig og tallene fremstår slik de skal."),
            shiny::tableOutput("contents")
          )
        )
      ),

      shiny::tabPanel("Loss",
        shiny::sidebarLayout(
          shiny::sidebarPanel(width = 3,
            shiny::uiOutput("select_db_table"),
            shiny::selectInput("file_format",
                               "Filformat:", c("csv", "csv2", "rds",
                                               "excel-csv",
                                               "excel-csv2")),
            shiny::downloadButton("download_db_table", "Loss!")
          ),
          shiny::mainPanel(
            shiny::uiOutput("ui_db_table")
          )
        )
      ),
      shiny::tabPanel("Sj\u00e6f",
        shiny::mainPanel(width = 12,
          shiny::htmlOutput("admin_frame"))
      )
    )
  )
}
