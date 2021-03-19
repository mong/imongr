#' Client (ui) for the imonger app
#'
#' @return An imonger shiny app ui object
#' @export

app_ui <- function() {

  conf <- get_config()
  shiny::addResourcePath("www", system.file("www", package = "imongr"))
  app_title <- "Data i Sykehusviseren"

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(type = "text/css",
                        paste0("body {",
                               "font-family: Arial, Helvetica, sans-serif;",
                               "color: #3b3b3b;}")),
      shiny::tags$style(type = "text/css",
                        paste0(".navbar-default { background-color: #f2f2f2;}")),
      shiny::tags$style(type = "text/css",
                        paste0(".navbar-default .navbar-brand {",
                               "color: #1b1b1b;line-height: 1;}")),
      shiny::tags$style(type = "text/css",
                        paste0(".navbar-default .navbar-nav > li > a {",
                               "color: #1b1b1b;}")),
      shiny::tags$style(type = "text/css",
                        paste0(".navbar-default .navbar-nav > .active > a:focus {",
                               "color: #000000;",
                               "background-color: #ffffff;}")),
      shiny::tags$style(type = "text/css",
                        paste0(".navbar-brand  a:hover, .navbar-brand  a:focus {",
                               "text-decoration: underline;}")),
    ),
    shiny::navbarPage(
      title = shiny::div(app_title),
      windowTitle = app_title,
      id = "tabs",

      shiny::tabPanel(value = "profile",
                      shiny::span("Profil",
                                  title = conf$app_text$tooltip$profile),
                      shiny::mainPanel(width = 12,
                                       shiny::htmlOutput("profile")),
                      shiny::checkboxInput("deliver_history",
                                           conf$profile$delivery$status),
                      shiny::uiOutput("ui_deliveries_table")
      ),

      shiny::tabPanel(value = "upload",
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

      shiny::tabPanel(value = "download",
                      shiny::span("Last ned data",
                                  title = conf$app_text$tooltip$download),
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
                          shiny::downloadButton("download_db_table",
                                                "Hent fra server")
                        ),
                        shiny::mainPanel(
                          shiny::uiOutput("ui_db_table")
                        )
                      )
      ),
      shiny::tabPanel(
        value = "adminer",
        shiny::span("Adminer",
                    title = conf$app_text$tooltip$adminer,
                    id = "adminer"),
        shiny::mainPanel(width = 12,
                         shiny::htmlOutput("admin_frame"))
      ),
      shiny::tabPanel(
        value = "mine_field",
        shiny::span("Minefelt!", title = conf$app_text$tooltip$mine_field),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::p("Tr\u00e5 forsiktig!"),
            shiny::actionButton("agg_all", "Aggreger alle data",
                                icon = shiny::icon("skull")),
            shiny::hr(),
            shiny::actionButton("clean_agg", "Rydd aggregerte data")
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
