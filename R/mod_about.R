#' About UI
#'
#' @param id Shiny module id
#' @importFrom shinydashboard box
#' @importFrom shiny NS tagList fluidRow column h2 br icon tags a strong p
#' @export
mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("About LDAShiny"),

    fluidRow(

      # 1️⃣ GENERAL DESCRIPTION
      column(
        width = 6,
        shinydashboard::box(
          width = 12,
          status = "info",
          title = tagList(icon("info-circle"), " General Description"),

          p(
            strong("LDAShiny"), " is a comprehensive Shiny application designed to streamline the entire workflow for ",
            strong("Latent Dirichlet Allocation (LDA) Topic Modeling"), " using bibliometric data. It transforms raw ",
            "output from databases like Scopus and Web of Science into actionable insights."
          ),
          p(
            "The package provides a step-by-step graphical interface covering: ",
            tags$ul(
              tags$li("Data Import and cleaning."),
              tags$li("Text Preprocessing (Stopwords, Stemming, N-grams)."),
              tags$li("Statistical Inference to determine the optimal number of topics (K)."),
              tags$li("Final LDA Model Training and detailed result analysis."),
              tags$li("Topic Trend Analysis over time.")
            )
          )
        )
      ),

      # TECHNICAL SPECIFICATIONS
      column(
        width = 6,
        shinydashboard::box(
          width = 12,
          status = "success",
          title = tagList(icon("wrench"), " Technical Specifications"),
          tags$ul(
            tags$li(strong("Version:"), " 1.0.0"),
            tags$li(strong("Framework:"), " Golem + Shiny + shinydashboard"),
            tags$li(strong("Main Packages:"), " textmineR, bibliometrix, tidytext, ggplot2")
          )
        )
      )
    ),

    # ============================
    #  NEW SECTION: CITATION
    # ============================
    fluidRow(
      column(
        width = 12,
        shinydashboard::box(
          width = 12,
          status = "warning",
          title = tagList(icon("quote-right"), " How to Cite LDAShiny"),

          p("If you use this application in academic or scientific work, please cite the following article:"),

          tags$blockquote(
            style = "font-size: 15px; color: #2c3e50; border-left: 4px solid #f39c12; padding-left: 12px;",
            strong("De la Hoz-M., J.; Fernandez-Gomez, M. J.; Mendes, S."), " (2021). ",
            em("LDAShiny: An R Package for Exploratory Review of Scientific Literature Based on a Bayesian Probabilistic Model and Machine Learning Tools."),
            " Mathematics, 9(14), 1671. ",
            tags$br(),
            "DOI: ",
            tags$a("10.3390/math9141671",
                   href = "https://doi.org/10.3390/math9141671",
                   target = "_blank")
          ),

          p("Including this citation helps support reproducibility and proper acknowledgment of the underlying methodology.")
        )
      )
    ),

    fluidRow(

      # GEMC RESEARCH GROUP
      column(
        width = 6,
        shinydashboard::box(
          width = 12,
          status = "warning",
          title = tagList(icon("university"), " GEMC Research Group"),

          tags$img(
            src = "www/Logo.jpg",
            alt = "GEMC Research Group Logo",
            style = "max-width: 50%; height: auto; display: block; margin: auto; padding-bottom: 20px;"
          ),

          p("This application was developed by the GEMC Research Group at Universidad del Magdalena."),
          p(strong("Full Name:"), " Grupo de Estadistica y Metodos Cuantitativos (GEMC)"),
          p(strong("Institution:"), " Universidad del Magdalena"),
          p("The GEMC group focuses on the research and application of quantitative methods across scientific fields."),
          p(strong("Research Lines:"),
            tags$ul(
              tags$li("Applied Statistics"),
              tags$li("Statistics Education"),
              tags$li("Mathematical Statistics")
            )
          ),
          p("For more information visit: ",
            tags$a("GEMC Website",
                   href = "https://www.unimagdalena.edu.co/grupos-de-investigacion/gemc",
                   target = "_blank"))
        )
      ),

      #  AUTHOR & CONTACT
      column(
        width = 6,
        shinydashboard::box(
          width = 12,
          status = "primary",
          title = tagList(icon("users"), " Author Contact and Credits"),

          p(strong("Main Developer:"), " Javier De La Hoz Maestre"),
          p(strong("Contact Email:"),
            tags$a("jdelahoz@unimagdalena.edu.co",
                   href = "mailto:jdelahoz@unimagdalena.edu.co")),

          tags$ul(
            tags$li(
              strong("ORCID:"), " ",
              tags$a("0000-0001-7779-0803",
                     href = "https://orcid.org/0000-0001-7779-0803",
                     target = "_blank")
            ),
            tags$li(
              strong("ResearchGate:"), " ",
              tags$a("Javier De La Hoz Maestre",
                     href = "https://www.researchgate.net/profile/Javier-Maestre-3",
                     target = "_blank")
            )
          ),

          p("Source code hosted on ",
            tags$a("GitHub",
                   href = "https://github.com/your_user/LDAShiny",
                   target = "_blank"),
            ".")
        )
      )
    )
  )
}

#' About Server
#'
#' @param id Shiny module id
#'
#' @export
mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed for static content
  })
}
