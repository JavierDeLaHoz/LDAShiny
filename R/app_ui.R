# ======================================================================
# app_ui.R — LDAShiny
# ======================================================================

#' Application UI
#'
#' @param request Shiny request object (used for bookmarking).
#' @return A Shiny UI definition.
#' @importFrom shiny tagList tags
#' @importFrom golem favicon bundle_resources
#' @noRd
app_ui <- function(request) {

  tagList(
    golem_add_external_resources(),

    shinydashboard::dashboardPage(
      skin = "blue",

      # ------------------------------------------------------------------
      # HEADER
      # ------------------------------------------------------------------
      shinydashboard::dashboardHeader(
        title = "LDAShiny"
      ),

      # ------------------------------------------------------------------
      # SIDEBAR
      # ------------------------------------------------------------------
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "tabs",

          shinydashboard::menuItem(
            "About",
            tabName = "about",
            icon    = shiny::icon("info-circle")
          ),
          shinydashboard::menuItem(
            "Import Data",
            tabName = "import",
            icon    = shiny::icon("upload")
          ),
          shinydashboard::menuItem(
            "Preprocess",
            tabName = "preprocess",
            icon    = shiny::icon("magic")
          ),
          shinydashboard::menuItem(
            "LDA Inference",
            tabName = "inference",
            icon    = shiny::icon("chart-line")
          ),
          shinydashboard::menuItem(
            "Final LDA Model",
            tabName = "lda_final",
            icon    = shiny::icon("cogs")
          ),
          shinydashboard::menuItem(
            "Topic Trends",
            tabName = "topic_trends",
            icon    = shiny::icon("chart-area")
          )
        )
      ),

      # ------------------------------------------------------------------
      # BODY
      # ------------------------------------------------------------------
      shinydashboard::dashboardBody(

        shinyjs::useShinyjs(),

        shinydashboard::tabItems(

          shinydashboard::tabItem(
            tabName = "about",
            mod_about_ui("about")
          ),

          shinydashboard::tabItem(
            tabName = "import",
            mod_import_bibliometrics_ui("import")
          ),

          shinydashboard::tabItem(
            tabName = "preprocess",
            mod_preprocess_ui("preprocess")
          ),

          shinydashboard::tabItem(
            tabName = "inference",
            mod_inference_ui("inference")
          ),

          shinydashboard::tabItem(
            tabName = "lda_final",
            mod_lda_train_ui("lda_final")
          ),

          shinydashboard::tabItem(
            tabName = "topic_trends",
            mod_trend_ui("trend_1")
          )
        )
      )
    )
  )
}

# ======================================================================
# EXTERNAL RESOURCES
# ======================================================================

#' Add External Resources to the UI
#'
#' @importFrom shiny addResourcePath tags
#' @importFrom golem favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  shiny::addResourcePath(
    prefix   = "www",
    directoryPath = system.file("app/www", package = "LDAShiny")
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path      = system.file("app/www", package = "LDAShiny"),
      app_title = "LDAShiny"
    )
  )
}
