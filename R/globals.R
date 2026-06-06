# ======================================================================
# globals.R — Global variable declarations (R CMD check NOTE suppression)
# ======================================================================
# All variables used in NSE / dplyr pipelines must be declared here so
# that R CMD check does not emit "no visible binding for global variable"
# notes, which CRAN treats as warnings.

utils::globalVariables(c(
  # Import module
  "Journal", "Year", "Count", "Documents", "Metric",
  "year", ".", "doi", "title", "abstract", "database",

  # LDA train module
  "Beta", "Document", "Gamma", "Label",
  "Top_Topic", "Topic", "weight", "value", "source",

  # Trend module
  "Intensity", "Trend", "estimate", "fit", "p.value",
  "s", "x", "y",

  # Misc
  "term"
))

# ======================================================================
# Shared theme list — defined once, used across modules via getter
# ======================================================================
# IMPORTANT: Do NOT define mutable objects at package level.
# Instead expose an accessor function so each call gets a fresh list.

#' Get the named list of ggplot2 themes used across LDAShiny modules
#'
#' @return A named list of ggplot2 theme objects.
#' @keywords internal
lda_themes <- function() {
  list(
    "theme_bw"       = ggplot2::theme_bw(),
    "theme_classic"  = ggplot2::theme_classic(),
    "theme_minimal"  = ggplot2::theme_minimal(),
    "theme_void"     = ggplot2::theme_void(),
    "theme_gray"     = ggplot2::theme_gray(),
    "theme_dark"     = ggplot2::theme_dark(),
    "theme_light"    = ggplot2::theme_light(),
    "theme_linedraw" = ggplot2::theme_linedraw(),
    "theme_test"     = ggplot2::theme_test()
  )
}
