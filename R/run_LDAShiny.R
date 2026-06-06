#' Run the LDAShiny Application
#'
#' Launches the LDAShiny Shiny application using the golem framework.
#' The function temporarily raises the maximum upload size to 500 MB for
#' the duration of the session and restores the previous value on exit.
#'
#' @param max_upload_mb Maximum file upload size in megabytes (default 500).
#'   Passed to \code{options(shiny.maxRequestSize = ...)}.
#' @param ... Additional named options forwarded to
#'   \code{\link[golem]{with_golem_options}}.
#'
#' @return A Shiny app object (invisibly).
#'
#' @examples
#' if (interactive()) {
#'   run_LDAShiny()
#' }
#'
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @export
run_LDAShiny <- function(max_upload_mb = 500, ...) {

  # Raise upload limit only for the interactive session; restore on exit.
  # CRAN policy: options() must NOT be set at package load time.
  old <- options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  on.exit(options(old), add = TRUE)

  golem::with_golem_options(
    app = shiny::shinyApp(
      ui     = app_ui,
      server = app_server
    ),
    golem_opts = list(...)
  )
}
