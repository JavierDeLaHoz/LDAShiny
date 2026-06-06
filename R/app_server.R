# ======================================================================
# app_server.R — LDAShiny
# ======================================================================

#' Application Server Logic
#'
#' @param input,output,session Shiny server parameters.
#' @importFrom shiny moduleServer
#' @noRd
app_server <- function(input, output, session) {

  # ABOUT ---------------------------------------------------------------
  mod_about_server("about")

  # IMPORT --------------------------------------------------------------
  imported <- mod_import_bibliometrics_server("import")

  # PREPROCESS ----------------------------------------------------------
  preprocessed <- mod_preprocess_server(
    id            = "preprocess",
    data_reactive = imported$data
  )

  # INFERENCE -----------------------------------------------------------
  inference <- mod_inference_server(
    id           = "inference",
    dtm_reactive = preprocessed$dtm
  )

  # FINAL LDA TRAIN -----------------------------------------------------
  lda_final <- mod_lda_train_server(
    id                 = "lda_final",
    dtm_reactive       = preprocessed$dtm,
    k_optimal_reactive = inference$k_optimal,
    bd_reactive        = imported$data
  )

  # TOPIC TRENDS --------------------------------------------------------
  mod_trend_server(
    id                     = "trend_1",
    df_year_topic_reactive = lda_final$df_year_topic,
    lda_model_reactive     = lda_final$lda_model
  )
}
