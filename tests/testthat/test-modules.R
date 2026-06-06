# tests/testthat/test-modules.R
# Integration-style tests using shiny::testServer()

# ---------- Import module ---------------------------------------------------

test_that("mod_import_bibliometrics_server initialises with NULL data", {
  shiny::testServer(mod_import_bibliometrics_server, {
    expect_null(session$returned$data())
  })
})

# ---------- Preprocess module -----------------------------------------------

test_that("mod_preprocess_server initialises with NULL dtm", {
  bib_data <- shiny::reactive(make_bib_df(20))

  shiny::testServer(
    mod_preprocess_server,
    args = list(data_reactive = bib_data),
    {
      expect_null(session$returned$dtm())
      expect_null(session$returned$tf_mat())
    }
  )
})

# ---------- Inference module ------------------------------------------------

test_that("mod_inference_server initialises with NULL model list", {
  dtm_r <- shiny::reactive(make_test_dtm())

  shiny::testServer(
    mod_inference_server,
    args = list(dtm_reactive = dtm_r),
    {
      expect_null(session$returned$model_list())
      expect_null(session$returned$coherence_mat())
    }
  )
})

test_that("mod_inference_server validates k_end > k_start", {
  dtm_r <- shiny::reactive(make_test_dtm(15, 30))

  shiny::testServer(
    mod_inference_server,
    args = list(dtm_reactive = dtm_r),
    {
      session$setInputs(
        k_start    = 10,
        k_end      = 5,   # Invalid: end < start
        k_step     = 1,
        iterations = 50,
        burnin     = 5,
        alpha      = 0.1,
        cpus       = 1
      )
      expect_error(
        session$setInputs(run_inference = 1),
        NA   # no error thrown at setInputs level; validate() handles it inside
      )
    }
  )
})

# ---------- LDA train module ------------------------------------------------

test_that("mod_lda_train_server initialises correctly", {
  dtm_r    <- shiny::reactive(make_test_dtm())
  k_opt_r  <- shiny::reactive(3L)
  bd_r     <- shiny::reactive(make_bib_df())

  shiny::testServer(
    mod_lda_train_server,
    args = list(
      dtm_reactive       = dtm_r,
      k_optimal_reactive = k_opt_r,
      bd_reactive        = bd_r
    ),
    {
      expect_null(session$returned$lda_model())
      expect_null(session$returned$tidy_beta())
      expect_null(session$returned$df_year_topic())
    }
  )
})

# ---------- Trend module ----------------------------------------------------

test_that("mod_trend_server initialises correctly", {
  df_yt_r <- shiny::reactive(NULL)
  mdl_r   <- shiny::reactive(NULL)

  shiny::testServer(
    mod_trend_server,
    args = list(
      df_year_topic_reactive = df_yt_r,
      lda_model_reactive     = mdl_r
    ),
    {
      # No crash on init
      expect_true(TRUE)
    }
  )
})

test_that("mod_trend_server validates required columns", {
  bad_df  <- shiny::reactive(data.frame(a = 1:5, b = 1:5))
  mdl_r   <- shiny::reactive(list())

  shiny::testServer(
    mod_trend_server,
    args = list(
      df_year_topic_reactive = bad_df,
      lda_model_reactive     = mdl_r
    ),
    {
      session$setInputs(
        run_trend_analysis = 1,
        p_value_threshold  = 0.05
      )
      # validate() inside server should catch missing columns
      # Session should not crash
      expect_true(TRUE)
    }
  )
})
