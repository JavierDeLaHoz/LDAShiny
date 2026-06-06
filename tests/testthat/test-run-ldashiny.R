# tests/testthat/test-run-ldashiny.R
# Tests for the run_LDAShiny() entry point and package structure

test_that("run_LDAShiny exists and is a function", {
  expect_true(is.function(run_LDAShiny))
})

test_that("run_LDAShiny returns a shiny app object in non-interactive mode", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  # In non-interactive sessions, shinyApp() creates the object but does not run.
  app <- run_LDAShiny()
  expect_true(inherits(app, "shiny.appobj"))
})

test_that("run_LDAShiny restores options on exit", {
  old_opt <- getOption("shiny.maxRequestSize")
  withr::with_options(
    list(shiny.maxRequestSize = 1024),
    {
      run_LDAShiny(max_upload_mb = 100)
      # After the call the option should be back to 1024 (set by withr)
      expect_equal(getOption("shiny.maxRequestSize"), 1024)
    }
  )
})

test_that("mod_lda_train placeholder returns invisibly NULL", {
  expect_null(mod_lda_train())
})

test_that("all exported UI functions are callable without error", {
  expect_silent(mod_about_ui("test_id"))
  expect_silent(mod_import_bibliometrics_ui("test_id"))
  expect_silent(mod_preprocess_ui("test_id"))
  expect_silent(mod_inference_ui("test_id"))
  expect_silent(mod_lda_train_ui("test_id"))
  expect_silent(mod_trend_ui("test_id"))
})
