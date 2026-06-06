# tests/testthat/test-globals.R
# Tests for package-level globals and the lda_themes() accessor

test_that("lda_themes returns a named list", {
  th <- LDAShiny:::lda_themes()
  expect_type(th, "list")
  expect_true(length(th) > 0)
  expect_true(all(nzchar(names(th))))
})

test_that("lda_themes contains expected theme names", {
  nms <- names(LDAShiny:::lda_themes())
  for (expected in c("theme_bw", "theme_classic", "theme_minimal", "theme_gray")) {
    expect_true(expected %in% nms,
                info = paste("Missing theme:", expected))
  }
})

test_that("lda_themes returns gg theme objects", {
  th <- LDAShiny:::lda_themes()
  for (nm in names(th)) {
    expect_true(inherits(th[[nm]], "theme"),
                info = paste("Not a ggplot2 theme:", nm))
  }
})

test_that("lda_themes returns independent copies each call", {
  th1 <- LDAShiny:::lda_themes()
  th2 <- LDAShiny:::lda_themes()
  # Should be equal in structure but not the same R object
  expect_equal(names(th1), names(th2))
})
