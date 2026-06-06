# tests/testthat/test-trend-logic.R
# Unit tests for the trend regression logic (independent of Shiny)

# Helper: build a minimal topic-year data.frame
make_trend_df <- function() {
  data.frame(
    Topic     = rep(c("t_1", "t_2", "t_3"), each = 6),
    year      = rep(2017:2022, times = 3),
    Intensity = c(
      seq(0.05, 0.30, length.out = 6),   # t_1 increasing
      seq(0.30, 0.05, length.out = 6),   # t_2 decreasing
      rep(0.15, 6)                         # t_3 flat
    ),
    stringsAsFactors = FALSE
  )
}

classify_trends <- function(df, alpha = 0.05) {
  topics  <- unique(df$Topic)
  results <- lapply(topics, function(tid) {
    sub   <- df[df$Topic == tid, ]
    fit   <- stats::lm(Intensity ~ year, data = sub)
    coefs <- broom::tidy(fit)
    slope <- coefs[coefs$term == "year", ]
    data.frame(
      Topic    = tid,
      Estimate = slope$estimate,
      P_Value  = slope$p.value,
      Trend    = dplyr::case_when(
        slope$p.value <  alpha & slope$estimate > 0 ~ "HOT (Increasing)",
        slope$p.value <  alpha & slope$estimate < 0 ~ "COLD (Decreasing)",
        slope$p.value >= alpha                       ~ "EQUAL (Stable)",
        TRUE                                         ~ "NA"
      ),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(results)
}

test_that("trend classification correctly identifies HOT, COLD, EQUAL", {
  df  <- make_trend_df()
  res <- classify_trends(df, alpha = 0.05)

  expect_equal(nrow(res), 3L)
  expect_true("HOT (Increasing)"   %in% res$Trend)
  expect_true("COLD (Decreasing)"  %in% res$Trend)
  expect_true("EQUAL (Stable)"     %in% res$Trend)

  expect_equal(res$Trend[res$Topic == "t_1"], "HOT (Increasing)")
  expect_equal(res$Trend[res$Topic == "t_2"], "COLD (Decreasing)")
  expect_equal(res$Trend[res$Topic == "t_3"], "EQUAL (Stable)")
})

test_that("trend classification respects custom alpha threshold", {
  df  <- make_trend_df()
  # With alpha = 0 nothing is significant
  res <- classify_trends(df, alpha = 0)
  expect_true(all(res$Trend == "EQUAL (Stable)"))
})

test_that("trend data has required columns", {
  df <- make_trend_df()
  expect_true(all(c("Topic", "year", "Intensity") %in% names(df)))
})
