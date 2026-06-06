# tests/testthat/test-import-helpers.R
# Unit tests for parse_wos() and standardize_scopus()

test_that("standardize_scopus returns correct columns", {
  raw <- data.frame(
    Title         = c("Paper A", "Paper B"),
    Year          = c(2020L, 2021L),
    "Source title" = c("Journal X", "Journal Y"),  # Common Scopus header
    Abstract      = c("Abs A", "Abs B"),
    DOI           = c("10.1/a", "10.1/b"),
    stringsAsFactors = FALSE,
    check.names  = FALSE
  )
  out <- LDAShiny:::standardize_scopus(raw)

  expect_s3_class(out, "data.frame")
  expect_true(all(c("doi", "title", "year", "Journal", "abstract", "database") %in% names(out)))
  expect_equal(nrow(out), 2L)
  expect_equal(out$database[1], "Scopus")
})

test_that("standardize_scopus handles missing columns gracefully", {
  raw <- data.frame(x = 1:3, stringsAsFactors = FALSE)
  out <- LDAShiny:::standardize_scopus(raw)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  # All required columns should exist (filled with NA)
  expect_true("doi" %in% names(out))
})

test_that("parse_wos returns empty data.frame for empty file", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(character(0), tmp)
  out <- LDAShiny:::parse_wos(tmp)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  file.remove(tmp)
})

test_that("parse_wos correctly parses a minimal WoS record", {
  wos_text <- c(
    "FN Clarivate Analytics Web of Science",
    "VR 1.0",
    "",
    "TI A Great Title",
    "AU Smith, J",
    "PY 2021",
    "SO Nature",
    "AB This is the abstract text.",
    "DI 10.1038/test",
    "ER",
    ""
  )
  tmp <- tempfile(fileext = ".txt")
  writeLines(wos_text, tmp)
  out <- LDAShiny:::parse_wos(tmp)
  file.remove(tmp)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_equal(out$title,    "A Great Title")
  expect_equal(out$year,     2021L)
  expect_equal(out$Journal,  "Nature")
  expect_equal(out$doi,      "10.1038/test")
  expect_equal(out$database, "Web of Science")
})

test_that("parse_wos handles multiple records", {
  wos_text <- c(
    "TI First Paper",   "PY 2019", "SO Science",  "AB Abstract one.", "DI 10.1/first",  "ER",
    "TI Second Paper",  "PY 2020", "SO Nature",   "AB Abstract two.", "DI 10.1/second", "ER"
  )
  tmp <- tempfile(fileext = ".txt")
  writeLines(wos_text, tmp)
  out <- LDAShiny:::parse_wos(tmp)
  file.remove(tmp)

  expect_equal(nrow(out), 2L)
  expect_equal(out$title, c("First Paper", "Second Paper"))
})

test_that("parse_wos handles records without DOI", {
  wos_text <- c("TI Paper Without DOI", "PY 2022", "SO PLOS ONE", "AB Some abstract.", "ER")
  tmp <- tempfile(fileext = ".txt")
  writeLines(wos_text, tmp)
  out <- LDAShiny:::parse_wos(tmp)
  file.remove(tmp)

  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$doi) || !nzchar(trimws(as.character(out$doi))))
})
