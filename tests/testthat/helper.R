# tests/testthat/helper.R
# Shared helpers for LDAShiny tests

# Minimal bibliometric data.frame for testing
make_bib_df <- function(n = 20) {
  data.frame(
    doi      = paste0("10.1234/test.", seq_len(n)),
    title    = paste("Title", seq_len(n)),
    year     = sample(2015:2022, n, replace = TRUE),
    Journal  = sample(c("Journal A", "Journal B", "Journal C"), n, replace = TRUE),
    abstract = paste(
      "This paper discusses", sample(c("machine learning", "statistics",
                                       "text mining", "natural language"), n, TRUE),
      "and", sample(c("algorithms", "methods", "models", "approaches"), n, TRUE),
      "for scientific research."
    ),
    database = sample(c("Scopus", "Web of Science"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Minimal sparse DTM for testing (dgCMatrix)
make_test_dtm <- function(n_docs = 15, n_terms = 30) {
  set.seed(42)
  m <- matrix(
    sample(0:5, n_docs * n_terms, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.05, 0.03, 0.02)),
    nrow = n_docs, ncol = n_terms
  )
  rownames(m) <- paste0("doc_", seq_len(n_docs))
  colnames(m) <- paste0("term_", seq_len(n_terms))
  methods::as(m, "dgCMatrix")
}
