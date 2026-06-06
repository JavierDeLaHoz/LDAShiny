# LDAShiny 1.0.0

## New features

* First release as an R package built with `golem`.
* Five-step pipeline: Import → Preprocess → Inference → Final Model → Trends.
* Multi-file upload: multiple Scopus CSVs and Web of Science TXTs can be merged
  in a single operation with DOI-based deduplication.
* Custom ggplot2 plots with full export control (PNG / TIFF / JPEG / PDF).
* Word cloud per topic with RColorBrewer palette support.
* Topic trend classification (HOT / COLD / EQUAL) via linear regression.

## Breaking changes

None (first release).

## Bug fixes

* `options(shiny.maxRequestSize)` is now set and restored within
  `run_LDAShiny()` only — not at package load time.
* Removed audio dependency (`beepr`) which is unsuitable for CRAN packages.
* Fixed `get(input$theme_choice)` calls replaced with safe named-list lookups
  using `lda_themes()`.
* `mod_inference_server` is now properly exported.
* `parallel::detectCores()` is now declared in NAMESPACE via `@importFrom`.
