# CRAN Submission Comments — LDAShiny 1.0.0

## Changes in this submission

* First CRAN submission under the `golem` framework (previous version was
  distributed as a standalone Shiny application, not an R package).
* Removed `beepr` dependency: audio feedback is not appropriate in a
  non-interactive package context.
* Replaced global `options(shiny.maxRequestSize = ...)` call with a
  scoped call inside `run_LDAShiny()` that restores the original value
  on exit (`on.exit(options(old))`).
* Removed `LazyData: true` from DESCRIPTION (no datasets shipped).
* Corrected `Depends: R (>= 4.1.0)` field (native pipe `|>` usage).
* All modules now use `shiny::validate()` / `shiny::req()` defensively.
* Package-level mutable state eliminated: `themes_gg` moved to a function
  `lda_themes()` that returns a fresh list on each call.
* `get()` calls replaced with safe named-list lookups.
* Added `testthat` unit tests covering helpers, module init, and trend logic.

## Test environments

* local: R 4.4.0, macOS 14 (arm64)
* R-hub: `ubuntu-clang` (R-devel), `windows-2022` (R-release)
* win-builder: R-release, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: "New submission" (first CRAN submission)

## Reverse dependencies

None (new package).
