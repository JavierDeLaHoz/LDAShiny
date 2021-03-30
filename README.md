---
output:
  pdf_document: default
  html_document: default
---
/badges[http://cranlogs.r-pkg.org}]/{LDAShiny}[?color={blue}]

<!-- README.md is generated from README.Rmd. Please edit that file -->

# LDAShiny

<!-- badges: start -->
  [![R-CMD-check](https://github.com/JavierDeLaHoz/LDAShiny/workflows/R-CMD-check/badge.svg)](https://github.com/JavierDeLaHoz/LDAShiny/actions)
<!-- badges: end -->

The goal of LDAShiny is to perform a review of the scientific literature under the Bayesian approach of latent Dirichlet allocation and machine learning algorithms. The application methodology is framed by the well known procedures in topic modelling on how to clean and process data.

## Installation

You can install the released version of LDAShiny from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("LDAShiny")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JavierDeLaHoz/LDAShiny")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LDAShiny)
runLDAShiny()
## basic example code
```

