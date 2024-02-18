# Stat302Proj2

`Stat302Proj2` is an R package for **UW STAT302** Project 2 Part 1. 

<!-- badges: start -->
  [![Travis build status](https://travis-ci.com/emilkere/Stat302Proj2.svg?branch=master)](https://travis-ci.com/emilkere/Stat302Proj2)
  [![Codecov test coverage](https://codecov.io/gh/emilkere/Stat302Proj2/branch/master/graph/badge.svg)](https://codecov.io/gh/emilkere/Stat302Proj2?branch=master)
  <!-- badges: end -->

## Installation

To download and install the package use the code below


``` r
# install.packages("devtools")
devtools::install_github("emilkere/Stat302Proj2")
library(Stat302Proj2)
```

## Use

The vignette demonstrates example usage of all main functions. You can see the vignette by using the following code:

``` r
# install.packages("devtools")
devtools::install_github("emilkere/Stat302Proj2", build_vignette = TRUE, build_opts = c())
library(Stat302Proj2)
# Use this to view the vignette in the Stat302Proj2 HTML help
help(package = "Stat302Proj2", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "Stat302Proj2")
```

## Attribution

This package redistributes data from the [Gapminder](https://www.gapminder.org/data/) project.
