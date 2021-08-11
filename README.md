
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AirVizR

<!-- badges: start -->
<!-- badges: end -->

The goal of AirVizR is to act as a tool for extracting, wrangling,
correcting, and visualizing spatio-temporal atmospheric data, with an
emphasis on fine particulate matter (PM2.5, i.e. Particulate Matter with
a diameter &lt;2.5 µm) data from
[PurpleAir](https://www.purpleair.com/). Correction factor equations
from the [Environmental Protection Agency (EPA)](https://www.epa.gov/)
and [Lane Regional Air Protection Agency
(LRAPA)](https://www.lrapa.org/) are implemented to allow for greatest
data accuracy. Visualization functions have been carefully selected to
maximize accessibility and data transparency.

## Installation

You can install the released version of AirVizR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("AirVizR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gmcginnis/AirVizR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(AirVizR)
## basic example code
```
