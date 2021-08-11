
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

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gmcginnis/AirVizR")
```

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.3     ✓ dplyr   1.0.7
#> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
#> ✓ readr   2.0.0     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(AirVizR)
```

## Wrangling options:

One of three methods of data importation are documented in the
vignettes, via either API or local import; \* **API (for PurpleAir)**:
\* **Local data** + **PurpleAir**: Many of the same arguments as the API
setup can be used. You can download data from PurpleAir’s sensor
[download tool](https://www.purpleair.com/sensorlist). It should be
noted that this data does not include as much information as API data,
including meta information regarding high values or A/B monitor sensor
diaprity. + **FRM**: The examples in this package use FRM data from
Oregon’s DEQ

## Exaple Visualizations:

### Map Spatio-Temporal Data

``` r
map_stad(july_api_daily, pm25_atm, location_data = july_api_meta, grouping_vars = "date_tag")
#> [1] "Daily set detected: x-axis will map across in units of 24 hours, with axis breaks each day"
#> Joining, by = "site_id"
#> [1] "Data now grouped and averaged. Location data added."
#> [1] "PM 2.5 detected"
#> Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
#> [1] "Base plot created."
#> [1] "Plot now faceted by date_tag"
#> [1] "Final plot created."
```

<img src="man/figures/README-example_map_stad-1.png" width="100%" />

### Heatmaps

``` r
heatmap_single(dataset = july_api_hourly, pm25_epa_2021, "Lighthouse", location_data = july_api_meta)
#> Joining, by = "site_id"
#> [1] "PM 2.5 detected"
#> [1] "Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day"
```

<img src="man/figures/README-example_heatmap_single-1.png" width="100%" />

Avoid washed out color palettes by applying a discrete cap

``` r
#Before
heatmap_cross(july_api_hourly, pm25_epa_2021, location_data = july_api_meta)
#> [1] "All monitors will be plotted."
#> [1] "PM 2.5 detected"
#> [1] "Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day"
#> Joining, by = "site_id"
```

<img src="man/figures/README-example_heatmap_cross-1.png" width="100%" />

``` r
#After
heatmap_cross(july_api_hourly, pm25_epa_2021, location_data = july_api_meta, cap_value = 50, cap_color = "green")
#> [1] "All monitors will be plotted."
#> [1] "PM 2.5 detected"
#> [1] "Values greater than or equal to 50 in pm25_epa_2021 will be colored green"
#> [1] "Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day"
#> Joining, by = "site_id"
```

<img src="man/figures/README-example_heatmap_cross-2.png" width="100%" />

### Timeseries line graphs

#### Spatio-temporal

``` r
ts_line(july_api_hourly, pm25_atm, label_filter = "STAR", location_data = july_api_meta, add_points = TRUE)
#> Joining, by = "site_id"
#> [1] "Charts will be arranged in multiple rows and columns."
#> [1] "Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day"
#> Joining, by = "site_id"
#> [1] "Average data added."
#> [1] "PM 2.5 detected"
#> [1] "Data points will be added."
```

<img src="man/figures/README-example_ts_line-1.png" width="100%" />

#### Variation

The following uses the [timeVariation()
function](https://bookdown.org/david_carslaw/openair/sec-timeVariation.html)
from the [OpenAir package](https://bookdown.org/david_carslaw/openair/).

``` r
ts_variation(july_api_hourly, "pm25_epa_2021", include = "Lighthouse", group = "date_tag", location_data = july_api_meta)
#> Joining, by = "site_id"
#> [1] "PM 2.5 detected"
#> [1] "Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day"
#> Detected data with Daylight Saving Time.
```

<img src="man/figures/README-example_ts_variation-1.png" width="100%" /><img src="man/figures/README-example_ts_variation-2.png" width="100%" />

    #> text[GRID.text.805]
