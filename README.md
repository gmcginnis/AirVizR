
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AirVizR

<!-- badges: start -->
<!-- badges: end -->

The goal of AirVizR is to act as a tool for extracting, wrangling,
correcting, and visualizing spatio-temporal atmospheric data, with an
emphasis on fine particulate matter (PM2.5, i.e. Particulate Matter with
a diameter &lt;2.5 µm) data from
[PurpleAir](https://www.purpleair.com/).  
Correction factor equations from the [Environmental Protection Agency
(EPA)](https://www.epa.gov/) and [Lane Regional Air Protection Agency
(LRAPA)](https://www.lrapa.org/) are implemented to allow for greatest
data accuracy.  
Visualization functions have been carefully selected to maximize
accessibility and data transparency.

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
vignettes, via either API or local import;  
\* **API** (PurpleAir only): Using a series of functions, some derived
from the [AirSensor](https://mazamascience.github.io/AirSensor/)
package, archived PurpleAir data can be pulled if provided coordinates
and date ranges of interest. See the `api-data` vignette for more
information. \* **Local data**  
+ **PurpleAir**: Many of the same arguments as the API setup can be
used. You can download data from PurpleAir’s sensor [download
tool](https://www.purpleair.com/sensorlist). It should be noted that
this data does not include as much information as API data, including
meta information regarding high values or A/B monitor sensor disparity.
See the `local-data` vignette for more information.  
+ **FRM**: Although currently no functions are explicitly dedicated to
loading FRM data, the visualization functions can be applied to them.
The examples in this package use FRM data from Oregon’s DEQ. See the
`frm-data` vignette for more information.

Many of the functions in this package are intended to work hand-in-hand
with a series of provided inputs, hence why many of the defaults inputs
for man class types are "input\_\*" or “raw\_”. See the `api-data`
vignette for a full example of inputs.

## Exaple Visualizations:

### Map Spatio-Temporal Data

To represent both the spatial and temporal aspects of atmospheric data,
maps can be used to visualize multiple monitors.  
Visual options, such as point size and background graphics, can also be
customized.

``` r
map_stad(july_api_daily, pm25_epa_2021, location_data = july_api_meta, grouping_vars = "date_tag")
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

Temporal heat maps use color to represent changes in a variable of
interest over time. Typically, single “cells” (individual observations)
of data are colored to the variable.  
The heatmap options in this package are designed for continuous numeric
variables.

#### Single monitors

To show hourly changes over time for a single monitor, data can be
visualized with the date on the x-axis, and hour of the day on the
y-axis. By default, cell data will also be supplemented with a text
label (which is color-customizable).

``` r
heatmap_single(dataset = july_api_hourly, pm25_epa_2021, "Lighthouse", location_data = july_api_meta)
#> Joining, by = "site_id"
#> [1] "PM 2.5 detected"
#> [1] "Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day"
```

<img src="man/figures/README-example_heatmap_single-1.png" width="100%" />

#### Multiple monitors

To compare multiple monitors in a set over time (of any temporal
resolution), data can be mapped to a heatmap where the time is on the
x-axis, and the y-axis is monitor labels. Monitors will be separated by
location (i.e. inside/outside), and arranged north to south, allowing
for a general representation of spatial differences.  
This visualization option is ideal for larger data sets with many
monitors. Optionally, monitors with incomplete data can be filtered
out.  
Below, an example is shown with the defaults, followed by one with only
complete monitors included and a “color cap” applied to avoid a washed
out color palette. This color “cap” can be applied to all visualization
types with continuous colors in this package.

``` r
# BEFORE dropping incomplete sets and applying a color cap
heatmap_cross(july_api_hourly, pm25_epa_2021, location_data = july_api_meta)
#> [1] "All monitors will be plotted."
#> [1] "PM 2.5 detected"
#> [1] "Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day"
#> Joining, by = "site_id"
```

<img src="man/figures/README-example_heatmap_cross-1.png" width="100%" />

``` r
# AFTER dropping incomplete sets and applying a color cap
heatmap_cross(july_api_hourly, pm25_epa_2021, location_data = july_api_meta, drop_incomplete = TRUE, cap_value = 50, cap_color = "green")
#> [1] "Monitors with incomplete temporal data that will be dropped:"
#> [1] "1b10dd8f4c3b95cf_21429" "65b3dca6d412ed31_55407" "83d4548499837cd9_43023"
#> [4] "b8a3ff485ef60d6d_7018"  "b9b7db32f74ef0bd_31197" "bde741b2b71bcfb4_15187"
#> [7] "fd592e6f0a68d9de_12821" "ff72451b47552941_23805"
#> [1] "PM 2.5 detected"
#> [1] "Values greater than or equal to 50 in pm25_epa_2021 will be colored green"
#> [1] "Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day"
#> Joining, by = "site_id"
```

<img src="man/figures/README-example_heatmap_cross-2.png" width="100%" />

### Timeseries line graphs

Time series visualizations are useful for representing changes over
time.

#### Spatio-temporal

The following is a visualization option intended to spotlight specific
monitors in a data set. Optional arguments allow for points to be added,
color caps (as above) to be applied, and show/hide maximum & minimum
values for each monitor. Additionally, data can be visualized in a
single “column” of results if desired, allowing for easier temporal
cross-comparisons.

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

A visualization option that builds upon the [timeVariation()
function](https://bookdown.org/david_carslaw/openair/sec-timeVariation.html)
from the [OpenAir package](https://bookdown.org/david_carslaw/openair/)
is also available.  
It can be modified to compare multiple groups (such as date ranges) or
multiple pollutants.

``` r
ts_variation(july_api_hourly, "pm25_epa_2021", include = "Lighthouse", group = "date_tag", location_data = july_api_meta, subset = "hour")
#> Joining, by = "site_id"
#> [1] "PM 2.5 detected"
#> [1] "Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day"
#> Detected data with Daylight Saving Time.
```

<img src="man/figures/README-example_ts_variation-1.png" width="100%" />

    #> [1] "Plot selected: hour"

<img src="man/figures/README-example_ts_variation-2.png" width="100%" />

    #> NULL
