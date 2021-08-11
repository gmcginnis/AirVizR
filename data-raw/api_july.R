# Libraries
library(AirVizR)

# Inputs
input_startdate <- "2020-07-01"
input_enddate <- "2020-07-07"
input_drop_hi <- TRUE
include_outside <- TRUE
include_inside <- TRUE

input_stateCode <- "OR"
input_west <- -122.854
input_south <- 45.4
input_east <- -122.58
input_north <- 45.6
input_labels <- c("se", "SE", "Se", "\\bSTAR\\b", "\\bPSU\\b", "(C|c)ollege", "(R|r)ow", "Richmond")
temperature_change <- 8

run_date_grouping <- TRUE
input_date_tags <- c("Before", "Independence Day", "After")
input_date_starts <- c("2020-07-01", "2020-07-04", "2020-07-05")
input_date_ends <- c("2020-07-03", "2020-07-04", "2020-07-07")

run_hour_grouping <- TRUE
input_hour_tags <- c("Morning", "Afternoon", "Evening", "Night")
input_hour_starts <- c(5, 12, 17, 21)

pas_area <- get_area_pas()
ids <- get_ids()
results <- get_area_pat()

raw_meta <- results$raw_meta
raw_data <- results$raw_data

# Wrangling the data
data_meta <- wrangle_meta(raw_meta)
data_pm <- wrangle_data(raw_data)

# Creating data sets for each grouping option
july_api_meta <- data_meta
july_api_hourly <- apply_functions(data_pm, by_day = TRUE, by_hour = TRUE)
july_api_daily <- apply_functions(data_pm, by_day = TRUE, by_hour = FALSE)
july_api_diurnal <- apply_functions(data_pm, by_day = FALSE, by_hour = TRUE)
july_api_full <- data_pm %>%
  apply_date_tags() %>% 
  apply_hour_tags()

july_api_raw <- raw_data
july_api_raw_meta <- raw_meta

usethis::use_data(july_api_raw, overwrite = TRUE)
usethis::use_data(july_api_raw_meta, overwrite = TRUE)

usethis::use_data(july_api_meta, overwrite = TRUE)
usethis::use_data(july_api_full, overwrite = TRUE)
usethis::use_data(july_api_hourly, overwrite = TRUE)
usethis::use_data(july_api_daily, overwrite = TRUE)
usethis::use_data(july_api_diurnal, overwrite = TRUE)
