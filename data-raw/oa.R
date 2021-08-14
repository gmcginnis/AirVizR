# libraries
library(AirVizR)

input_timezone <- "America/Los_Angeles"

# Static data
oa_static_raw <- read_oa_folder("data-raw/oa_static", input_timezone)
oa_static_full <- dplyr::filter(wrangle_oa(oa_static_raw), longitude < 0)
oa_static_meta <- wrangle_oa_meta(oa_static_full, input_timezone, single = TRUE)

oa_static_daily <- group_stad(oa_static_full)
oa_static_hourly <- group_stad(oa_static_full, by_day = TRUE, by_hour = TRUE)
oa_static_diurnal <- group_stad(oa_static_full, by_day = FALSE, by_hour = TRUE)


# Moving data
oa_moving_raw <- read_oa_folder("data-raw/oa_moving", input_timezone)
oa_moving_full <- dplyr::filter(wrangle_oa(oa_moving_raw), longitude < 0)
oa_moving_meta <- wrangle_oa_meta(oa_moving_full, input_timezone, single = FALSE)

oa_moving_daily <- group_stad(oa_moving_full, by_day = TRUE, by_hour = FALSE)
oa_moving_hourly <- group_stad(oa_moving_full, by_day = TRUE, by_hour = TRUE)
oa_moving_diurnal <- group_stad(oa_moving_full, by_day = FALSE, by_hour = TRUE)


usethis::use_data(oa_moving_raw, overwrite = TRUE)
usethis::use_data(oa_moving_meta, overwrite = TRUE)
usethis::use_data(oa_moving_full, overwrite = TRUE)
usethis::use_data(oa_moving_daily, overwrite = TRUE)
usethis::use_data(oa_moving_hourly, overwrite = TRUE)
usethis::use_data(oa_moving_diurnal, overwrite = TRUE)

usethis::use_data(oa_static_raw, overwrite = TRUE)
usethis::use_data(oa_static_meta, overwrite = TRUE)
usethis::use_data(oa_static_full, overwrite = TRUE)
usethis::use_data(oa_static_daily, overwrite = TRUE)
usethis::use_data(oa_static_hourly, overwrite = TRUE)
usethis::use_data(oa_static_diurnal, overwrite = TRUE)
