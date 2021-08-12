# Libraries
library(AirVizR)

# Setup
# # Importing local Federal Reference Monitor (FRM) data
# The following example demonstrates how to wrangle local FRM data.
# It is based on the data format of the [Oregon Department of Environmental Quality (DEQ)](https://oraqi.deq.state.or.us/home/map) "Portland SE Lafayette" station, which you can download from their ["single station report" tool](https://oraqi.deq.state.or.us/report/SingleStationReport).
# Other FRM data can be imported in a similar manner, but might require customization.
# File path to the file of interest
input_frm_path <- "data-raw/frm_july.xlsx"

run_date_grouping <- TRUE
input_date_tags <- c("Before", "Independence Day", "After")
input_date_starts <- c("2020-07-01", "2020-07-04", "2020-07-05")
input_date_ends <- c("2020-07-03", "2020-07-04", "2020-07-07")

run_hour_grouping <- TRUE
input_hour_tags <- c("Morning", "Afternoon", "Evening", "Night")
input_hour_starts <- c(5, 12, 17, 21)

frm_raw_meta <- data.frame(
  label = "DEQ SE Lafayette",
  location = factor("FRM"), # The device is outside but this will label it separately
  latitude = 45.496641,
  longitude = -122.602877,
  site_id = "41-051-0080",
  timezone = "America/Los_Angeles", # Use OlsonNames() to see valid inputs.
  flag_highValue = FALSE # Being an FRM, it is not expected to be reporting poorly
)

frm_meta <- wrangle_meta(frm_raw_meta)

raw_frm <- read_frm(input_frm_path)
frm_full <- wrangle_frm(raw_frm, frm_meta)

july_frm_meta <- frm_meta
july_frm_hourly <- group_stad(frm_full, by_day = TRUE, by_hour = TRUE)
july_frm_daily <- group_stad(frm_full, by_day = TRUE, by_hour = FALSE)
july_frm_diurnal <- group_stad(frm_full, by_day = FALSE, by_hour = TRUE)
july_frm_full <- apply_hour_tags(apply_date_tags(frm_full))

usethis::use_data(july_frm_meta, overwrite = TRUE)
usethis::use_data(july_frm_full, overwrite = TRUE)
usethis::use_data(july_frm_hourly, overwrite = TRUE)
usethis::use_data(july_frm_daily, overwrite = TRUE)
usethis::use_data(july_frm_diurnal, overwrite = TRUE)
