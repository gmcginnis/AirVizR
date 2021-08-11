# Libraries
library(AirVizR)
library(readxl)

# Setup
# # Importing local Federal Reference Monitor (FRM) data
# The following example demonstrates how to wrangle local FRM data.
# It is based on the data format of the [Oregon Department of Environmental Quality (DEQ)](https://oraqi.deq.state.or.us/home/map) "Portland SE Lafayette" station, which you can download from their ["single station report" tool](https://oraqi.deq.state.or.us/report/SingleStationReport).
# Other FRM data can be imported in a similar manner, but might require customization.
# File path to the file of interest
input_deq_path <- "data-raw/frm_july.xlsx"

run_date_grouping <- TRUE
input_date_tags <- c("Before", "Independence Day", "After")
input_date_starts <- c("2020-07-01", "2020-07-04", "2020-07-05")
input_date_ends <- c("2020-07-03", "2020-07-04", "2020-07-07")

run_hour_grouping <- TRUE
input_hour_tags <- c("Morning", "Afternoon", "Evening", "Night")
input_hour_starts <- c(5, 12, 17, 21)

data_deq_meta <- data.frame(
  label = "DEQ SE Lafayette",
  location = factor("FRM"), # The device is outside but this will label it separately
  latitude = 45.496641,
  longitude = -122.602877,
  site_id = "41-051-0080",
  timezone = "America/Los_Angeles", # Use OlsonNames() to see valid inputs.
  flag_highValue = FALSE # Being an FRM, it is not expected to be reporting poorly
)

# Input of renames. Those listed will be included in the final data frame. All others will be discarded.
# Time stamp will be automatically renamed and kept.
input_deq_renames <- c(
  "PM2.5" = "pm25_deq",
  "Temperature" = "temperature_c",
  "Relative Humidity" = "humidity",
  "Wind Direction" = "wind_direction",
  "Wind Speed (mph)" = "wind_speed",
  "Black Carbon (BC6 880nm)" = "black_carbon"
)

#Importing
# Creating a list of variable names
deq_names <- read_excel(input_deq_path,
                        sheet = 1, # Selecting sheet 1
                        skip = 2, # Skipping the empty rows at the top
                        n_max = 0) %>% names() # Selecting 0 observations, and extracting the names only

# All DEQ data aside from time stamps is numeric
# To avoid R from identifying columns with lots of missing data as logical, all columns except date time are set to numeric:
deq_coltypes <- ifelse(grepl("Date Time", deq_names), "text", "numeric")

# Creating a data frame of the data
raw_deq <- read_excel(input_deq_path,
                      sheet = 1, # Selecting sheet 1
                      skip = 4, # Skipping the topmost rows naming the variables, etc
                      na = "----", # NAs are reported using this symbol in the Excel file
                      col_names = deq_names, # Setting column names to be those listed above
                      col_types = deq_coltypes) # Setting column types

data_deq <- raw_deq %>% 
  mutate(
    # Oregon DEQ reports time stamps in Pacific Standard Time, meaning DST is NOT accounted for.
    # Using Etc/GMT+8 here will mark the time stamps as the appropriate -8 ("PST")
    datetime_standard = parse_date_time(`Date Time`, "%H:%M %m/%d/%Y", tz = "Etc/GMT+8"),
    # The following will adjust time stamps for DST appropriately:
    `Date Time` = with_tz(datetime_standard, tzone = "America/Los_Angeles")
  ) %>% 
  rename(datetime = `Date Time`) %>% 
  # Renaming variables based on inputs at the top
  plyr::rename(warn_missing = FALSE, input_deq_renames) %>% 
  mutate(site_id = data_deq_meta$site_id) %>% 
  # Selecting only renamed variables in addition to time stamps
  select(c(site_id, datetime, intersect(all_of(input_deq_renames), names(.)), datetime_standard)) %>% 
  mutate(temperature = unit_convert(celsius = temperature_c))

remove(input_deq_renames)

july_frm_meta <- data_deq_meta
july_frm_hourly <- group_stad(data_deq, by_day = TRUE, by_hour = TRUE)
july_frm_daily <- group_stad(data_deq, by_day = TRUE, by_hour = FALSE)
july_frm_diurnal <- group_stad(data_deq, by_day = FALSE, by_hour = TRUE)
july_frm_full <- data_deq %>%
  apply_date_tags() %>% 
  apply_hour_tags()

usethis::use_data(july_frm_meta, overwrite = TRUE)
usethis::use_data(july_frm_full, overwrite = TRUE)
usethis::use_data(july_frm_hourly, overwrite = TRUE)
usethis::use_data(july_frm_daily, overwrite = TRUE)
usethis::use_data(july_frm_diurnal, overwrite = TRUE)
