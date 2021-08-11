#' Title
#'
#' Read specific file by location, skipping columns as needed; extracting info from title
#' @param input_file Character; a file path
#' @param selects Character list of the renamed variables to keep. Time stamp automatically kept.
#' @return Data frame of input file with relevant info in columns
#' @importFrom data.table fread
#' @importFrom tidyr replace_na
#' @export
read_local_single <- function(input_file, selects = c("pm25_cf1", "pm25_atm", "temperature", "humidity")){
  
  # list of (ORIGINAL name) variables to skip when reading file
  skips <- c(
    "entry_id",
    "UptimeMinutes",
    "V11" #Auto added by fread b/c of how the CSVs are stored
  )
  
  # List of renames, using the format of "Original name" = "new_name"
  # Caveat: do NOT rename the timestamp variable here
  renames <- c(
    "PM1.0_CF1_ug/m3" = "pm1_cf1",
    "PM2.5_CF1_ug/m3" = "pm25_cf1",
    "PM10.0_CF1_ug/m3" = "pm10_cf1",
    "UptimeMinutes" = "uptime",
    "RSSI_dbm" = "rssi",
    "Temperature_F" = "temperature",
    "Humidity_%" = "humidity",
    "PM1.0_ATM_ug/m" = "pm1_atm",
    "PM2.5_ATM_ug/m3" = "pm25_atm",
    "PM10_ATM_ug/m3" = "pm10_atm"
  )
  
  fread(input_file,
        sep = ",",
        fill = TRUE) %>% 
    select(!intersect(all_of(skips), names(.))) %>% 
    plyr::rename(warn_missing = FALSE, renames) %>% 
    select(created_at, intersect(all_of(selects), names(.))) %>%
    mutate(datetime = as.POSIXct(created_at, tz = "UTC")) %>% 
    select(!created_at) %>% 
    mutate(
      source = input_file,
      loc_path = str_extract(source, "\\w.+(?= \\((outside|inside|undefined))"),
      label = str_replace(loc_path, paste(input_path, "/", sep = ""), ""),
      label = str_replace(label, " B$", ""),
      location = str_extract(source, "B(?= \\((outside|inside|undefined))"),
      location = replace_na(location, "A"),
      location = factor(location, levels = c("A", "B"))
    ) %>% 
    select(!c(source, loc_path)) %>% 
    # Adding site_id variable so that same functions from API can also apply here
    dplyr::rename(site_id = label)
}
