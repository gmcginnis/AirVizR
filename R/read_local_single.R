#' Read a single local PurpleAir document of interest
#'
#' Read specific file by location, skipping columns as needed; extracting info from title. Intended to be used in iteration, as with \link{read_local_folder}.
#' @family {PA local functions}
#' @seealso \code{\link{read_local_folder()}} for reading a full folder.
#' @param folder_path Character; a file path to the folder containing the file of interest
#' @param single_file_path Character; the full file path to the file of interest
#' @param selects Character list of the renamed variables to keep. Time stamp automatically kept.
#' @return Data frame of input file with relevant info in columns
#' @examples 
#' \dontrun{
#' read_local_single("data-raw/local", "data-raw/local/36th & SE Woodward (outside) (45.501722 -122.626231) Primary Real Time 07_01_2020 07_07_2020.csv")
#' }
#' @importFrom magrittr %>%
#' @export
read_local_single <- function(folder_path, single_file_path, selects = c("pm25_cf1", "pm25_atm", "temperature", "humidity")){
  
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
  
  data.table::fread(single_file_path,
        sep = ",",
        fill = TRUE) %>% 
    dplyr::select(!intersect(dplyr::all_of(skips), names(.))) %>% 
    plyr::rename(warn_missing = FALSE, renames) %>% 
    dplyr::select(created_at, intersect(dplyr::all_of(selects), names(.))) %>%
    dplyr::mutate(datetime = as.POSIXct(created_at, tz = "UTC")) %>% 
    dplyr::select(!created_at) %>% 
    dplyr::mutate(
      source = single_file_path,
      loc_path = stringr::str_extract(source, "\\w.+(?= \\((outside|inside|undefined))"),
      label = stringr::str_replace(loc_path, paste(folder_path, "/", sep = ""), ""),
      label = stringr::str_replace(label, " B$", ""),
      ab = stringr::str_extract(source, "B(?= \\((outside|inside|undefined))"),
      ab = tidyr::replace_na(ab, "A"),
      ab = factor(ab, levels = c("A", "B"))
    ) %>% 
    dplyr::select(!c(source, loc_path)) %>% 
    # Adding site_id variable so that same functions from API can also apply here
    dplyr::rename(site_id = label)
}
