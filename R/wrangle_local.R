#' Wrangle local raw PurpleAir data frames
#' 
#' Applying various quality control functions, time rounding, and pivots to provide a more managable data frame of spatio-temporal atmospheric data.
#' @param data_local_raw Character; the file path for the folder that contains the monitor data frames
#' @param data_local_meta Character, optional; filter for a specific location (by label in the file title)
#' @return A data frame with quality-controlled values (\link{apply_qc}), timezone correction (\link{adjust_timezone}), and ambient temperature calculations (\link{ambient temperature}).
#' @examples 
#' example_local_raw <- read_local_folder(file_path = paste0(path.package("AirVizR"), "/data-raw/local"))
#' example_local_meta <- read_local_meta(file_path = paste0(path.package("AirVizR"), "/data-raw/local"), timezone = "America/Los_Angeles")
#' wrangle_local(example_local_raw, example_local_meta)
#' remove(example_local_raw, example_local_meta)
#' @export
wrangle_local <- function(data_local_raw, data_local_meta){
  cols_to_pivot <- data_local_raw %>%
    # Selecting one row, for ease of processing
    slice(1) %>% 
    # Selecting all numeric columns
    select_if(is.numeric) %>% 
    # # B does not report temperature and humidity, therefore we will not pivot them
    # select(!intersect(c("temperature", "humidity"), names(.))) %>% 
    select(starts_with("pm")) %>% 
    names()
  
  data_local_full <- data_local_raw %>% 
    pivot_wider(names_from = location, values_from = all_of(cols_to_pivot)) %>% 
    mutate(datetime = floor_date(datetime, unit = "2 minutes")) %>% 
    group_by(site_id, datetime) %>% 
    summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
    apply_qc(drop_hi = FALSE) %>% 
    adjust_timezone(location_data = data_local_meta) %>% 
    ambient_temperature() %>% 
    select(site_id, datetime, everything())
  
  return(data_local_full)
}
