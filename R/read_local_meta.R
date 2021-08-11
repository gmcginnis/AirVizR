#' Create a data frame of locally loaded monitor data
#'
#' Load PurpleAir monitor data based on locally stored data, extracting relevant information from the file names. See 
#' @param file_path Character; the file path for the folder that contains the monitor data frames
#' @param pattern Character, optional; filter for a specific location (by label in the file title)
#' @param timezone Character of valid Olson name for timezone of the monitors
#' @return A wrangled data frame of monitor location data from a local folder of PurpleAir data
#' @examples 
#' read_local_meta(file_path = paste0(path.package("AirVizR"), "/data-raw/local"), timezone = "America/Los_Angeles")
#' read_local_meta(file_path = paste0(path.package("AirVizR"), "/data-raw/local"), pattern = "Lincoln", timezone = "America/Los_Angeles")
#' @export
read_local_meta <- function(file_path = input_path, pattern = NULL, timezone = input_timezone){
  
  file_list <- list.files(path = input_path,
                          pattern = pattern,
                          full.names = TRUE)
  
  data.frame(source = file_list) %>% 
    mutate(
      loc_path = str_extract(source, "\\w.+(?= \\((outside|inside|undefined))"),
      label = str_replace(loc_path, paste(input_path, "/", sep = ""), ""),
      label = str_replace(label, " B$", ""),
      location = str_extract(source, "(?!\\()(outside|inside)(?=\\))"),
      latitude = str_extract(source, "(?!\\()-*\\d+\\.\\d+(?= )"),
      longitude = str_extract(source, "-*\\d+\\.\\d+(?=\\))")
    ) %>% 
    select(!c(source, loc_path)) %>% 
    group_by(label) %>%
    fill(location) %>%
    unique() %>% 
    mutate(
      location = factor(location, levels = c("outside", "inside")),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    ) %>% 
    mutate(
      site_id = label,
      timezone = timezone,
      flag_highValue = NA
    ) %>% 
    wrangle_meta()
}
