#' Create a data frame of locally loaded monitor data
#'
#' Load PurpleAir monitor data based on locally stored data, extracting relevant information from the file names. See 
#' @param folder_path Character; the file path for the folder that contains the monitor data frames
#' @param pattern Character, optional; filter for a specific location (by label in the file title)
#' @param timezone Character of valid Olson name for timezone of the monitors
#' @return A wrangled data frame of monitor location data from a local folder of PurpleAir data
#' @examples 
#' \dontrun{
#' read_local_meta("data-raw/local", timezone = "America/Los_Angeles")
#' read_local_meta("data-raw/local", timezone = "America/Los_Angeles", pattern = "Lincoln")
#' }
#' @export
read_local_meta <- function(folder_path = input_path, pattern = NULL, timezone = input_timezone){
  
  file_list <- list.files(path = folder_path,
                          pattern = pattern,
                          full.names = TRUE)
  
  data.frame(source = file_list) %>% 
    dplyr::mutate(
      loc_path = stringr::str_extract(source, "\\w.+(?= \\((outside|inside|undefined))"),
      label = stringr::str_replace(loc_path, paste(folder_path, "/", sep = ""), ""),
      label = stringr::str_replace(label, " B$", ""),
      location = stringr::str_extract(source, "(?!\\()(outside|inside)(?=\\))"),
      latitude = stringr::str_extract(source, "(?!\\()-*\\d+\\.\\d+(?= )"),
      longitude = stringr::str_extract(source, "-*\\d+\\.\\d+(?=\\))")
    ) %>% 
    dplyr::select(!c(source, loc_path)) %>% 
    dplyr::group_by(label) %>%
    tidyr::fill(location) %>%
    unique() %>% 
    dplyr::mutate(
      location = factor(location, levels = c("outside", "inside")),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    ) %>% 
    dplyr::mutate(
      site_id = label,
      timezone = timezone,
      flag_highValue = NA
    ) %>% 
    wrangle_meta()
}
