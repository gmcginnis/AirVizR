#' Wrangle local raw PurpleAir data frames
#' 
#' Applying various quality control functions, time rounding, and pivots to provide a more managable data frame of spatio-temporal atmospheric data.
#' @family {PA local functions}
#' @seealso \code{\link{apply_qc()}}, \code{\link{adjust_temperature()}}, \code{\link{ambient_temperature()}}
#' @param data_local_raw Character; the file path for the folder that contains the monitor data frames
#' @param data_local_meta Character, optional; filter for a specific location (by label in the file title)
#' @return A data frame with quality-controlled values (\link{apply_qc}), timezone correction (\link{adjust_timezone}), and ambient temperature calculations (\link{ambient temperature}).
#' @examples 
#' \donttest{
#' example_local_raw <- read_local_folder(folder_path = "data-raw/local")
#' example_local_meta <- read_local_meta(folder_path = "data-raw/local", timezone = "America/Los_Angeles")
#' wrangle_local(example_local_raw, example_local_meta)
#' remove(example_local_raw, example_local_meta)
#' }
#' @importFrom magrittr %>%
#' @export
wrangle_local <- function(data_local_raw, data_local_meta){
  cols_to_pivot <- data_local_raw %>%
    # Selecting one row, for ease of processing
    dplyr::slice(1) %>% 
    # Selecting all numeric columns
    dplyr::select_if(is.numeric) %>% 
    # # B does not report temperature and humidity, therefore we will not pivot them
    # select(!intersect(c("temperature", "humidity"), names(.))) %>% 
    dplyr::select(starts_with("pm")) %>% 
    names()
  
  data_local_full <- data_local_raw %>% 
    tidyr::pivot_wider(names_from = ab, values_from = dplyr::all_of(cols_to_pivot)) %>% 
    dplyr::mutate(datetime = lubridate::floor_date(datetime, unit = "2 minutes")) %>% 
    dplyr::group_by(site_id, datetime) %>% 
    dplyr::summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
    apply_qc(drop_hi = FALSE) %>% 
    adjust_timezone(location_data = data_local_meta) %>% 
    ambient_temperature() %>% 
    dplyr::select(site_id, datetime, everything())
  
  return(data_local_full)
}
