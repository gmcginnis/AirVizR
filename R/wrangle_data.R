#' Wrangle PurpleAir PM Data
#'
#' Apply quality control functions (\link{apply_qc}), timezone adjustments (\link{unit_convert} and \link{adjust_timezone}), and temperature conversions (\link{ambient_temperature}) to a provided raw dataset.
#' @param raw_pm_data The dataset which to wrangle.
#' @return Data frame with rows removed from quality control, timezone adjusted to be that of the location, and improved temperature values (ºC and ambient calculated for both ºF and ºC).
#' @examples 
#' data_pm25 <- wrangle_data(slice(july_api_raw, 1:5))
#' @export
wrangle_data <- function(raw_pm_data = raw_data) {
  pm_data <- raw_pm_data %>% 
    # Applying quality control
    apply_qc() %>% 
    # Adjusting to be reported time zone
    adjust_timezone() %>% 
    mutate(temperature_c = unit_convert(temperature)) %>% 
    ambient_temperature() %>% 
    # Reordering variables for ease of reading
    select(site_id, datetime, everything())
  print("PM data now wrangled.")
  return(pm_data)
}