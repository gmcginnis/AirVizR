#' Wrangle PM Data
#'
#' Apply quality control functions (\link{apply_qc}), timezone adjustments (\link{unit_convert} and \link{adjust_timezone}), and temperature conversions (\link{ambient_temperature}) to a provided raw dataset.
#' @param raw_pm_data The dataset which to wrangle.
#' @return Dataframe with selected columns of interest.
#' @examples 
#' data_pm25 <- wrangle_data(raw_data)
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