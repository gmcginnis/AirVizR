#' Wrangle PurpleAir PM Data
#'
#' Apply quality control functions (\link{apply_qc}), timezone adjustments (\link{unit_convert} and \link{adjust_timezone}), and temperature conversions (\link{ambient_temperature}) to a provided raw dataset.
#' @param raw_pm_data The dataset which to wrangle.
#' @param raw_meta_data Monitor meta data that \link{apply_qc} and \link{adjust_timezone} will rely upon
#' @param drop_high Logical; see \code{drop_hi} in \link{apply_qc}.
#' @return Data frame with rows removed from quality control, timezone adjusted to be that of the location, and improved temperature values (ºC and ambient calculated for both ºF and ºC).
#' @examples 
#' wrangle_data(slice(july_api_raw, 1:5), july_api_raw_meta, drop_high = TRUE)
#' @importFrom magrittr %>%
#' @export
wrangle_data <- function(raw_pm_data = raw_data, raw_meta_data = raw_meta, drop_high = input_drop_hi) {
  pm_data <- raw_pm_data %>% 
    # Applying quality control
    apply_qc(drop_hi = drop_high, loc_data = raw_meta_data) %>% 
    # Adjusting to be reported time zone
    adjust_timezone(location_data = raw_meta_data) %>% 
    ambient_temperature() %>% 
    # Reordering variables for ease of reading
    dplyr::select(site_id, datetime, everything())
  print("PM data now wrangled.")
  return(pm_data)
}
