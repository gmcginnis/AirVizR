#' Wrangle ObservAir data
#'
#' Wrangle imported ObservAir data frames.
#' @family {OA functions}
#' @seealso \link{unit_convert()}
#' @param dataset Data set for which to wrangle
#' @param drop_minutes Numeric; number of minutes at the start of each file to remove
#' @return Wrangled data frame
#' @examples 
#' \dontrun{
#' wrangle_oa(raw_oa)
#' }
#' @importFrom magrittr %>%
#' @export
wrangle_oa <- function(dataset, drop_minutes = 5){
  
  input_oa_renames <- c(
    "TS" = "datetime",
    "Iref" = "intensity_reference",
    "Isig" = "intensity_signal",
    "ATN" = "attenuation",
    "BC" = "black_carbon",
    "RH" = "humidity",
    "T" = "temperature_c",
    "FR" = "flow_rate",
    "VBat" = "battery_voltage",
    "GPSlat" = "latitude",
    "GPSlong" = "longitude"
  )
  
  dataset %>% 
    dplyr::group_by(site_id) %>% 
    dplyr::slice(-(1:((60*drop_minutes)/2))) %>% 
    plyr::rename(warn_missing = FALSE, input_oa_renames) %>% 
    dplyr::filter(
      between(black_carbon, 0, 500), #Black carbon in micrograms per cubic meter
      between(humidity, 0, 80), #Relative humidity in percent
      between(temperature_c, 0, 40), #Temperature in deg C
      between(latitude, -90, 90),
      between(longitude, -180, 180)
    ) %>% 
    dplyr::mutate(temperature = unit_convert(celsius = temperature_c))
}
