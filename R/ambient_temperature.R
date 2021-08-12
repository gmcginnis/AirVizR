#' Ambient Temperature
#'
#' PurpleAir monitor reports include internal temperature, which is on average slightly higher than ambient temperature.
#' More information can be found on \href{https://www2.purpleair.com/community/faq#!hc-primary-and-secondary-data-header}{their FAQ}.
#' @param dataset The dataset containing temperature values to adjust
#' @param variable The column name of temperature data (in ºF) which to apply the change
#' @param change Numeric: The value which to subtract from the original temperature value
#' @return Dataset with two new columns for ambient temperature, and temperature in ºC:
#' \describe{
#'   \item{temperature}{Ambient temperature in ºF}
#'   \item{temperature_c}{Ambient temperature in ºC}
#'   \item{temperature_inernal}{Internal temperature in ºF}
#'   \item{temperature_internal_c}{Internal temperature in ºC}
#' }
#' @examples 
#' ambient_temperature(head(july_api_raw[1:3]))
#' @export
ambient_temperature <- function(dataset, variable = temperature, change = 8) {
  
  var_qt <- deparse(substitute(variable))
  
  if (var_qt %in% colnames(dataset) == TRUE & stringr::str_detect(var_qt, "_c") == FALSE) {
    dataset <- dplyr::mutate(dataset, 
                             temperature_internal = {{variable}},
                             temperature_internal_c = unit_convert(temperature_internal),
                             temperature = temperature_internal - change,
                             temperature_c = unit_convert(temperature))
                             # temperature_c = unit_convert({{variable}}),
                             # temperature_ambient = {{variable}} - change,
                             # temperature_ambient_c = unit_convert(temperature_ambient))
    
    print(paste("Ambient temperature column added using an adjustment of", change))
  } else { print("Temperature not detected, or in degrees Celsius. Please rename the variable or specify it manually.") }
  return(dataset)
}
