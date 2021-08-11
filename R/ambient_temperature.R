#' Ambient Temperature
#'
#' PurpleAir monitor reports include internal temperature, which is on average slightly higher than ambient temperature.
#' More information can be found on \href{https://www2.purpleair.com/community/faq#!hc-primary-and-secondary-data-header}{their FAQ}.
#' @param dataset The dataset containing temperature values to adjust
#' @param variable The column name of temperature data (in ºF) which to apply the change
#' @param change Numeric: The value which to subtract from the original temperature value
#' @return Dataset with two new columns for ambient temperature, and temperature in ºC:
#' \describe{
#'   \item{temperature_c}{Internal temperature in ºC}
#'   \item{temperature_ambient}{Ambient temperature in ºF}
#'   \item{temperature_ambient_c}{Ambient temperature in ºC}
#' }
#' @examples 
#' july_api_raw %>% 
#'   select(!starts_with("pm25")) %>% 
#'   ungroup() %>% 
#'   slice(1:5) %>% 
#'   ambient_temperature()
#' @export
ambient_temperature <- function(dataset, variable = temperature, change = 8) {
  
  var_qt <- deparse(substitute(variable))
  
  if (var_qt %in% colnames(dataset) == TRUE & str_detect(var_qt, "_c") == FALSE) {
    dataset <- dataset %>% 
      mutate(
        temperature_c = unit_convert({{variable}}),
        temperature_ambient = {{variable}} - change,
        temperature_ambient_c = unit_convert(temperature_ambient)
      )
    print(paste("Ambient temperature column added using an adjustment of", change))
  } else { print("Temperature not detected, or in degrees Celsius. Please rename the variable or specify it manually.") }
  return(dataset)
}
