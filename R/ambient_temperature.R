#' Ambient Temperature
#'
#' PurpleAir monitor reports include internal temperature, which is on average slightly higher than ambient temperature.
#' More information can be found on \href{https://www2.purpleair.com/community/faq#!hc-primary-and-secondary-data-header}{their FAQ}.
#' @param dataset The dataset containing temperature values to adjust
#' @param variable The column name of temperature data (in ºF) which to apply the change
#' @param change Numeric: The value which to subtract from the original temperature value
#' @return Dataset with two new columns for ambient temperature
#' @examples 
#' dataset <- ambient_temperature(data_raw, change = 8)
#' @export
ambient_temperature <- function(dataset, variable = temperature, change = temperature_change) {
  
  var_qt <- deparse(substitute(variable))
  
  if (var_qt %in% colnames(dataset) == TRUE & str_detect(var_qt, "_c") == FALSE) {
    dataset <- dataset %>% 
      mutate(
        temperature_ambient = {{variable}} - change,
        temperature_ambient_c = unit_convert(temperature_ambient)
      )
    print(paste("Ambient temperature column added using an adjustment of", change))
  } else { print("Temperature not detected, or in degrees Celsius. Please rename the variable or specify it manually.") }
  return(dataset)
}