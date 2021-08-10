#' Convert Temperature
#'
#' Convert temperature from Fahrenheit to Celsius, or vice versa.
#' @param fahrenheit Numeric: A value of temperature in ºF to be converted to ºC
#' @param celsius Numeric: A value of temperature in ºC to be converted to ºF
#' @return Value in the other unit
#' @examples 
#' unit_convert(celsius = 23)
#' dataset <- mutate(dataset, temperature_c = unit_convert(fahrenheit = temperature))
#' @export
unit_convert <- function(fahrenheit, celsius){
  if (missing(fahrenheit) == FALSE) {
    print("Converting from inputted deg F to deg C")
    result <- (fahrenheit - 32) * (5/9)
  } else if (missing(celsius) == FALSE) {
    print("Converting from inputted deg C to deg F")
    result <- (celsius * 9/5) + 32
  }
  return(result)
}