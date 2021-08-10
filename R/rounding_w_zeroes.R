#' Round a Value, Keeping Trailing Zeroes
#'
#' When pasting labels, R will not display trailing zeroes; this is a way to work around it. The result can be returned to a numeric class if desired.
#' @param num Numeric; value(s) to round
#' @param input_digits Numeric; the number of digits which to preserve
#' @return Character versions rounded to the number of digits specified, including trailing zeroes 
#' @examples 
#' daytime_data <- filter_df(data_pm25, hour_tag, include = c("Morning", "Afternoon"))
#' no_lighthouse <- filter_df(data_pm25, label, exclude = c("Lighthouse"))
#' @export
rounding_w_zeroes <- function(num, input_digits) {
  input_digits <- {{input_digits}}
  
  as.character(
    sprintf(
      paste0("%.", deparse(substitute(input_digits)), "f"),
      round(num, digits = input_digits)
    )
  )
}