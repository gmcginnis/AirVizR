#' Round a Value, Keeping Trailing Zeroes
#'
#' When pasting labels, R will not display trailing zeroes; this is a way to work around it. The result can be returned to a numeric class if desired.
#' @param num Numeric; value(s) to round
#' @param input_digits Numeric; the number of digits which to preserve
#' @return Character versions rounded to the number of digits specified, including trailing zeroes 
#' @examples 
#' rounding_w_zeroes(10.501, 2) # Will output 10.50
#' rounding_w_zeroes(10.591, 2) # Will output 10.59
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