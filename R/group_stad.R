#' Group temporal data by a specified time unit
#'
#' Average numeric values in a data set by day and/or by hour. This is intended to be applied to raw or otherwise high-frequency temporal data.
#' @family {miscellaneous functions}
#' @seealso \code{\link{column_dt()}}
#' @param dataset The data set for which to average
#' @param by_day Logical; average data by day
#' @param by_hour Logical; average data by hour
#' @return Data frame with numeric values averaged by specified time stamp(s)
#' @examples 
#' group_stad(july_api_full)
#' \donttest{
#' group_stad(july_api_full, by_day = FALSE, by_hour = TRUE)
#' group_stad(july_api_full, by_day = TRUE, by_hour = TRUE)
#' }
#' @importFrom magrittr %>%
#' @export
group_stad <- function(dataset, by_day = TRUE, by_hour = FALSE) {
  
  if (by_day == FALSE & by_hour == FALSE) {
    stop("INPUT ERROR: Please set `by_day` and/or `by_hour` to TRUE.")
  } else if (by_day == TRUE & by_hour == FALSE) {
    print("Grouping by date (24 hour averages, by day) [DEFAULT]")
    dataset <- column_dt(dataset, "date")
    groupings <- dplyr::vars(site_id, date)
  } else if (by_day == TRUE & by_hour == TRUE) {
    print("Grouping by date and hour (1 hour averages, by day)")
    dataset <- column_dt(dataset, "date_hour")
    groupings <- dplyr::vars(site_id, date_hour)
  } else if (by_day == FALSE & by_hour == TRUE) {
    print("Grouping by hour (1 hour averages)")
    dataset <- column_dt(dataset, "hour")
    groupings <- dplyr::vars(site_id, hour)
  }
  
  dataset <- dataset %>% 
    dplyr::group_by_at(groupings) %>% 
    dplyr::summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
    dplyr::rowwise()
  
  return(dataset)
}