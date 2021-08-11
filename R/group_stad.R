#' Group temporal data by a specified time unit
#'
#' Average numeric values in a data set by day and/or by hour. This is intended to be applied to raw or otherwise high-frequency temporal data.
#' @param dataset The data set for which to average
#' @param by_day Logical; average data by day
#' @param by_hour Logical; average data by hour
#' @return Data frame with numeric values averaged by specified time stamp(s)
#' @examples 
#' group_stad(july_api_full)
#' group_stad(july_api_full %>% select(!starts_with("pm")), by_day = FALSE, by_hour = TRUE)
#' @export
group_stad <- function(dataset, by_day = TRUE, by_hour = FALSE) {
  
  if (by_day == FALSE & by_hour == FALSE) {
    stop("INPUT ERROR: Please set `by_day` and/or `by_hour` to TRUE.")
  } else if (by_day == TRUE & by_hour == FALSE) {
    print("Grouping by date (24 hour averages, by day) [DEFAULT]")
    dataset <- dataset %>% column_dt("date")
    groupings <- vars(site_id, date)
  } else if (by_day == TRUE & by_hour == TRUE) {
    print("Grouping by date and hour (1 hour averages, by day)")
    dataset <- dataset %>% column_dt("date_hour")
    groupings <- vars(site_id, date_hour)
  } else if (by_day == FALSE & by_hour == TRUE) {
    print("Grouping by hour (1 hour averages)")
    dataset <- dataset %>% column_dt("hour")
    groupings <- vars(site_id, hour)
  }
  
  dataset <- dataset %>% 
    group_by_at(groupings) %>% 
    summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
    rowwise()
  
  return(dataset)
}