#' Average Data by Time Groupings
#'
#' Average numeric values in a dataset by day and/or by hour.
#' @param dataset The dataset for which to average
#' @param by_day Logical; average data by day
#' @param by_hour Logical; average data by hour
#' @return Dataframe with numeric values averaged by specified timestamp(s)
#' @examples 
#' data_daily <- group_data(raw_data, by_day = TRUE)
#' data_hourly <- group_data(raw_data, by_day = TRUE, by_hour = FALSE)
#' @export
group_data <- function(dataset, by_day = TRUE, by_hour = FALSE) {
  
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