#' Adjust timezone
#'
#' Time zones are reported as a variable when downloading the data. Time stamps in PurpleAir data are reported in UTC. The following will convert the time stamps to said reported time zone.
#' If more than one time zone is reported in the data, the conversion will use the time zone most frequently used in the data set. This is because no more than one time zone can be applied to a date time variable.
#' @param dataset The data set for which to convert timezone data (column: "datetime")
#' @param location_data The data set containing information on the timezone (column: "timezone") to apply
#' @return Dataset with a column of corrected time stamp ("datetime") and the original ("datetime_utc")
#' @examples 
#' adjust_timezone(slice(july_api_raw, 1:5), location_data = july_api_raw_meta)
#' @export
adjust_timezone <- function(dataset, location_data = raw_meta){
  
  # Translation: If more than 1 unique timezone is reported in the data frame, then:
  if (length(unique(location_data$timezone)) > 1) {
    print("Multiple time zones reported. Timestamp will be based on the most frequent time zone reported.")
    timezone <- (location_data %>% 
                   group_by(timezone) %>% 
                   count() %>% 
                   arrange(desc(n)) %>% 
                   pull(timezone))[1]
  } else {
    timezone <- unique(location_data$timezone)
  }
  
  dataset <- dataset %>%
    # Original time stamps (in UTC) will be preserved
    rename(datetime_utc = datetime) %>% 
    mutate(datetime = lubridate::with_tz(datetime_utc, tzone = timezone))
  
  print(paste("Time zone applied:", timezone))
  return(dataset)
}
