#' Adding new date/time column(s)
#'
#' Creating a new column in a data set with a specified date or time rounding/conversion
#' @param dataset The dataset for which to convert timezone data (column: "datetime")
#' @param unit The unit(s) for which to apply to the dataset. Options: "date", "date_hour", "hour", "hour_minute", "time"
#' @return Dataset with column(s) of newly specified time unit by row:
#' \describe{
#'   \item{date}{Column of the time stamp's date; class 'Date'.}
#'   \item{date_hour}{Column of the time stamp's date and time, rounded down by hour; class 'POSIXct','POSIXt'.}
#'   \item{hour}{Column of the time stamp's time, rounded down by hour; class 'hms', 'difftime'.}
#'   \item{hour_minute}{Column of the time stamp's time, rounded by minute; class 'hms', 'difftime'.}
#'   \item{time}{Column of the time stamp's time; class 'hms', 'difftime'.}
#' }
#' @examples 
#' column_dt(head(july_api_full)[1:3], c("date", "hour_minute"))
#' column_dt(head(july_api_hourly)[1:3], "hour")
#' @importFrom magrittr %>%
#' @export
column_dt <- function(dataset, unit){
  
  if (
    "datetime" %in% colnames(dataset) == FALSE &
    "date_hour" %in% colnames(dataset) == FALSE
  ) { stop("ERROR: columns `datetime` and `date_hour` not found in dataset. Execusion halted.") }
  
  for (unit_single in unit) {
    if (unit_single %in% c("date", "date_hour", "hour", "hour_minute", "time") == FALSE) {
      stop(paste0("Unrecognized unit: \`", unit_single, "\`. Please use `date`, `date_hour`, `hour`, `hour_minute`, or `time`."))
    }
  }
  
  if ("date" %in% unit) {
    if ("date_hour" %in% colnames(dataset) == TRUE) {
      dataset <- dataset %>% 
        dplyr::mutate(date = lubridate::date(date_hour))
      print("`date` column added from `date_hour`")
    } else {
      dataset <- dataset %>%
        dplyr::mutate(date = lubridate::date(datetime))
      print("`date` column added")
    }
  }
  
  if ("date_hour" %in% unit & !"date_hour" %in% colnames(dataset)) {
    dataset <- dataset %>% 
      dplyr::mutate(date_hour = lubridate::floor_date(datetime, unit = "hour"))
    print("`date_hour` column added")
  }
  
  if ("time" %in% unit) {
    dataset <- dataset %>% 
      dplyr::mutate(time = hms::as_hms(datetime))
    print("`time` column added")
  }
  
  if ("hour" %in% unit) {
    if ("date_hour" %in% colnames(dataset) == TRUE) {
      dataset <- dataset %>% 
        dplyr::mutate(hour = hms::as_hms(date_hour))
      print("`hour` column added")
    } else {
      dataset <- dataset %>% 
        dplyr::mutate(
          date_hour = lubridate::floor_date(datetime, unit = "hour"),
          hour = hms::as_hms(date_hour)
        ) %>% 
        select(!date_hour)
      print("`hour` column added, `date_hour` column created then disgarded")
    }
  }
  
  if ("hour_minute" %in% unit) {
    dataset <- dataset %>%
      dplyr::mutate(
        date_minute = lubridate::floor_date(datetime, unit = "minute"),
        hour_minute = hms::as_hms(date_minute)
      ) %>%
      dplyr::select(!date_minute)
    print("`hour_minute` column added, `date_minute` column created then disgarded")
  }
  
  return(dataset)
}
