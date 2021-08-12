#' Visualization Settings for a Date/Time Scale
#'
#' A series of arguments to detect the minimum time stamp value within a data set, and provide an output of visualization settings to apply to allow for appropriate labeling and scaling.
#' @family {visualization settings}
#' @param dataset The data set which to evaluate
#' @param start_date The start date (to be pasted into captions); will be automatically adjusted based on actual start date of the data set, if possible
#' @param end_date The end date (to be pasted into captions); will be automatically adjusted based on actual end date of the data set, if possible
#' @return List of values to be applied to various visualization methods:
#' \describe{
#'   \item{dataset}{Dataset with the minimum time stamp column header renamed 'timestamp'.}
#'   \item{lab_title_sub}{Character; Working in conjunction with \code{lab_title} from \link{settings_units}, the unit of temporal measurement of the dataset}
#'   \item{x_angle}{Numeric; the angle to place x-axis (temporal) text.}
#'   \item{x_scale}{x-axis scale (from \link[ggplot2]{scale_x_datetime}) formatting and scale breaks.}
#'   \item{date_in_set}{Logical; if date value(s) present in data set.}
#'   \item{start_date}{Date; actual minimum date of data set, regardless of function input.}
#'   \item{end_date}{Date; actual maximum date of data set, regardless of function input.}
#'   \item{lab_caption}{Character; label information for details containing date range in the set.}
#' }
#' @examples 
#' \donttest{
#' settings_results_example <- settings_dt_scale(july_api_diurnal, start_date = "2020-07-01", end_date = "2020-07-07")
#' ggplot(settings_results_example$dataset, aes(timestamp, temperature)) +
#'   settings_results_example$x_scale +
#'   labs(subtitle = settings_results_example$lab_caption)
#' remove(settings_results_example)
#' }
#' @export
settings_dt_scale <- function(dataset, start_date = input_startdate, end_date = input_enddate) {
  # Expanding axis to allow monitor labels to be closer to the data
  options_expand <- ggplot2::expansion(mult = c(0.01, 0.01))
  x_angle <- 30
  lab_title_sub <- "across time"
  
  # Setting axis breaks based on the minimum time unit in the data set
  if ("datetime" %in% colnames(dataset) == TRUE) {
    lab_title_sub <- paste0(lab_title_sub, ", unaveraged")
    dataset <- dplyr::rename(dataset, timestamp = datetime)
    x_scale <- ggplot2::scale_x_datetime(breaks = "1 day", date_labels = "%d %b", expand = options_expand)
    print("Raw set detected: x-axis will map across in units of apx. 2 minutes, with axis breaks each day")
    date_in_set <- TRUE
  } else if ("date_hour" %in% colnames(dataset) == TRUE) {
    lab_title_sub <- paste0(lab_title_sub, ", averaged hourly by day")
    dataset <- dplyr::rename(dataset, timestamp = date_hour)
    x_scale <- ggplot2::scale_x_datetime(breaks = "1 day", date_labels = "%d %b", expand = options_expand)
    print("Hourly set detected: x-axis will map across in units of hour in each day, with axis breaks each day")
    date_in_set <- TRUE
  } else if ("date" %in% colnames(dataset) == TRUE) {
    lab_title_sub <- paste0(lab_title_sub, ", averaged by day")
    dataset <- dplyr::rename(dataset, timestamp = date)
    x_scale <- ggplot2::scale_x_date(breaks = "1 day", date_labels = "%d %b", expand = options_expand)
    print("Daily set detected: x-axis will map across in units of 24 hours, with axis breaks each day")
    date_in_set <- TRUE
  } else if ("hour" %in% colnames(dataset) == TRUE) {
    lab_title_sub <- paste0(lab_title_sub, ", averaged by hour of day")
    dataset <- dplyr::mutate(dataset, time = hms::as_hms(hour), timestamp = lubridate::as_datetime(time))
    x_scale <- ggplot2::scale_x_datetime(breaks = "1 hour", date_labels = "%H:%M", expand = options_expand)
    x_angle <- 45
    print("Diurnal set detected: Data will map across by hour of day, with axis breaks each hour")
    print("Dates in the caption will default to the inputted start and end dates")
    date_in_set <- FALSE
  } else { stop("INPUT ERROR: No time-based variables found!") }
  
  if (date_in_set == TRUE) {
    start_date <- min(lubridate::date(dataset$timestamp))
    end_date <- max(lubridate::date(dataset$timestamp))
  } else {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
  }
  # Formatting dates
  start_date <- format(start_date, "%d %b %Y")
  end_date <- format(end_date, "%d %b %Y")
  
  lab_caption <- paste0("Data from ", start_date, " through ", end_date, ".")
  
  return(list(
    dataset = dataset,
    lab_title_sub = lab_title_sub,
    x_angle = x_angle,
    x_scale = x_scale,
    date_in_set = date_in_set,
    start_date = start_date,
    end_date = end_date,
    lab_caption = lab_caption
  ))
}