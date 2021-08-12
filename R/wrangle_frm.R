#' Wrangle federal reference monitor (FRM)
#' 
#' The following is based on inputs assuming an Excel input from \href{https://oraqi.deq.state.or.us/report/SingleStationReport}{Oregon DEQ single station report}.
#' @param data_frm_raw Raw FRM data set to wrangle.
#' @param data_frm_meta User-created location data for the FRM (key component: presence of a \code{site_id} and \code{timezone}).
#' @param timezone_original Character of valid Olson name; the original timezone of the raw file.
#'   For the Oregon DEQ, it is in Pacific Standard Time, meaning DST is NOT accounted for.
#'   Using Etc/GMT+8 here will mark the time stamps as the appropriate -8 ("PST").
#' @return A data frame with quality-controlled values (\link{apply_qc}), timezone correction (\link{adjust_timezone}), and ambient temperature calculations (\link{ambient temperature}).
#' @examples 
#' \dontrun{
#' 2+2
#' }
#' @importFrom magrittr %>%
#' @export
wrangle_frm <- function(data_frm_raw, data_frm_meta, timezone_original = "Etc/GMT+8") {
  
  input_deq_renames <- c(
    "PM2.5" = "pm25_deq",
    "Temperature" = "temperature_c",
    "Relative Humidity" = "humidity",
    "Wind Direction" = "wind_direction",
    "Wind Speed (mph)" = "wind_speed",
    "Black Carbon (BC6 880nm)" = "black_carbon"
  )
  
  input_site_id <- dplyr::pull(data_frm_meta, site_id)
  input_timezone <- dplyr::pull(data_frm_meta, timezone)
  
  data_frm <- data_frm_raw %>% 
    dplyr::mutate(
      datetime_standard = lubridate::parse_date_time(`Date Time`, "%H:%M %m/%d/%Y", tz = timezone_original),
      # The following will adjust time stamps for DST appropriately:
      `Date Time` = lubridate::with_tz(datetime_standard, tzone = input_timezone)
    ) %>% 
    dplyr::rename(datetime = `Date Time`) %>% 
    # Renaming variables based on inputs at the top
    plyr::rename(warn_missing = FALSE, input_deq_renames) %>% 
    dplyr::mutate(site_id = input_site_id) %>% 
    # Selecting only renamed variables in addition to time stamps
    dplyr::select(c(site_id, datetime, intersect(all_of(input_deq_renames), names(.)), datetime_standard))
  
  # Converting deg Ç (units in which DEQ reports) to deg F (units in which PA reports)
  if ("temperature_c" %in% colnames(data_frm) == TRUE & "temperature" %in% colnames(data_frm) == FALSE) {
    data_frm <- dplyr::mutate(data_frm, temperature = unit_convert(celsius = temperature_c))
  }
  return(data_frm)
}
