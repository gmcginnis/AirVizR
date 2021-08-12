#' Getting PurpleAir Sensors (PAS) by Area and Date
#'
#' Load the PAS within a specified area
#' @param state_code Character, optional. State of monitors to include. If NULL/unspecified, all monitors specified in the above boundaries will be included regardless of state.
#' @param west Numeric; The western boundary coordinate
#' @param east Numeric; The eastern boundary coordinate
#' @param south Numeric; The southern boundary coordinate
#' @param north Numeric; The northern boundary coordinate
#' @param labels Character list, optional; Labels of monitors to include. If \code{NULL}/unspecified, all monitors will be included. The labels are based on string detection (\link[stringr]{str_detect}), so for instance having "STAR" will pull all that have the word "STAR" in the label. Capitalization matters.
#' @param datestamp Character; Important argument for loading historical data, as not all monitors might be actively reporting anymore. Format: "YYYY-MM-DD".
#' @param startdate Character; Relevant for calculating lookback days (the maximum number of days to go back and try to load data if the requested date cannot be retrieved). Format: "YYYY-MM-DD".
#' @param archive_url Character; see \link[AirSensor]{setArchiveBaseUrl}. Known options include:
#' \itemize{
#'   \item "https://airsensor.aqmd.gov/PurpleAir/v1/" (default) - The archive URL which works given that inputs are at least a few months prior to current date
#'   \item "http://data.mazamascience.com/PurpleAir/v1" - The archive URL used when designing these functions, which has recently stopped working
#' }
#' @return A dataframe returning all PAS within the defined area
#' @examples 
#' get_area_pas(
#'   "OR",
#'   -122.854, 45.4, -122.58, 45.6,
#'   c("se", "SE", "Se", "\\bSTAR\\b", "\\bPSU\\b"),
#'   datestamp = "2020-07-07", startdate = "2020-07-01"
#' )
#' @import AirSensor
#' @export
get_area_pas <- function(state_code = input_stateCode,
                         west = input_west, east = input_east, south = input_south, north = input_north,
                         labels = input_labels,
                         datestamp = input_enddate, startdate = input_startdate,
                         archive_url = "https://airsensor.aqmd.gov/PurpleAir/v1/"){
  
  setArchiveBaseUrl(archive_url)
  
  # Stringing the selected labels as one argument to be used as a string
  labels_string <- paste0("(", paste(labels, collapse = ")|("), ")")
  
  lookback_days <- as.numeric(as.Date(datestamp) - as.Date(startdate)) + 1
  
  pas <- pas_load(
    # archival allows for retrieval of sensor info even if they are no longer reporting
    archival = TRUE,
    # datestamp is important when loading historical data, as not all monitors might be actively reporting anymore
    datestamp = stringr::str_replace_all(as.character(datestamp), "-", ""),
    # Retries (default: 30 days) are the maximum number of days to go back and try to load data if the requested date cannot be retrieved
    retries = lookback_days
  )
  
  # Filtering (or not) filtering by state
  if (is.null(state_code) == TRUE) {
    pas_state <- pas
    print("Sensors not filtered for a specified state code.")
  } else {
    pas_state <- pas_filter(pas, stateCode == state_code)
    print(paste("Sensors now filtered for the state of", state_code))
  }
  
  pas_area <- pas_filter(pas_filterArea(pas = pas_state, w = west, e = east, s = south, n = north),
                         stringr::str_detect(label, labels_string))
  
  return(pas_area)
}
