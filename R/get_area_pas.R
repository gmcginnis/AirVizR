#' Getting PAS by Area
#'
#' Load the PAS within a specified area
#' @param state_code Optional. State of monitors to include. If NULL/unspecified, all monitors specified in the above boundaries will be included regardless of state.
#' @param west The western boundary coordinate
#' @param east The eastern boundary coordinate
#' @param south The southern boundary coordinate
#' @param north The northern boundary coordinate
#' @param labels Labels of monitors to include. If NULL/unspecified, all monitors will be included. The labels are based on string detection, so for instance having "STAR" will pull all that have the word "STAR" in the label. Capitalization matters.
#' @param datestamp Important argument for loading historical data, as not all monitors might be actively reporting anymore
#' @param startdate Relevant for calculating lookback days (the maximum number of days to go back and try to load data if the requested date cannot be retrieved)
#' @return A dataframe returning all PAS within the defined area
#' @examples 
#' area_pas_defaults <- get_area_pas()
#' area_pas_custom <- get_area_pas("OR", -122.854, 45.4, -122.58, 45.6, c("se", "SE", "Se", "\\bSTAR\\b", "\\bPSU\\b"), datestamp = "2021-07-07", startdate = "2021-07-01")
#' @export
get_area_pas <- function(state_code = input_stateCode,
                         west = input_west, east = input_east, south = input_south, north = input_north,
                         labels = input_labels,
                         datestamp = input_enddate, startdate = input_startdate){
  
  setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
  
  # Stringing the selected labels as one argument to be used as a string
  labels_string <- paste0("(", paste(labels, collapse = ")|("), ")")
  
  lookback_days <- as.numeric(as.Date(datestamp) - as.Date(startdate)) + 1
  
  pas <- pas_load(
    # archival allows for retrieval of sensor info even if they are no longer reporting
    archival = TRUE,
    # datestamp is important when loading historical data, as not all monitors might be actively reporting anymore
    datestamp = str_replace_all(as.character(datestamp), "-", ""),
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
  
  pas_area <- pas_filterArea(pas = pas_state, w = west, e = east, s = south, n = north) %>% 
    pas_filter(str_detect(label, labels_string))
  
  return(pas_area)
}