#' Get PurpleAir Timeseries (PAT) raw data and sensor meta data
#'
#' Iterate over a list of PurpleAir sensor IDs to extract sensor meta data and timeseries data given specified start and end dates
#' @family {PA API functions}
#' @seealso \code{\link[AirSensor]{pat_downloadParseRawData()}}
#' @param id_list Character list; A list of PAS IDs
#' @param pas_input PAS object; see \link{get_area_pas}.
#' @param startdate Character; Date at which to start the collection (format: "YYYY-MM-DD")
#' @param enddate Character; Date through which the data will be collected (format: "YYYY-MM-DD")
#' @param cols Character list; List of variables of interest to keep. Site ID info will autmatically be collected
#' @return List: dataframes of sensor meta info, and a dataframe of raw data for the variables of interest
#' @examples 
#' \donttest{
#' get_area_pat(
#'   id_list = c("6c18a7181a09037c_47107", "9f6643766cd4ba03_7572"),
#'   pas_input = (example(get_area_pas))$value,
#'   startdate = "2020-07-01", enddate = "2020-07-03"
#' )
#' }
#' @importFrom magrittr %>%
#' @export
get_area_pat <- function(id_list = ids, pas_input = pas_area, startdate = input_startdate, enddate = input_enddate, cols = c("created_at", "temperature", "humidity", "pm2.5_cf1", "pm2.5_atm")){
  ## Setup for iteration
  # URL to be used to grab data
  input_baseUrl <- "https://api.thingspeak.com/channels/"
  # Number of IDs to be evaluated. Used for console messages.
  id_count <- length(id_list)
  # Starting the counts at 0; will grow upon iteration
  count <- 0 
  # Creating empty objects to be filled upon iteration appends/row-binds
  pat_single <- list()
  raw_meta <- data.frame()
  data_a <- data.frame()
  data_b <- data.frame()
  # Columns of interest
  cols <- c("created_at", "temperature", "humidity", "pm2.5_cf1", "pm2.5_atm")
  
  enddate_1 <- as.Date(enddate) + 1
  # Sequencing dates into a list by 1 week gaps due to API limitations
  date_sequence <- seq(from = as.Date(startdate), to = enddate_1, by = "week")
  # Adding end date to the end of the sequence if not already present in the set
  if (tail(date_sequence, n=1) != enddate_1) {
    date_sequence <- date_sequence %>%
      append(enddate_1)
  }
  remove(enddate_1)
  
  # Iteration
  for (id in id_list[1:id_count]) {
    
    # Message to report progress
    count <- count + 1
    print(sprintf("Working on %s (%d/%d) ...", id, count, id_count))
    print(sprintf("... from %s to %s", date_sequence[1], date_sequence[2]))
    
    # Creating single PAT for first date range
    pat_single <- AirSensor::pat_downloadParseRawData(
      id = id,
      label = NULL,
      pas = pas_input,
      startdate = date_sequence[1],
      enddate = date_sequence[2],
      baseUrl = input_baseUrl
    )
    
    raw_meta_single <- pat_single$meta %>% 
      dplyr::rename(location = DEVICE_LOCATIONTYPE) %>% 
      tidyr::drop_na(location) %>% 
      dplyr::mutate("site_id" = id)
    
    data_a_single <- pat_single$A_PRIMARY %>% 
      dplyr::select(intersect(cols, names(.))) %>% 
      dplyr::mutate("site_id" = id)
    
    data_b_single <- pat_single$B_PRIMARY %>%
      dplyr::select(intersect(cols, names(.))) %>% 
      dplyr::mutate("site_id" = id)
    
    # If there is more than a single pair of dates in the date sequence, sub-iterations will occur
    if (length(date_sequence) > 2) {
      
      for (single_position in 2:(length(date_sequence)-1) ) {
        
        print(sprintf("... from %s to %s", date_sequence[single_position], date_sequence[single_position+1]))
        
        pat_single_more <- AirSensor::pat_downloadParseRawData(
          id = id,
          label = NULL,
          pas = pas_input,
          startdate = date_sequence[single_position],
          enddate = date_sequence[single_position + 1],
          baseUrl = input_baseUrl
        )
        
        data_a_more <- pat_single_more$A_PRIMARY %>% 
          dplyr::select(intersect(cols, names(.))) %>% 
          dplyr::mutate("site_id" = id)
        
        data_b_more <- pat_single_more$B_PRIMARY %>% 
          dplyr::select(intersect(cols, names(.))) %>% 
          dplyr::mutate("site_id" = id)
        
        data_a_single <- rbind(data_a_single, data_a_more)
        data_b_single <- rbind(data_b_single, data_b_more)
        
        # Cleaning environment
        remove(pat_single_more, data_a_more, data_b_more, single_position)
      }
    }
    
    raw_meta <- rbind(raw_meta, raw_meta_single)
    data_a <- rbind(data_a, data_a_single)
    data_b <- rbind(data_b, data_b_single)
    
    # Cleaning environment
    remove(data_a_single, data_b_single, pat_single, raw_meta_single)
  }
  
  raw_data <- dplyr::full_join(data_a, data_b, # Data sets to join
                        by = c("created_at", "site_id"), # Columns to unite
                        suffix = c("_A", "_B")) %>% # Adding custom suffixes to differentiate the columns
    # Rounding for low time increments, to allow for mostly parallel A & B reports
    dplyr::mutate(datetime = lubridate::floor_date(created_at, unit = "2 minutes")) %>% 
    # Grouping for the summarization that will follow
    dplyr::group_by(site_id, datetime) %>% 
    # Summarizing all numeric columns. Removing NAs prevents values being removed for having an NA in the group
    dplyr::summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
    # Removing periods from column headers
    dplyr::rename_with(~gsub("\\.", "", .x))
  
  # Review
  print(sprintf("Successfully created %d/%d pat objects.", length(unique(raw_data$site_id)), id_count))
  remove(id_count)
  
  return(list(
    raw_meta = raw_meta,
    raw_data = raw_data
  ))
}
