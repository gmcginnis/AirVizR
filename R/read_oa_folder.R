#' Read a folder of ObservAir data
#'
#' Read a folder containing the resulting \code{.txt} file outputs from ObservAir
#' @family {OA functions}
#' @param file_folder Location of data files. Files in the folder should use the following naming conventions:
#' \describe{
#'   \item{Data...}{Data files should start with "Data". Example: \code{"Data34_2021-07-01.txt"}.}
#'   \item{Settings...}{Settings file should start with "Settings". Only have one Settings file per folder. Example: \code{Settings_Data34.txt}.}
#' }
#' @param time_zone Character; valid Olson Name of time zone.
#' Not sure which name to use? Run \code{stringr::str_subset(OlsonNames(), "string_argument")} with \code{string_argument} replaced with an area to see names.
#' It is highly recommended to use a full name rather than a \code{GMT} argument.
#' @param columns Character list, optional; List of column names as provided in the Settings file. 
#' If Settings file does not exist in the folder, you can list them here or use the provided default.
#' @return Raw data frame containing the columns (either detected from the Settings file or specified),
#' with an additional \code{site_id} column for the data source.
#' @examples 
#' \dontrun{
#' read_oa_folder("data-raw/oa_moving", "America/Los_Angeles")
#' }
#' @importFrom magrittr %>%
#' @export
read_oa_folder <- function(file_folder, time_zone,
                 columns = c("TS", "Iref", "Isig", "ATN", "BC", "RH", "T", "FR", "Vbat", "GPSlat", "GPSlong")){
  # list of files in the folders of interest
  file_list <- list.files(path = file.path(file_folder))
  # list of data files
  file_data <- stringr::str_subset(file_list, "^Data")
  
  file_settings <- stringr::str_subset(file_list, "^Settings")
  
  if(length(file_settings) == 0){
    print("Defaulting to inputted column names")
    col_names <- columns
  } else {
    print("Reading settings file for column names")
    
    col_names <- utils::read.csv(file.path(file_folder, file_settings),
                                 skip = 3,
                                 header = FALSE,
                                 row.names = 1,
                                 sep = " ") %>% 
      stringr::str_replace_all(",", "")
    
  }
  print(col_names)
  
  raw_oa_data <- data.frame()
  # iterating for instances where multiple data files exist in one folder
  for(i in file_data){
    data_temp <- readr::read_csv(file.path(file_folder, i),
                          col_names = col_names) %>% 
      dplyr::mutate(
        TS = as.POSIXct(stringr::str_replace(TS, "\\$", ""), tz = time_zone),
        site_id = as.character(i)
      )
    
    raw_oa_data <- rbind(raw_oa_data, data_temp)
    remove(data_temp)
  }
  
  if("GPSlat" %in% col_names & "GPSlong" %in% col_names){
    print("Changing latitude and longitude to numeric values")
    raw_oa_data <- raw_oa_data %>% 
      dplyr::mutate(
        GPSlat = as.numeric(GPSlat),
        GPSlong = as.numeric(GPSlong)
      )
  }
  
  return(raw_oa_data)
}
