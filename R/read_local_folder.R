#' Title
#'
#' Blah
#' @param folder_path Character; the file path for the folder that contains the monitor data frames
#' @param read_primary Logical; read primary data
#' @param read_secondary Logical; read secondary data
#' @param pattern Character, optional; filter for a specific location (by label in the file title)
#' @return Blah
#' @examples 
#' \dontrun{
#' read_local_folder(folder_path = "data-raw/local")
#' read_local_folder(folder_path = "data-raw/local", pattern = "Lincoln")
#' }
#' @importFrom plyr rbind.fill
#' @importFrom dplyr full_join
#' @export
read_local_folder <- function(folder_path = input_path, read_primary = TRUE, read_secondary = TRUE, pattern = NULL){
  
  file_list <- list.files(path = folder_path,
                          pattern = pattern,
                          full.names = TRUE)
  
  data_primary <- data.frame()
  data_secondary <- data.frame()
  count_active <- 0
  count_total <- length(file_list)
  
  # Loop purpose: repeat file reading process for all files in a specified path
  # Output: data frames (one primary, one secondary)
  for(file_single in file_list){
    
    if(read_primary == FALSE & read_secondary == FALSE){
      stop("INPUT ERROR: Execution halted. Please set input read options to TRUE for primary and/or secondary.")
    }
    
    count_active <- count_active + 1
    print(sprintf("Working on %d/%d (%s) ...", count_active, count_total, file_single))
    
    if (read_primary == TRUE & stringr::str_detect(file_single, "\\) Primary") == TRUE) {
      temp_primary <- read_local_single(folder_path, file_single)
      
      data_primary <- plyr::rbind.fill(data_primary, temp_primary)
      
      remove(temp_primary)
    }
    
    if (read_secondary == TRUE & stringr::str_detect(file_single, "\\) Secondary") == TRUE) {
      temp_secondary <- read_local_single(folder_path, file_single)
      data_secondary <- plyr::rbind.fill(data_secondary, temp_secondary)
      remove(temp_secondary)
    }
    remove(file_single)
  }
  
  if (read_primary == TRUE & read_secondary == TRUE) {
    raw_pa_data <- dplyr::full_join(data_primary, data_secondary)
    message_combine <- "Primary and secondary data frames now joined."
  }
  if (read_secondary == FALSE) {
    raw_pa_data <- data_primary
    message_combine <- "Only primary data selected."
  }
  if (read_primary == FALSE) {
    raw_pa_data <- data_secondary
    message_combine <- "Only secondary data selected."
  }
  print(message_combine)
  
  return(raw_pa_data)
}
