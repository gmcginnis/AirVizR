#' Wrangle federal reference monitor (FRM)
#' 
#' The following is based on inputs assuming an Excel input from \href{https://oraqi.deq.state.or.us/report/SingleStationReport}{Oregon DEQ single station report}.
#' @family {FRM functions}
#' @param file_path File path to the Excel sheet of FRM data set to wrangle.
#' @return A raw R data set, prepared to be passed to \link{wrangle_frm}.
#' @examples 
#' \donttest{
#' 2+2
#' }
#' @export
read_frm <- function(file_path = input_frm_path){
  
  # Creating a list of variable names
  frm_names <- names(
    readxl::read_excel(
      file_path,
      sheet = 1, # Selecting sheet 1
      skip = 2, # Skipping the empty rows at the top
      n_max = 0 # Selecting 0 observations, and extracting the names only
    ))
  
  # All DEQ data aside from time stamps is numeric
  # To avoid R from identifying columns with lots of missing data as logical, all columns except date time are set to numeric:
  frm_coltypes <- ifelse(grepl("Date Time", frm_names), "text", "numeric")
  
  # Creating a data frame of the data
  raw_deq <- readxl::read_excel(file_path,
                                sheet = 1, # Selecting sheet 1
                                skip = 4, # Skipping the topmost rows naming the variables, etc
                                na = "----", # NAs are reported using this symbol in the Excel file
                                col_names = frm_names, # Setting column names to be those listed above
                                col_types = frm_coltypes) # Setting column types
  
  # Data frame of units, for reference if needed.
  frm_units <- readxl::read_excel(file_path,
                                  sheet = 1, # Selecting sheet 1
                                  skip = 2, # Skipping the empty rows at the top
                                  n_max = 1) # Selecting 1 "observation" (next row)
  
  print(frm_units)
  print(head(raw_deq))
  return(raw_deq)
}
