#' Get IDs from a PAS
#'
#' Load a list of device deployment IDs from a provided PAS
#' @param pas A dataframe of PAS containing a column of device deployment IDs
#' @param outside Logical argument to include/exclude outdoor monitors
#' @param inside Logical argument to include/exclude indoor monitors
#' @return A list of IDs from a provided PAS
#' @examples 
#' get_ids(pas = filter(pas_load(datestamp = "20200701"), stateCode == "RI"), outside = TRUE, inside = FALSE)
#' get_ids(pas = filter(pas_load(datestamp = "20200701"), stateCode == "OR"), outside = TRUE, inside = TRUE)
#' @export
get_ids <- function(pas = pas_area, outside = include_outside, inside = include_inside){
  
  ids_outside <- pas_getDeviceDeploymentIDs(pas, isOutside = TRUE)
  ids_inside <- pas_getDeviceDeploymentIDs(pas, isOutside = FALSE)
  
  if (outside == TRUE & inside == TRUE) {
    ids <- c(ids_outside, ids_inside)
    inout_report <- print("Both indoor and outdoor sensor data will be loaded.")
  }
  if (outside == TRUE & inside == FALSE) {
    ids <- ids_outside
    inout_report <- print("Only outdoor sensor data will be loaded.")
  }
  if (outside == FALSE & inside == TRUE) {
    ids <- ids_inside
    inout_report <- print("Only indoor sensor data will be loaded.")
  }
  if (outside == FALSE & inside == FALSE) {
    inout_report <- warning("INPUT ERROR: Please set `outside` and/or `inside` to TRUE.")
    stop(inout_report)
  }
  
  # Message with feedback as to which monitors will be selected
  inout_report
  
  # Warning message if there are many monitors
  if (length(ids) > 100) {
    warning("CAUTION: More than 100 monitors selected. Data processing will take significant time. Consider applying more area filters first!")
  }
  # Message returning the number of monitors that will be selected
  print(paste("Number of selected monitors:", length(ids)))
  
  return(ids)
}
