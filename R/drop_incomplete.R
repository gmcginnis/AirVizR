#' Drop Incomplete Sets
#'
#' Drop monitor data for which its time series data is incomplete compared to that of other monitors in the data set.
#' @param dataset Data set for which to filter incomplete monitor data
#' @param var_qt Character; the variable of interest (in quotation marks) for which to apply the cap
#' @return Data set with monitors without complete sets removed
#' @examples 
#' drop_incomplete(july_api_daily, "pm25_atm")
#' @importFrom magrittr %>%
#' @export
drop_incomplete <- function(dataset, var_qt) {
  dataset <- dataset %>% 
    tidyr::drop_na({{var_qt}})
  
  # Number of values expected for a complete set
  complete_num <- (dataset %>% 
                     dplyr::ungroup() %>% 
                     dplyr::count(site_id) %>% 
                     dplyr::arrange(dplyr::desc(n)) %>% 
                     dplyr::pull(n))[1]
  
  # List of monitors with incomplete sets
  to_drop <- dataset %>% 
    dplyr::ungroup() %>% 
    dplyr::count(site_id) %>% 
    dplyr::filter(n != complete_num) %>% 
    dplyr::pull(site_id)
  
  # Feedback
  print("Monitors with incomplete temporal data that will be dropped:")
  print(paste(to_drop))
  
  # Removing incomplete monitors
  dataset %>% 
    dplyr::filter(!site_id %in% to_drop)
}