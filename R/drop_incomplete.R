#' Drop Incomplete Sets
#'
#' Drop monitor data for which its time series data is incomplete compared to that of other monitors in the data set.
#' @param dataset Data set for which to filter incomplete monitor data
#' @param var_qt Character; the variable of interest (in quotation marks) for which to apply the cap
#' @return Data set with monitors without complete sets removed
#' @examples 
#' drop_incomplete(july_api_diurnal, "pm25_atm") %>% ggplot(aes(hour, site_id, fill = pm25_atm)) + geom_tile()
#' @export
drop_incomplete <- function(dataset, var_qt) {
  dataset <- dataset %>% 
    drop_na({{var_qt}})
  
  # Number of values expected for a complete set
  complete_num <- (dataset %>% 
                     ungroup() %>% 
                     count(site_id) %>% 
                     arrange(desc(n)) %>% 
                     pull(n))[1]
  
  # List of monitors with incomplete sets
  to_drop <- dataset %>% 
    ungroup() %>% 
    count(site_id) %>% 
    filter(n != complete_num) %>% 
    pull(site_id)
  
  # Feedback
  print("Monitors with incomplete temporal data that will be dropped:")
  print(paste(to_drop))
  
  # Removing incomplete monitors
  dataset %>% 
    filter(!site_id %in% to_drop)
}