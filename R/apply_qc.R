#' Apply Quality Control Filters
#'
#' Apply basic quality control to a dataset: PM2.5 between 0 and 2000, temperature (ºF) between -40 and 185, and humidity (%) between 0 and 100.
#' Also provides an option to average PM2.5 columns A and B of a dataset
#' @param dataset The dataset for which to convert timezone data (column: "datetime")
#' @param drop_hi Logical; drop monitors for which "flag_highValue" is TRUE in the defined meta data
#' @param avg_ab Logical; create a column to average PM2.5 values of A & B monitors
#' @param loc_data The dataset containing site IDs and flag_highValue
#' @return Dataset with columns
#' @examples 
#' corrected_data <- apply_qt(raw_data)
#' @export
apply_qc <- function(dataset, drop_hi = input_drop_hi, avg_ab = TRUE, loc_data = raw_meta){
  
  if (drop_hi == TRUE) {
    hi_monitors <- loc_data %>% 
      filter(flag_highValue == TRUE)
    
    print("Monitors flagged as high value to be filtered out:")
    print(hi_monitors$label)
    
    dataset <- dataset %>% 
      filter(!site_id %in% hi_monitors$site_id)
    
    remove(hi_monitors)
  } else { print("All monitor data will be kept.") }
  
  # Creating columns to average A & B data
  if (avg_ab == TRUE) {
    dataset <- dataset %>% 
      rowwise() %>% 
      mutate(
        pm25_cf1 = mean(c(pm25_cf1_A, pm25_cf1_B), na.rm = TRUE),
        pm25_atm = mean(c(pm25_atm_A, pm25_atm_B), na.rm = TRUE)
      ) %>% 
      ungroup()
    print("Columns for averages of A & B data added")
  }
  
  dataset <- dataset %>% 
    # Basic quality control; only physically possible values (and real numbers) are kept
    filter_at(
      # Selecting the columns that start with 'pm25'
      vars(starts_with("pm25")),
      # Filtering said columns such that only values 0:2000 are kept
      all_vars(between(., 0, 2000))
    ) %>% 
    filter(
      # Filtering temperature for -40:185
      between(temperature, -40, 185),
      # Filtering humidity for 0:100
      between(humidity, 0, 100)
    )
  
  return(dataset)
}
