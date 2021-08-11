#' Apply EPA & LRAPA Correction Factors
#'
#' Apply EPA correction factors to PurpleAir PM2.5 data: a combination of \link{apply_lrapa} and \link{apply_epa}.
#' @param dataset The dataset for which to apply the correction factors to
#' @param by_day Logical; average data by day
#' @param by_hour Logical; average data by hour
#' @param epa_percent Numeric; Minimum percentage of data required to be included
#' @param keep_cols Logical; Keep or disgard extra columns. If FALSE, only identifying columns and EPA-corrected columns will remain
#' @return Dataframe with new columns for EPA-corrected and LRAPA-corrected PM2.5.
#' @examples 
#' corrected_data <- apply_corrections(july_api_full)
#' corrected_hourly <- apply_corrections(july_api_full, hourly = TRUE)
#' @export
apply_corrections <- function(dataset, daily = TRUE, hourly = FALSE, epa = 75){
  epa <- apply_epa(dataset, by_day = daily, by_hour = hourly, epa_percent = epa, keep_cols = TRUE)
  print("EPA corrections applied")
  lrapa <- apply_lrapa(dataset, by_day = daily, by_hour = hourly, keep_cols = TRUE)
  print("LRAPA corrections applied")
  
  dataset_corrected <- full_join(epa, lrapa)
  print("Data frame of corrected values created")
  return(dataset_corrected)
}