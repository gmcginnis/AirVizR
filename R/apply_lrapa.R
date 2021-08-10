#' Apply LRAPA Corrections
#'
#' Apply LRAPA correction factors to PurpleAir PM2.5 data.
#' \href{https://www.lrapa.org/}{LRAPA} established PM2.5 correction factors for PurpleAir's PM2.5 CF=ATM values.
#' Unlike the EPA correction factors (\link{apply_epa}) which uses CF=1 PM2.5 values, the LRAPA correction factor uses CF=ATM values. More information on this variable can be found on the \href{https://www2.purpleair.com/community/faq#hc-what-is-the-difference-between-cf-1-and-cf-atm}{PurpleAir FAQ}.
#' It should also be noted that this correction factor might not apply to all airsheds, and was developed in Oregon, where (along with much of the Pacific Northwest) woodsmoke is a major contributor to PM2.5.
#' For more information on the correction factor, see the \href{https://www.lrapa.org/DocumentCenter/View/4147/PurpleAir-Correction-Summary}{LRAPA documentation}.
#' @param dataset The dataset for which to apply the correction factors to
#' @param by_day Logical; average data by day
#' @param by_hour Logical; average data by hour
#' @param keep_cols Logical; Keep or disgard extra columns. If FALSE, only identifying columns and LRAPA-corrected columns will remain
#' @return Dataframe with new columns for LRAPA-corrected PM2.5.
#' @examples 
#' data_with_lrapa <- apply_lrapa(raw_data, keep_cols = TRUE)
#' lrapa_hourly <- apply_lrapa(raw_data, by_hour = TRUE)
#' @export
apply_lrapa <- function(dataset, by_day = TRUE, by_hour = FALSE, keep_cols = FALSE){
  dataset <- dataset %>% 
    group_data(by_day, by_hour) %>% 
    rowwise() %>% 
    mutate(pm25_lrapa = case_when(pm25_cf1 <= 65 ~ 0.5 * pm25_atm - 0.66)) %>% 
    # Setting negative values to NA
    mutate_at(vars(pm25_lrapa), ~replace(., which(.<0), NA))
  
  if (keep_cols == FALSE) {
    print("Dropping extraneous columns [default]")
    dataset <- dataset %>% 
      select(!intersect(c("temperature", "humidity",
                          "temeprature_c", "temperature_ambient", "temperature_ambient_c",
                          "pm25_cf1_A", "pm25_atm_A", "pm25_cf1_B", "pm25_atm_B",
                          "pm25_cf1", "pm25_atm"), colnames(.)))
  } else { print("Keeping all columns") }
  
  return(dataset)
}