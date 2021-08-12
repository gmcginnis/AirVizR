#' Apply LRAPA Corrections
#'
#' Apply LRAPA correction factors to PurpleAir PM2.5 data.
#' \href{https://www.lrapa.org/}{LRAPA} established PM2.5 correction factors for PurpleAir's PM2.5 CF=ATM values.
#' Unlike the EPA correction factors (\link{apply_epa}) which uses CF=1 PM2.5 values, the LRAPA correction factor uses CF=ATM values. More information on this variable can be found on the \href{https://www2.purpleair.com/community/faq#hc-what-is-the-difference-between-cf-1-and-cf-atm}{PurpleAir FAQ}.
#' It should also be noted that this correction factor might not apply to all airsheds, and was developed in Oregon, where (along with much of the Pacific Northwest) woodsmoke is a major contributor to PM2.5.
#' Futhermore, this correction factor only reliably applies to PM2.5 values of ≤ 65 µg/m^3.
#' For more information on the correction factor, see the \href{https://www.lrapa.org/DocumentCenter/View/4147/PurpleAir-Correction-Summary}{LRAPA documentation}.
#' @source \url{https://www.lrapa.org/DocumentCenter/View/4147/PurpleAir-Correction-Summary}
#' @family {PA functions}
#' @param dataset The dataset for which to apply the correction factors to
#' @param by_day Logical; average data by day
#' @param by_hour Logical; average data by hour
#' @param keep_cols Logical; Keep or disgard extra columns. If FALSE, only identifying columns and LRAPA-corrected columns will remain
#' @return Dataframe with new column for LRAPA-corrected PM2.5:
#' \describe{
#'   \item{pm25_lrapa}{LRAPA-corrected PM2.5 value, calculated for values where PM2.5(CF=1) ≤ 65 µg/m^3 as 0.5 × PM2.5(CF=ATM) - 0.66}
#' }
#' @examples 
#' apply_lrapa(july_api_full)
#' \donttest{apply_lrapa(july_api_full, by_hour = TRUE, keep_cols = TRUE)}
#' @source \href{https://www.lrapa.org/DocumentCenter/View/4147/PurpleAir-Correction-Summary}{LRAPA documentation}
#' @importFrom magrittr %>%
#' @export
apply_lrapa <- function(dataset, by_day = TRUE, by_hour = FALSE, keep_cols = FALSE){
  dataset <- dataset %>% 
    group_stad(by_day, by_hour) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(pm25_lrapa = dplyr::case_when(pm25_cf1 <= 65 ~ 0.5 * pm25_atm - 0.66)) %>% 
    # Setting negative values to NA
    dplyr::mutate_at(dplyr::vars(pm25_lrapa), ~replace(., which(.<0), NA))
  
  if (keep_cols == FALSE) {
    print("Dropping extraneous columns [default]")
    dataset <- dataset %>% 
      dplyr::select(!intersect(c("temperature", "humidity",
                          "temeprature_c", "temperature_ambient", "temperature_ambient_c",
                          "pm25_cf1_A", "pm25_atm_A", "pm25_cf1_B", "pm25_atm_B",
                          "pm25_cf1", "pm25_atm"), colnames(.)))
  } else { print("Keeping all columns") }
  
  return(dataset)
}