#' Apply EPA Corrections
#'
#' Apply EPA correction factors to PurpleAir PM2.5 data.
#' The EPA has established conditions in 2020 and late 2020 (the latter of which is tagged as "2021" in this set) for correcting PurpleAir data, including minimum data requirements and checks for A & B monitor agreement.
#' The late 2020 correction factor includes consideration for high-PM events (such as wildfires); the low-PM values have the same correction factor as 2020.
#' Unlike the LRAPA correction factor (\link{apply_lrapa}) which uses CF=ATM PM2.5 values, the major EPA correction factors use CF=1 values. If needed, an additional EPA-based output column using CF=ATM is included. More information on this variable can be found on the \href{https://www2.purpleair.com/community/faq#hc-what-is-the-difference-between-cf-1-and-cf-atm}{PurpleAir FAQ}.
#' More information about EPA correction factors can be found on the \href{https://www.epa.gov/air-sensor-toolbox/technical-approaches-sensor-data-airnow-fire-and-smoke-map}{Air Sensor Toolbox}.
#' @family {PA functions}
#' @param dataset The dataset for which to apply the correction factors to
#' @param by_day Logical; average data by day
#' @param by_hour Logical; average data by hour
#' @param epa_percent Numeric; Minimum percentage of data required to be included
#' @param keep_cols Logical; Keep or discard extra columns. If FALSE, only identifying columns and EPA-corrected columns will remain
#' @return Dataframe with new columns for EPA-corrected PM2.5, and values removed if A & B determined to be in disagreement or minimum data requirement not met.
#' \describe{
#'   \item{pm25_epa_2020}{EPA-corrected PM2.5 values, calculated as 0.524 × PM2.5(CF=1) - (0.0852 × humidity) + 5.72}
#'   \item{pm25_epa_2021}{EPA-corrected PM2.5 values, calculated as:
#'     \itemize{
#'       \item The 2020 equation (see \code{pm25_epa_2020} description) if PM2.5(CF=1) ≤ 343 µg/m^3
#'       \item 0.46 × PM2.5(CF=1) + (3.93 × 10^(-4) × PM2.5(CF=1)^2) + 2.97 for PM2.5(CF=1) > 343 µg/m^3
#'     }
#'   }
#'   \item{pm25_epa_atm}{EPA-corrected PM2.5 values, calculated as:
#'     \itemize{
#'       \item 0.25 × PM2.5(CF=ATM) - 0.086 × humidity + 5.75 when PM2.5(CF=ATM) < 50
#'       \item 0.786 × PM2.5(CF=ATM) - 0.086 × humidity + 5.75 when 50 ≤ PM2.5(CF=ATM) < 229
#'       \item 0.69 × PM2.5(CF=ATM) + 8.84 × 10^(-4) × PM2.5(CF=ATM)(^2) + 2.97 when PM2.5(CF=ATM) > 229
#'     }
#'   }
#' }
#' @examples 
#' apply_epa(july_api_full)
#' \dontrun{apply_epa(july_api_full, keep_cols = TRUE, by_hour = TRUE)}
#' @source \href{https://www.epa.gov/air-sensor-toolbox/technical-approaches-sensor-data-airnow-fire-and-smoke-map}{EPA Air Sensor Toolbox}
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
apply_epa <- function(dataset, by_day = TRUE, by_hour = FALSE, epa_percent = 75, keep_cols = FALSE) {
  
  require(dplyr)
  
  if (by_day == FALSE & by_hour == FALSE) {
    stop("INPUT ERROR: Please set `by_day` and/or `by_hour` to TRUE.")
  } else if (by_day == TRUE & by_hour == FALSE) {
    print("Grouping by date (24 hour averages, by day) [default]")
    dataset_stamped <- dataset %>% 
      column_dt(c("date", "date_hour"))
    groupings_drop <- dplyr::vars(site_id, date, date_hour)
    groupings <- dplyr::vars(site_id, date)
    time_unit <- 24
  } else if (by_day == TRUE & by_hour == TRUE) {
    print("Grouping by date and hour (1 hour averages, by day)")
    dataset_stamped <- dataset %>% 
      column_dt(c("date_hour", "hour_minute"))
    groupings_drop <- dplyr::vars(site_id, date_hour, hour_minute)
    groupings <- dplyr::vars(site_id, date_hour)
    time_unit <- 60/2
  } else if (by_day == FALSE & by_hour == TRUE) {
    print("Grouping by hour (1 hour averages)")
    dataset_stamped <- dataset %>% 
      column_dt(c("hour", "hour_minute"))
    groupings_drop <- dplyr::vars(site_id, hour, hour_minute)
    groupings <- dplyr::vars(site_id, hour)
    time_unit <- 60/2
  }
  
  # Calculating the minimum number of data points required to be included in the set
  count_to_drop <- ceiling(time_unit*(epa_percent/100))
  
  # Creating a data frame of groups to be removed due to low data quantity
  drop_quantity <- dataset_stamped %>% 
    group_by_at(groupings_drop) %>% 
    summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
    group_by_at(groupings) %>% 
    count() %>% 
    filter(n < count_to_drop) %>% 
    arrange(n)
  
  print("Values to be dropped via an anti-join due to low data quantity:")
  print(drop_quantity)
  
  drop_ab <- dataset %>% 
    select(!intersect(c("temperature", "humidity", "pm25_cf1", "pm25_atm"), colnames(.))) %>% 
    column_dt("date") %>% 
    group_by(site_id, date) %>% 
    summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
    mutate(
      # Difference between A & B sensors
      diff = pm25_cf1_A-pm25_cf1_B,
      # Percentage difference between A & B sensors
      per_diff = 100*(abs(pm25_cf1_A-pm25_cf1_B))/((pm25_cf1_A+pm25_cf1_B)/2),
      drop = case_when(
        # Will drop the following based on EPA recommendations
        abs(diff) >= 5 & abs(per_diff) >= 62 ~ TRUE,
        # Fills all other values (the ones to be kept) with 'FALSE'
        TRUE ~ FALSE
      )
    ) %>% 
    filter(drop == TRUE) %>% 
    select(site_id, date)
  
  print("Days (by sensor) to be dropped via an anti-join due to A & B sensor disagreement:")
  print(drop_ab)
  
  dataset_stamped <- dataset_stamped %>% 
    anti_join(drop_quantity) %>% 
    anti_join(drop_ab) %>% 
    group_by_at(groupings) %>% 
    summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
    rowwise() %>% 
    mutate(
      # US-wide correction factor published in 2020
      pm25_epa_2020 = 0.524*pm25_cf1 - 0.0852*humidity + 5.72,
      # US-wide correction factor published in late 2020
      pm25_epa_2021 = case_when(
        pm25_cf1 <= 343 ~ 0.52*pm25_cf1 - 0.086*humidity + 5.75,
        pm25_cf1 > 343 ~ 0.46*pm25_cf1 + (3.93*10^(-4))*(pm25_cf1^2) +2.97
      ),
      # US-wide correction factor published in late 2020 using CF=ATM values
      pm25_epa_atm = case_when(
        pm25_atm < 50 ~ 0.25*pm25_atm - 0.086*humidity + 5.75,
        pm25_atm >= 50 & pm25_atm < 229 ~ 0.786*pm25_atm - 0.086*humidity + 5.75,
        pm25_atm > 229 ~ 0.69*pm25_atm + (8.84*10^-4)*(pm25_atm^2) + 2.97
      )
    ) %>% 
    # Setting negative values to NA
    mutate_at(vars(pm25_epa_2020, pm25_epa_2021, pm25_epa_atm), ~replace(., which(.<0), NA))
  
  if (keep_cols == FALSE) {
    print("Dropping extraneous columns [default]")
    dataset_stamped <- dataset_stamped %>% 
      select(!intersect(c("temperature", "humidity",
                          "temeprature_c", "temperature_ambient", "temperature_ambient_c",
                          "pm25_cf1_A", "pm25_atm_A", "pm25_cf1_B", "pm25_atm_B",
                          "pm25_cf1", "pm25_atm"), colnames(.)))
  } else { print("Keeping all columns") }
  
  return(dataset_stamped)
}