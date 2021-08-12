#' Create a column of "Date Tags"
#'
#' Create a column of date tags based on specified date ranges (\code{starts} and \code{ends}) and respective names (\code{tags}).
#' @family {miscellaneous functions}
#' @param dataset The dataset for which to apply the date tags to.
#' @param starts Character list of start dates (format for each date: "YYYY-MM-DD")
#' @param ends Character list of end dates (format for each date: "YYYY-MM-DD")
#' @param tags Character list of tags
#' @param stamp Desired date stamp format type. To avoid ambiguity, specify a day example with a value greater than 12, and a 1990s year.
#' @return Dataframe with a new column ("date_tag") containing the appropriate date tag with respect to each row's time stamp.
#' @examples 
#'   apply_date_tags(
#'     dplyr::filter(july_api_daily, site_id == "0441c6b9f431d0e4_23361")[1:3],
#'     starts = c("2020-07-01", "2020-07-04", "2020-07-05"),
#'     ends = c("2020-07-03", "2020-07-04", "2020-07-07"),
#'     tags = c("Before", "Independence Day", "After")
#'   )
#' @importFrom magrittr %>%
#' @export
apply_date_tags <- function(dataset,
                            starts = input_date_starts, ends = input_date_ends, tags = input_date_tags,
                            stamp = "17 Jan 1999"){
  
  starts <- as.Date(starts)
  ends <- as.Date(ends)
  tags <- factor(tags)
  custom_stamp <- lubridate::stamp_date(stamp)
  
  data_temp <- data.frame(starts, ends, tags) %>% 
    dplyr::arrange(starts) %>% 
    dplyr::mutate(
      start_stamp = custom_stamp(starts),
      end_stamp = custom_stamp(ends),
      date_tag = forcats::fct_inorder(paste0(tags, "\n(", start_stamp, " - ", end_stamp, ")"))
    ) %>% 
    tidyr::pivot_longer(cols = c(starts, ends), values_to = "date") %>% 
    dplyr::group_by(date_tag) %>% 
    tidyr::complete(date = tidyr::full_seq(date, 1)) %>% 
    dplyr::select(!c(name, tags, start_stamp, end_stamp))
  
  remove_date <- FALSE
  
  if ("date" %in% colnames(dataset) == FALSE) {
    print("`date` column not detected; temporarily adding to dataframe")
    dataset <- column_dt(dataset, "date")
    remove_date <- TRUE
  }
  
  dataset <- dataset %>% 
    dplyr::left_join(data_temp) %>% 
    tidyr::drop_na(date_tag)
  
  if (remove_date == TRUE) {
    dataset <- dplyr::select(dataset, !date)
    print("Temporary `date` column removed")
  }
  
  return(dataset)
}