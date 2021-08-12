#' Create a column of "Date Tags"
#'
#' Create a column of date tags based on specified hour ranges (created using inputted \code{starts}) and respective names (\code{tags}).
#' @param dataset The dataset for which to apply the date tags to.
#' @param starts Numeric list of start hours (in 24 hour format)
#' @param tags Character list of tags
#' @return Dataframe with a new column ("hour_tag") containing the appropriate hour tag with respect to each row's timestamp.
#' @examples 
#' apply_hour_tags(
#'   head(july_api_diurnal[1:3]),
#'   starts = c(5, 12, 17, 21),
#'   tags = c("Morning", "Afternoon", "Evening", "Night")
#' )
#' @importFrom magrittr %>%
#' @export
apply_hour_tags <- function(dataset, starts = input_hour_starts, tags = input_hour_tags){
  
  require(dplyr)
  
  if (length(intersect(c("hour", "date_hour", "datetime"), colnames(dataset))) < 1) {
    stop("Data set does not contain hours. Execution halted.")
  }
  
  if ("hour" %in% colnames(dataset) == FALSE) {
    print("`hour` column not detected; temporarily adding to dataframe")
    dataset <- column_dt(dataset, "hour")
    remove_hour <- TRUE
  } else { remove_hour <- FALSE }
  
  tags <- factor(tags)
  
  hours_in_day <- data.frame(starts = 0:23)
  
  data_temp <- data.frame(starts, tags) %>% 
    dplyr::mutate(
      ends = dplyr::lead(starts - 1),
      ends = tidyr::replace_na(ends, starts[1]-1),
      tags = paste0(tags, "\n(", formatC(starts, width=2, flag=0), ":00-", formatC(ends, width=2, flag=0), ":59)")
    ) %>% 
    dplyr::arrange(starts) %>% 
    dplyr::mutate(hour_tag = forcats::fct_inorder(tags)) %>%
    tidyr::complete(starts = tidyr::full_seq(starts, 1)) %>%
    dplyr::right_join(hours_in_day) %>%
    dplyr::rename(hour = starts) %>%
    tidyr::fill(hour_tag) %>%
    dplyr::select(!c(tags, ends)) %>%
    dplyr::mutate(hour = hms::as_hms(hour*60*60))
  
  dataset <- dplyr::left_join(dataset, data_temp)
  
  if (remove_hour == TRUE) {
    dataset <- dplyr::select(dataset, !hour)
    print("Temporary `hour` column removed")
  }
  
  return(dataset)
}