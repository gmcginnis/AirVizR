#' Create a column of "Date Tags"
#'
#' Create a column of date tags based on specified hour ranges (created using inputted \code{starts}) and respective names (\code{tags}).
#' @param dataset The dataset for which to apply the date tags to.
#' @param starts Numeric list of start hours (in 24 hour format)
#' @param tags Character list of tags
#' @return Dataframe with a new column ("hour_tag") containing the appropriate hour tag with respect to each row's timestamp.
#' @examples 
#' tagged_data <- apply_hour_tags(july_api_diurnal, starts = c(5, 12, 17, 21), tags = c("Morning", "Afternoon", "Evening", "Night"))
#' @export
apply_hour_tags <- function(dataset, starts = input_hour_starts, tags = input_hour_tags){
  
  if (length(intersect(c("hour", "date_hour", "datetime"), colnames(dataset))) < 1) {
    stop("Data set does not contain hours. Execution halted.")
  }
  
  if ("hour" %in% colnames(dataset) == FALSE) {
    print("`hour` column not detected; temporarily adding to dataframe")
    dataset <- dataset %>% column_dt("hour")
    remove_hour <- TRUE
  } else { remove_hour <- FALSE }
  
  tags <- factor(tags)
  
  hours_in_day <- data.frame(starts = 0:23)
  
  data_temp <- data.frame(starts, tags) %>% 
    mutate(
      ends = lead(starts - 1),
      ends = replace_na(ends, starts[1]-1),
      tags = paste0(tags, "\n(", formatC(starts, width=2, flag=0), ":00-", formatC(ends, width=2, flag=0), ":59)")
    ) %>% 
    arrange(starts) %>% 
    mutate(hour_tag = fct_inorder(tags)) %>%
    complete(starts = full_seq(starts, 1)) %>%
    right_join(hours_in_day) %>%
    rename(hour = starts) %>%
    fill(hour_tag) %>%
    select(!c(tags, ends)) %>%
    mutate(hour = hms::as_hms(hour*60*60))
  
  dataset <- dataset %>% 
    left_join(data_temp)
  
  if (remove_hour == TRUE) {
    dataset <- select(dataset, !hour)
    print("Temporary `hour` column removed")
  }
  
  return(dataset)
}