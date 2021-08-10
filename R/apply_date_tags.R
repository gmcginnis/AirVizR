#' Create a column of "Date Tags"
#'
#' Create a column of date tags based on specified date ranges (\code{starts} and \code{ends}) and respective names (\code{tags}).
#' @param dataset The dataset for which to apply the date tags to.
#' @param starts Character list of start dates (format for each date: "YYYY-MM-DD")
#' @param ends Character list of end dates (format for each date: "YYYY-MM-DD")
#' @param tags Character list of tags
#' @param stamp Desired date stamp format type. To avoid ambiguity, specify a day example with a value greater than 12, and a 1990s year.
#' @return Dataframe with a new column ("date_tag") containing the appropriate date tag with respect to each row's timestamp.
#' @examples 
#' tagged_data <- apply_date_tags(data_daily, starts = c("2021-07-01" "2021-07-04" "2021-07-05"), ends = c("2021-07-03" "2021-07-04" "2021-07-07"), tags = c("Before", "Independence Day", "After"))
#' @export
apply_date_tags <- function(dataset,
                            starts = input_date_starts, ends = input_date_ends, tags = input_date_tags,
                            stamp = "17 Jan 1999"){
  
  starts <- as.Date(starts)
  ends <- as.Date(ends)
  tags <- factor(tags)
  date_format <- stamp_date(stamp)
  
  data_temp <- data.frame(starts, ends, tags) %>% 
    arrange(starts) %>% 
    mutate(
      start_stamp = date_format(starts),
      end_stamp = date_format(ends),
      date_tag = fct_inorder(paste0(tags, "\n(", start_stamp, " - ", end_stamp, ")"))
    ) %>% 
    pivot_longer(cols = c(starts, ends), values_to = "date") %>% 
    group_by(date_tag) %>% 
    complete(date = full_seq(date, 1)) %>% 
    select(!c(name, tags, start_stamp, end_stamp))
  
  remove_date <- FALSE
  
  if ("date" %in% colnames(dataset) == FALSE) {
    print("`date` column not detected; temporarily adding to dataframe")
    dataset <- column_dt(dataset, "date")
    remove_date <- TRUE
  }
  
  dataset <- dataset %>% 
    left_join(data_temp) %>% 
    drop_na(date_tag)
  
  if (remove_date == TRUE) {
    dataset <- select(dataset, !date)
    print("Temporary `date` column removed")
  }
  
  return(dataset)
}