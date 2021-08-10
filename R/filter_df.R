#' Filter a Data Frame
#'
#' Filter a data frame based on any character variable, including varaibles from meta data, without having to conduct joins or string arguments yourself.
#' @param dataset The dataset which to filter
#' @param var The varaible which to filter (not in quotation marks)
#' @param include Character list; the list of values to keep for the data frame (optional if \code{exclude} specified)
#' @param exclude Character list; the list of values to remove from the data frame (optional if \code{include} specified)
#' @param location_data The data set containing monitor meta data
#' @return Data frame filtered by the specifiecations above
#' @examples 
#' daytime_data <- filter_df(data_pm25, hour_tag, include = c("Morning", "Afternoon"))
#' no_lighthouse <- filter_df(data_pm25, label, exclude = c("Lighthouse"))
#' @export
filter_df <- function(dataframe, var = label, include = c(""), exclude = c(""), location_data = data_meta) {
  
  to_include <- paste0("(", paste(include, collapse = ")|("), ")")
  to_exclude <- paste0("(", paste(exclude, collapse = ")|("), ")")
  
  df <- dataframe %>% left_join(data_meta)
  
  df <- df %>% 
    filter(str_detect({{var}}, to_include))
  
  if (to_exclude != "()"){
    df <- df %>% 
      filter(!str_detect({{var}}, to_exclude))
  }
  
  df %>% 
    select(all_of(names(dataframe)))
}