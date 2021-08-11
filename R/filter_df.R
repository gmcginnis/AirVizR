#' Filter a Data Frame
#'
#' Filter a data frame based on any character variable, including varaibles from meta data, without having to conduct joins or string arguments yourself.
#' @param dataset The dataset which to filter
#' @param var The varaible which to filter (not in quotation marks)
#' @param include Character list; the list of values to keep for the data frame (optional if \code{exclude} specified)
#' @param exclude Character list; the list of values to remove from the data frame (optional if \code{include} specified)
#' @param location_data Optional, unless \code{var} not in \code{dataset}; the data set containing monitor meta data
#' @return Data frame filtered by the specifications above
#' @examples 
#' filter_df(july_api_diurnal, hour_tag, include = c("Morning", "Afternoon"))
#' filter_df(july_api_diurnal, label, exclude = c("Lighthouse"), location_data = july_api_meta)
#' @export
filter_df <- function(dataframe, var = label, include = c(""), exclude = c(""), location_data = data_meta) {
  
  to_include <- paste0("(", paste(include, collapse = ")|("), ")")
  to_exclude <- paste0("(", paste(exclude, collapse = ")|("), ")")
  
  if (exists(deparse(substitute(location_data))) == TRUE) {
    print("Joining meta data")
    if (deparse(substitute(var)) %in% colnames(location_data)){
      dataframe <- dataframe %>% left_join(location_data)
    }
  } else { print("Meta data not needed for filtering") }
  
  dataframe <- dataframe %>% 
    filter(str_detect({{var}}, to_include))
  
  if (to_exclude != "()"){
    dataframe <- dataframe %>% 
      filter(!str_detect({{var}}, to_exclude))
  }
  
  dataframe %>% 
    select(all_of(names(dataframe)))
}
