#' Wrangle Location (Meta) Data
#'
#' Select variables of interest from location data, and (importantly!) make monitor labels unique from one another.
#' @param raw_location_data The data set which to wrangle.
#' @return Dataframe with selected columns of interest.
#' @examples 
#' wrangle_meta(july_api_raw_meta)
#' @importFrom magrittr %>%
#' @export
wrangle_meta <- function(raw_location_data = raw_meta) {
  location_data <- raw_location_data %>% 
    dplyr::select(site_id, location, label, longitude, latitude, timezone, flag_highValue) %>% 
    dplyr::mutate(
      label_orig = label,
      label = make.unique(label, sep = " ")
    )
  print("Meta data now sorted and labels made distinctive.")
  return(location_data)
}