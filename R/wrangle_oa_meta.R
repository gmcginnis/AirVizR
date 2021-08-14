#' Create meta data set for OA data
#' 
#' For OA data that does not involve movement, create a meta data frame with this.
#' @family {OA functions}
#' @seealso \code{\link{wrangle_meta()}}
#' @param dataset Blah
#' @param timezone Character
#' @param single Logical
#' @return Data set.
#' @examples 
#' 2+2
#' @importFrom magrittr %>%
#' @export
wrangle_oa_meta <- function(dataset, timezone, single = TRUE){
  
  dataset <- dplyr::select(dataset, site_id, latitude, longitude)
  
  if(length(unique(dataset$site_id)) == 1) {
    single <- TRUE
    print("Single site_id detected")
  }
  
  if(isTRUE(single)){
    dataset <- dplyr::ungroup(dataset)
  } else {
    dataset <- dplyr::group_by(dataset, site_id)
  }
  
  metaset <- dataset %>% 
    dplyr::mutate(
      latitude = median(latitude, na.rm = TRUE),
      longitude = median(longitude, na.rm = TRUE)
    ) %>% 
    dplyr::distinct() %>% 
    mutate(
      location = factor("OA"),
      timezone = timezone,
      flag_highValue = NA
    )
  
  if(isTRUE(single)){
    metaset <- metaset %>% 
      mutate(
        label = "ObservAir",
        label_orig = paste(
          "ObservAir",
          site_id %>%
            stringr::str_replace(".txt", "") %>% 
            stringr::str_replace("Data34_", "")
        )
      ) %>% 
      dplyr::select(site_id, location, label, longitude, latitude, timezone, flag_highValue, label_orig)
  } else {
    metaset <- metaset %>% 
      dplyr::mutate(
        label = paste(
          "ObservAir",
          site_id %>%
            stringr::str_replace(".txt", "") %>% 
            stringr::str_replace("Data34_", "")
        ),
        label_orig = label
      ) %>% 
      dplyr::select(site_id, location, label, longitude, latitude, timezone, flag_highValue, label_orig)
  }
  return(metaset)
}
