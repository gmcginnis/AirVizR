#' Visualization Settings for Monitor Locations
#'
#' Arguments to detect the relevant shapes to be applied to visualizations based on monitor location(s) in the data set.
#' Custom shapes are necessary for the custom visualizations, since "fill" and "color" arguments can be individually specified for the shapes below, unlike the default shapes.
#' @param dataset The dataset which to evaluate
#' @param meta The location data, related the data set
#' @return List of values to be applied to various visualization methods:
#' \describe{
#'   \item{shape_set}{Scale settings for shapes; see \link[ggplot2]{scale_shape_manual}.}
#'   \item{shape_guide}{Guide settings for shapes; see \link[ggplot2]{guides}.}
#' }
#' @examples 
#' shape_results_example <- settings_shapes(july_api_daily, july_api_meta)
#' ggplot(full_join(july_api_daily, july_api_meta), aes(date, site_id, shape = location, color = location, fill = location)) +
#'   geom_point(size = 3) +
#'   shape_results_example$shape_set +
#'   shape_results_example$shape_guide
#' remove(shape_results_example)
#' @export
settings_shapes <- function(dataset = dataset, meta = data_meta) {
  
  if ("location" %in% colnames(dataset) == FALSE) {
    dataset <- left_join(dataset, meta)
  }
  
  # Creating a list of unique monitor locations
  unique_locs <- unique(dataset$location)
  shape_nrow <- length(unique_locs)
  
  shapes_df <- data.frame(
    "location" = c("outside", "inside", "FRM", "average"),
    "shape" = c(21, 23, 25, 24)
  ) %>% 
    filter(location %in% unique_locs)
  
  shape_set <- scale_shape_manual(values = setNames(shapes_df$shape, as.character(shapes_df$location)))
  shape_guide <- guides(shape = guide_legend(override.aes = list(fill = "black", color = "black"), nrow = shape_nrow))
  
  return(list(
    shape_set = shape_set,
    shape_guide = shape_guide
  ))
}