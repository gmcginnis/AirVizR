#' Visualization Settings for Monitor Locations
#'
#' Arguments to detect the relevant shapes to be applied to visualizations based on monitor location(s) in the data set.
#' Custom shapes are necessary for the custom visualizations, since "fill" and "color" arguments can be individually specified for the shapes below, unlike the default shapes.
#' @param dataset The dataset which to evaluate
#' @param meta The location data, related the data set
#' @return List of values to be applyed to various visualization methods.
#' @examples 
#' shape_results <- settings_shapes(data_pm25, data_meta)
#' ggplot() + shape_set + shape_guide
#' @export
settings_shapes <- function(dataset = dataset, meta = data_meta) {
  
  if ("location" %in% colnames(dataset) == FALSE) {
    dataset <- left_join(dataset, meta)
  }
  
  # Creating a list of unique monitor locations
  unique_locs <- unique(dataset$location)
  shape_nrow <- length(unique_locs)
  
  shapes_df <- data.frame(
    "location" = c("outside", "inside", "DEQ", "average"),
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