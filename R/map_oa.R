#' View moving data on an interactive map
#' 
#' Visualize data that involves lat/long movement.
#' @family {STAD visualizations}
#' @seealso \code{\link{map_stad()}}
#' @param dataset Data set to visualize. Requires \code{latitude} and \code{longitude} columns.
#' @param variable_of_interest blah
#' @param palette_color Character; any valid input for the \code{palette} argument for \code{\link{colorNumeric()}}.
#' @param provider_tiles Character or provider source; see \code{provider} argument for \code{\link{addProviderTiles()}} for a link to lists.
#' @return Interactive leaflet map.
#' @examples 
#' map_oa(oa_moving_full, black_carbon)
#' @export
map_oa <- function(dataset, variable_of_interest,
                   palette_color = "viridis", provider_tiles = "CartoDB.DarkMatter"){
  
  var_qt <- deparse(substitute(variable_of_interest))
  # Getting unit label
  var_unit <- (settings_units(dataset, var_qt))$lab_fill
  
  names(dataset)[names(dataset) == var_qt] <- 'var_of_interest'
  
  dataset <- (settings_dt_scale(dataset))$dataset
  
  # Creating a color palette
  var_pal <- leaflet::colorNumeric(palette = palette_color, domain = dataset$var_of_interest)
  
  # Creating argument for the pop up that will appear when one clicks a point
  # can use HTML arguments for text settings
  bc_popup <- paste0("<b>Timestamp:</b> ", dataset$timestamp,
                     "<br><b>", var_unit, ":</b> ", dataset$var_of_interest)
  
  # can set the data frame in the opening argument, or in individual layers
  leaflet::leaflet(data = dataset) %>% 
    # Adding tiles (making the map)-- can use addTiles() or provider tiles as below
    # Preview provider options at https://leaflet-extras.github.io/leaflet-providers/preview/
    leaflet::addProviderTiles(provider_tiles) %>% 
    # Adding points on the map
    leaflet::addCircles(
      # use a tilde when referring to variables in the data set
      lng = ~longitude,
      lat = ~latitude,
      # for color, the palette made above acts as a function for the specified variable
      color = ~var_pal(var_of_interest),
      # What will display when the point is hovered over:
      label = ~as.character(var_of_interest),
      # What will display when the point is clicked:
      popup = bc_popup
    ) %>% 
    leaflet::addLegend(
      pal = var_pal,
      values = ~var_of_interest,
      title = var_unit
    )
}
