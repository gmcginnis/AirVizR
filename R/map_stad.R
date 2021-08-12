#' Map Spatio-Temporal Atmospheric Data (STAD)
#'
#' Visualize spatio-temporal atmospheric data using points on a map, colored by a specified variable.
#' Relevant information (such as date ranges, averaging methods, facets, and min/max values in the set) will be reported automatically in the visualization.
#' @family {STAD visualizations}
#' @param dataset Data set for which to visualize
#' @param variable_of_interest The variable of interest (not in quotation marks) which to visualize
#' @param grouping_vars Character, optional; one or two variables for which to facet (grid) the plot by
#' @param location_data Data set containing latitude and longitude data
#' @param cap_value Numeric, optional; values at or above to be colored serpately from the regular continuous scale. See \link{add_cap} for more information.
#' @param cap_color Character; color for values at or above the \code{cap_value}
#' @param point_size Numeric; size of the points on the map
#' @param maptype Character; the \link[ggmap]{qmplot} class of maptype to use; see \link[ggmap]{get_map} for types.
#' @param zoom Numeric; the zoom level of the map
#' @param tint_alpha Numeric; the transparency for the background tint overlaid on the map
#' @param tint_color Character; the color of background tint overlaid on the map
#' @return Data visualization: map with data points colored by a specified numeric variable, located by a provided data frame of lat/long data.
#' @examples 
#' \donttest{
#' map_stad(july_api_daily, pm25_atm, location_data = july_api_meta, grouping_vars = "date_tag")
#' map_stad(
#'   july_api_daily, pm25_atm, location_data = july_api_meta, grouping_vars = "date_tag",
#'   point_size = 5, cap_value = 50, cap_color = "green",
#'   maptype = "terrain", tint_color = "white"
#' )
#' }
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
map_stad <- function(dataset, variable_of_interest, grouping_vars = NULL, location_data = data_meta,
                     cap_value = NA, cap_color = "red", point_size = 3,
                     maptype = "toner-lite", zoom = 11, tint_alpha = 0.5, tint_color = "black"){
  
  # Warning message and halting execution if required input is missing.
  if (missing(variable_of_interest) == TRUE) {
    stop ("ERROR in argument 'variable_of_interest': Missing input, with no default. Execution haulted. Please input a valid PM2.5 variable (e.g. pm25_atm).")
  }
  
  # Warning message and halting execution if inputeded data does not have more than one unique site_id (map will bug)
  if (length(unique(dataset$site_id)) == 1) {
    stop("ERROR: Insufficient data to create a map. Provide a data set with more than one site.")
  }
  
  variable_of_interest_qt <- deparse(substitute(variable_of_interest))
  
  scale_results <- settings_dt_scale(dataset = dataset)
  
  lab_title_sub <- scale_results$lab_title_sub
  lab_subtitle <- scale_results$lab_caption
  
  # Wrangling based on provided inputs
  dataset <- dataset %>% 
    dplyr::ungroup() %>% 
    # Grouping by provided variables, as well as site_id
    dplyr::group_by_at(dplyr::vars(site_id, {{grouping_vars}})) %>% 
    # Calculating means by said grouping variables
    dplyr::summarize(mean = mean({{variable_of_interest}}, na.rm = TRUE)) %>% 
    tidyr::drop_na(mean) %>% 
    # Adding location data to get lat & lon
    dplyr::left_join(location_data)
  
  # Renaming newly-created "mean" variable
  names(dataset)[names(dataset) == 'mean'] <- variable_of_interest_qt
  
  # Feedback message
  print("Data now grouped and averaged. Location data added.")
  
  shape_results <- settings_shapes(dataset = dataset, meta = location_data)
  
  shape_set <- shape_results$shape_set
  shape_guide <- shape_results$shape_guide
  
  unit_results <- settings_units(dataset = dataset, var_qt = variable_of_interest_qt,
                                 cap_color = cap_color, lab_title = "Map of")
  
  lab_title <- unit_results$lab_title
  lab_title_val <- unit_results$lab_title_val
  lab_subtitle <- paste(lab_subtitle, unit_results$lab_subtitle, sep = "\n")
  lab_fill <- unit_results$lab_fill
  fill_colors <- unit_results$fill_colors
  
  cap_results <- add_cap(dataset = dataset, var_qt = variable_of_interest_qt,
                         cap_value = cap_value, cap_color = cap_color)
  
  dataset <- cap_results$dataset
  lab_subtitle <- paste(lab_subtitle, cap_results$lab_subtitle_cap, sep="\n")
  cap_guide <- cap_results$cap_guide
  
  if ("hour" %in% grouping_vars) {
    dataset <- dataset %>% 
      dplyr::mutate(hour = paste0(formatC(lubridate::hour(hour), width = 2, flag = 0), ":00"))
    print("Hour column changed to class 'character' to allow for faceting.")
  }
  
  # Base plot
  plot <- ggmap::qmplot(data = dataset, x = longitude, y = latitude,
                 geom = "blank", maptype = maptype, zoom = zoom, darken = c(tint_alpha, tint_color)) +
    geom_point(
      aes(fill = {{variable_of_interest}}, shape = location, color = ""),
      # color = "white",
      alpha = 0.8,
      size = point_size
    ) +
    fill_colors +
    theme_void() +
    theme(legend.position = "bottom") + 
    shape_set +
    shape_guide +
    labs(
      title = paste(lab_title, lab_title_val),
      subtitle = lab_subtitle,
      fill = lab_fill,
      shape = "Monitor location:",
      caption = paste0("Data ", lab_title_sub, "; displayed as collective average by location")
    ) +
    cap_guide +
    scale_color_manual(values = "transparent")
  
  # Feedback message
  print("Base plot created.")
  
  if (length(grouping_vars) == 2) {
    plot <- plot +
      facet_grid(
        formula(paste(
          grouping_vars[1],
          "~",
          grouping_vars[2]
        ))) +
      labs(
        caption = paste(
          "Data",
          lab_title_sub,
          "then grouped by",
          grouping_vars[1],
          "and",
          grouping_vars[2]
        )
      )
    # Feedback message
    print(paste("Plot now faceted by",
                grouping_vars[1],
                "and",
                grouping_vars[2]
    ))
  } else if (length(grouping_vars) == 1) {
    plot <- plot +
      facet_wrap(grouping_vars) +
      labs(
        caption = paste(
          "Data",
          lab_title_sub,
          "then grouped by",
          grouping_vars[1]
        )
      )
    # Feedback message
    print(paste("Plot now faceted by", grouping_vars))
  } else if (length(grouping_vars) > 1) { print("Error: more than two grouping variable provided. Please select up to two.") }
  
  print("Final plot created.")
  
  # Returning the final plot
  plot
}
