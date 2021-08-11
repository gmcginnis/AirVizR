#' Heatmap for multiple monitors' temporal atmospheric data
#'
#' Visualize temporal atmospheric data for multiple monitors using a heatmap, with optional data labels. To visualize one monitor on a heatmap, see \link{heatmap_single}.
#' Relevant information (such as date ranges, averaging methods, facets, and min/max values in the set) will be reported automatically in the visualization.
#' @param dataset The hourly dataset to visualize
#' @param variable_of_interest The variable of interest (not in quotation marks) which to visualize
#' @param drop_incomplete Logical; drop/keep incomplete monitors, see \link{drop_incomplete} for more information.
#' @param cap_value Numeric, optional; values at or above to be colored serpately from the regular continuous scale. See \link{add_cap} for more information.
#' @param cap_color Character; color for values at or above the \code{cap_value}
#' @param data_labels Logical; label each cell in the heatmap with the appropriate value
#' @param text_color Character; the color of data label text
#' @param digits Numeric; the number of digits to report
#' @param location_data Data set containing latitude and longitude data
#' @return Data visualization: heatmap colored by a specified numeric variable, with time on the x-axis (and appropriate breaks & labels) and monitor labels on the y-axis arranged from north to south and separted by location.
#' @examples
#' heatmap_cross(july_api_daily, humidity)
#' heatmap_cross(july_api_hourly, pm25_atm, location_data = july_api_meta, cap_value = 75, cap_color = "green")
#' @export
heatmap_cross <- function(dataset, variable_of_interest, drop_incomplete = FALSE,
                          cap_value = NA, cap_color = "red",
                          data_labels = FALSE, text_color = "black",
                          location_data = data_meta, digits = 2){
  
  # Dropping NA values from the variable of interest
  # NA values will appear as gaps if viewing the complete viz, rather than gray
  # Adding another column with duplicate info, in case a color cap applied AND text labeling is desired
  dataset <- dataset %>% 
    drop_na({{variable_of_interest}}) %>% 
    mutate(val_orig = {{variable_of_interest}})
  
  variable_of_interest_qt <- deparse(substitute(variable_of_interest))
  
  if (drop_incomplete == TRUE) {
    dataset <- drop_incomplete(dataset, var_qt = variable_of_interest_qt)
  } else { print("All monitors will be plotted.") }
  
  # Adding quotation marks
  cap_color <- deparse(substitute(cap_color)) %>%
    str_replace_all("\\\"", "") # Removing extra quotation marks if already provided
  
  # Location data
  temp_loc <- location_data %>% 
    # Arranging such that northern-most monitors will be on top
    mutate(label = fct_reorder(as.factor(label), desc(latitude))) %>% 
    # Selecting only variables of interest to save space
    select(site_id, label, location)
  
  unit_results <- settings_units(dataset = dataset, var = variable_of_interest_qt, cap_color = cap_color,
                                 lab_title = "Heatmap of")
  
  lab_title <- unit_results$lab_title
  lab_title_val <- unit_results$lab_title_val
  lab_subtitle <- unit_results$lab_subtitle
  lab_fill <- unit_results$lab_fill
  fill_colors <- unit_results$fill_colors
  
  cap_results <- add_cap(dataset = dataset, var_qt = variable_of_interest_qt,
                         cap_value = cap_value, cap_color = cap_color)
  
  dataset <- cap_results$dataset
  lab_subtitle <- paste(lab_subtitle, cap_results$lab_subtitle_cap, sep="\n")
  cap_guide <- cap_results$cap_guide
  
  scale_results <- settings_dt_scale(dataset = dataset)
  
  dataset <- scale_results$dataset
  lab_title_sub <- scale_results$lab_title_sub
  x_angle <- scale_results$x_angle
  x_scale <- scale_results$x_scale
  lab_caption <- scale_results$lab_caption
  
  lab_caption <- paste0(lab_caption, "\nMonitors are arranged north to south in their respective groups.")
  
  if (data_labels == TRUE) {
    data_labels <- geom_text(color = {{text_color}})
  } else { data_labels <- NULL }
  
  dataset %>% 
    distinct() %>% 
    left_join(temp_loc) %>% 
    ggplot(
      aes(
        x = timestamp,
        y = label,
        color = "",
        fill = {{variable_of_interest}},
        label = rounding_w_zeroes(val_orig, digits)
      )
    ) +
    facet_grid(
      location~.,
      scales = "free_y",
      space = "free_y"
    ) +
    geom_tile() +
    fill_colors + 
    data_labels +
    scale_y_discrete(limits = rev) + 
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_text(angle = x_angle, hjust = 1),
      axis.title = element_blank(),
      axis.ticks.x = element_line()
    ) + 
    labs(
      title = paste(lab_title, lab_title_val, lab_title_sub),
      subtitle = lab_subtitle,
      fill = lab_fill,
      caption = lab_caption
    ) +
    x_scale + 
    scale_color_manual(values = "transparent") +
    cap_guide
}