#' Heatmap for a single monitor's temporal atmospheric data
#'
#' Visualize hourly atmospheric data for a single monitor using a heatmap, with optional data labels. To visualize multiple monitors on a heatmap, see \link{heatmap_cross}.
#' Relevant information (such as date ranges, averaging methods, facets, and min/max values in the set) will be reported automatically in the visualization.
#' @param variable_of_interest The variable of interest (not in quotation marks) which to visualize
#' @param site_of_interest Character; the label (or a portion of the label) of the monitor to visualize
#' @param cap_value Numeric, optional; values at or above to be colored serpately from the regular continuous scale. See \link{add_cap} for more information.
#' @param cap_color Character; color for values at or above the \code{cap_value}
#' @param data_labels Logical; label each cell in the heatmap with the appropriate value
#' @param digits Numeric; the number of digits to report
#' @param date_breaks Character; the frequency of x-axis label breaks
#' @param text_color Character; the color of data label text
#' @param dataset The hourly data set to visualize
#' @param location_data Data set containing latitude and longitude data
#' @return Data visualization: hourly heatmap colored by a specified numeric variable, with date on the x-axis and hours on the y-axis.
#' @examples 
#' heatmap_single(pm25_epa_2021, "Lighthouse", dataset = july_api_hourly, location_data = july_api_meta)
#' heatmap_single(temperature, "Lighthouse", cap_value = 85, cap_color = "green", data_label = FALSE, dataset = july_api_hourly, location_data = july_api_meta)
#' @export
heatmap_single <- function(variable_of_interest, site_of_interest = "",
                           cap_value = NA, cap_color = "red",
                           data_labels = TRUE, digits = 2, date_breaks = "1 day",
                           text_color = "black", dataset = data_hourly, location_data = data_meta){
  
  if ("date_hour" %in% colnames(dataset) == FALSE) { stop("ERROR: Inputted data set is not hourly by day") }
  
  variable_of_interest_qt <- deparse(substitute(variable_of_interest))
  
  if (variable_of_interest_qt %in% colnames(dataset) == FALSE) {
    dataset %>%
      slice(1) %>%
      ungroup() %>%
      select_if(is.numeric) %>%
      colnames() %>%
      print()
    stop("ERROR: Inputted variable of interest is not in the provided data set. Execution halted. Valid inputs are listed above")
  }
  
  temp_loc <- location_data %>% 
    select(site_id, label, location)
  
  dataset <- dataset %>% 
    ungroup() %>% 
    # Adding labels to the data set
    left_join(temp_loc) %>% 
    # Filtering data set based on inputted site of interest
    filter(str_detect(label, site_of_interest)) %>% 
    # Removing empty values from variable of interest (otherwise is grey when mapping)
    drop_na({{variable_of_interest}}) %>% 
    # Removing duplicate rows
    distinct() %>% 
    mutate(val_orig = {{variable_of_interest}})
  
  # dataset_vals <- dataset %>%
  #   select("date_hour", "vals" = variable_of_interest_qt)
  
  # Verification that only one monitor is selected. Execution will halt if 0 or >1 are detected
  if (length(unique(dataset$label)) == 0) {
    stop("ERROR: No monitors selected. Please verify that a distinct label was provided. Capitalization matters.")
  }
  if (length(unique(dataset$label)) > 1) {
    print("Matching locations from meta data:")
    temp_loc %>% filter(str_detect(label, site_of_interest)) %>% distinct() %>% pull(label) %>% print()
    print("Matching locations that contain values of interest:")
    dataset %>% pull(label) %>% unique() %>% print()
    stop("ERROR: More than one site selected. Please use a more precise string argument; matching values are listed above.")
  }
  
  unit_results <- settings_units(dataset = dataset, var = variable_of_interest_qt,
                                 cap_color = cap_color, lab_title = "Heatmap of")
  
  lab_title <- unit_results$lab_title
  lab_title_val <- unit_results$lab_title_val
  lab_subtitle <- paste0(
    "Monitor selected: \"", dataset %>% pull(label),
    "\" (", dataset %>% pull(location),
    ", ID: ", dataset %>% pull(site_id),").\n",
    unit_results$lab_subtitle
  )
  lab_fill <- unit_results$lab_fill
  fill_colors <- unit_results$fill_colors
  
  
  cap_results <- add_cap(dataset = dataset, var_qt = variable_of_interest_qt,
                         cap_value = cap_value, cap_color = cap_color)
  
  dataset <- cap_results$dataset
  lab_subtitle <- paste(lab_subtitle, cap_results$lab_subtitle_cap, sep="\n")
  cap_guide <- cap_results$cap_guide
  
  
  scale_results <- settings_dt_scale(dataset = dataset, start_date = NA, end_date = NA)
  
  dataset <- scale_results$dataset %>% rename(date_hour = timestamp)
  lab_title_sub <- scale_results$lab_title_sub
  lab_caption <- scale_results$lab_caption
  
  
  if (data_labels == TRUE) {
    data_labels <- geom_text(color = {{text_color}})
  } else { data_labels <- NULL }
  
  # Plotting data
  dataset %>% 
    ggplot(aes(
      x = lubridate::date(date_hour),
      y = lubridate::hour(date_hour),
      fill = {{variable_of_interest}},
      color = "",
      label = rounding_w_zeroes(val_orig, digits)
    )) +
    geom_tile() +
    fill_colors +
    data_labels +
    scale_y_continuous(
      # Reverse such that morning is at top, night at bottom
      trans = "reverse",
      # Labeling with proper hourly formatting
      labels = function(x) sprintf("%02d:00", x),
      # # Limits to force axis to always display 00 to 24 hr, regardless of availability
      # limits = c(24,-1),
      # Breaks every hour
      breaks = 0:23
    ) +
    scale_x_date(
      breaks = date_breaks,
      expand = c(0.01, 0.01),
      date_labels = "%d %b"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1),
      axis.ticks.x = element_line()
    ) +
    labs(
      title = paste(lab_title, lab_title_val, lab_title_sub),
      subtitle = lab_subtitle,
      fill = lab_fill,
      caption = lab_caption,
      y = "Hour of the day"
    ) + 
    scale_color_manual(values = "transparent") +
    cap_guide
}