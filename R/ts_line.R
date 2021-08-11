#' Time Series for multiple monitors' temporal atmospheric data
#'
#' Visualize temporal atmospheric data for multiple monitors, with optional minimum and maximum labels and a moving average.
#' Relevant information (such as date ranges, averaging methods, facets, and min/max values in the set) will be reported autmatically in the visualization.
#' @param dataset The hourly dataset to visualize
#' @param variable_of_interest The variable of interest (not in quotation marks) which to visualize
#' @param add_extrema Logical; label the extreme (minimum and maximum) valuess of the monitors of interest
#' @param digits Numeric; the number of digits to report
#' @param add_points Logical; add colored points for the \code{variable_of_interest}
#' @param add_average Logical; add a seprate plot for the average of ALL monitors in a set, to be a line colored by \link{avg_color}
#' @param label_filter Character list, optional (but recommended for large sets); list of monitor labels to be spotlighted (note: does NOT remove unspecified labels, which will remain as gray background lines)
#' @param cap_value Numeric, optional; values at or above to be colored serpately from the regular continuous scale. See \link{add_cap} for more information.
#' @param cap_color Character; color for values at or above the \code{cap_value}
#' @param single_column Logical; plot the data all as a single column, making cross-comparisons for monitors at specific times easier
#' @param label_length Character; applied if \code{single_column} is \code{TRUE}, the number of characters for which to wrap the monitor label
#' @param avg_color,min_color,max_color Character; colors for the average line, minimum text label, and maximum text label, respectively
#' @param location_data Data set containing latitude and longitude data
#' @return Data visualization: line graph time series spotlighting individual monitors, with background graphics representing the time series data for all other monitors in the set.
#' @examples 
#' ts_line(july_api_diurnal, pm25_atm, location_data = july_api_meta)
#' ts_line(july_api_hourly, pm25_atm, label_filter = c("\\bPSU\\b"), add_points = TRUE, single_column = TRUE, add_average = FALSE, location_data = july_api_meta)
#' @importFrom ggrepel geom_text_repel
#' @export
ts_line <- function(dataset, variable_of_interest,
                    add_extrema = TRUE, digits = 2,
                    add_points = FALSE,
                    add_average = TRUE, 
                    label_filter = c(""),
                    cap_value = NA, cap_color = "green",
                    single_column = FALSE, label_length = 10,
                    avg_color = "darkgreen", min_color = "royalblue", max_color = "darkorange",
                    location_data = data_meta){
  
  variable_of_interest_qt <- deparse(substitute(variable_of_interest))
  
  # Dropping NA values from the variable of interest
  dataset <- dataset %>% 
    drop_na({{variable_of_interest}})
  
  location_data <- location_data %>% 
    inner_join((dataset %>% distinct(site_id)))
  
  if (single_column == TRUE) {
    facet <- facet_grid(label~.)
    print("Charts will be arranged in a single column (multiple rows).")
    location_data <- location_data %>% mutate(label = str_wrap(label, label_length))
    print("Line breaks added to labels")
  } else {
    facet <- facet_wrap(~label)
    print("Charts will be arranged in multiple rows and columns.")
  }
  
  scale_results <- settings_dt_scale(dataset = dataset)
  
  dataset <- scale_results$dataset
  lab_title_sub <- scale_results$lab_title_sub
  x_angle <- scale_results$x_angle
  x_scale <- scale_results$x_scale
  lab_dates <- scale_results$lab_caption
  lab_caption <- paste("Black lines indiciate the data for a specific monitor of interest.",
                       "Gray lines represent data from all monitors.",
                       "\nMaximum and minimum values by monitor are marked by", max_color,
                       "and", min_color, "text, respectively.")
  
  
  dataset <- dataset %>% 
    left_join(location_data) %>% 
    select(timestamp, site_id, label, location, {{variable_of_interest}})
  
  input_labels <- paste0("(", paste(label_filter, collapse = ")|("), ")")
  
  label_order <- location_data %>% 
    select(site_id, label, latitude) %>% 
    distinct() %>% 
    mutate(label = fct_reorder(as.factor(label), desc(latitude))) %>% 
    pull(label)
  
  extrema <- dataset %>% 
    filter(str_detect(label, input_labels))
  
  if (add_extrema == TRUE) {
    extrema <- extrema %>% 
      group_by(site_id) %>% 
      mutate(
        date = lubridate::date(timestamp),
        max = case_when({{variable_of_interest}} == max({{variable_of_interest}}, na.rm = TRUE) ~ {{variable_of_interest}}),
        min = case_when({{variable_of_interest}} == min({{variable_of_interest}}, na.rm = TRUE) ~ {{variable_of_interest}})
      ) %>% 
      group_by(site_id, date) %>% 
      mutate_at(vars(max, min), ~replace(., duplicated(.), NA)) %>% 
      ungroup() %>% 
      select(!date)
  }
  
  if (add_average == TRUE) {
    
    extrema_avg <- dataset %>% 
      ungroup() %>% 
      select(timestamp, {{variable_of_interest}}) %>%
      group_by(timestamp) %>% 
      summarize(mean = mean({{variable_of_interest}}, na.rm = TRUE)) %>% 
      mutate(
        site_id = "Averages",
        label = "AVERAGE",
        location = "average"
      )
    
    if (add_extrema == TRUE) {
      extrema_avg <- extrema_avg %>% 
        mutate(
          date = lubridate::date(timestamp),
          max = case_when(mean == max(mean, na.rm = TRUE) ~ mean),
          min = case_when(mean == min(mean, na.rm = TRUE) ~ mean)
        ) %>%
        group_by(date, site_id, label) %>% 
        mutate_at(vars(max, min), ~replace(., duplicated(.), NA)) %>%
        ungroup() %>% 
        select(!date)
    }
    
    names(extrema_avg)[names(extrema_avg) == 'mean'] <- variable_of_interest_qt
    
    extrema <- rbind(extrema, extrema_avg)
    
    # Appending the "average" label onto the set
    label_order <- fct_inorder(c(levels(label_order), "AVERAGE"))
    
    print("Average data added.")
    
    lab_caption <- paste(
      lab_caption,
      "\nThe", avg_color, "colored graph represents the moving average of all monitors in the data set."
    )
    
  } else {
    label_order <- fct_inorder(levels(label_order))
    print("No averages will be added.")
  }
  
  dataset_full <- dataset %>% 
    select(!label)
  
  dataset <- extrema
  
  unit_results <- settings_units(dataset = dataset, var = variable_of_interest_qt,
                                 cap_color = NA, lab_title = "Timeseries of")
  
  lab_title <- unit_results$lab_title
  lab_title_val <- unit_results$lab_title_val
  lab_subtitle <- unit_results$lab_subtitle
  lab_fill <- unit_results$lab_fill
  y_lab <- lab_fill
  fill_colors <- unit_results$fill_colors
  
  
  cap_results <- add_cap(dataset = dataset, var_qt = variable_of_interest_qt,
                         cap_value = cap_value, cap_color = cap_color, type = "flag")
  
  dataset <- cap_results$dataset
  lab_subtitle <- paste(lab_subtitle, cap_results$lab_subtitle_cap, sep="\n")
  cap_guide <- cap_results$cap_guide
  
  
  if (add_points == TRUE) {
    shape_results <- settings_shapes(dataset = dataset)
    
    shape_set <- shape_results$shape_set
    shape_guide <- shape_results$shape_guide
    
    data_points <- geom_point(
      aes(fill = {{variable_of_interest}}),
      alpha = 0.7,
      stroke = 0
    )
    above_cap_points <- NULL
    
    if ("above_cap" %in% colnames(dataset)) {
      data_points <- geom_point(
        data = . %>% filter(above_cap == FALSE),
        aes(fill = {{variable_of_interest}}),
        alpha = 0.7,
        stroke = 0
      )
      above_cap_points <- geom_point(
        data = . %>% filter(above_cap == TRUE),
        color = {{cap_color}},
        fill = {{cap_color}},
        alpha = 0.7,
        stroke = 0
      )
    }
    
    print("Data points will be added.")
  } else {
    data_points <- NULL
    above_cap_points <- NULL
    shape_set <- NULL
    shape_guide <- NULL
  }
  
  if (add_extrema == TRUE) {
    dataset <- dataset %>%
      mutate_at(vars(max, min), ~case_when(!is.na(.) ~ rounding_w_zeroes(., digits))) %>% 
      mutate_at(vars(max, min), ~replace_na(., ""))
  }
  
  plot <- dataset %>%
    mutate(label = factor(as.character(label), label_order)) %>%
    ggplot(aes(
      x = timestamp,
      y = {{variable_of_interest}},
      color = "",
      group = label,
      shape = location
    )) +
    facet +
    geom_line(
      data = dataset_full,
      aes(group = site_id),
      color = "gray",
      alpha = 0.5
    ) +
    geom_line(
      data = . %>% filter(site_id != "Averages"),
      color = "black"
    ) +
    geom_line(
      data = . %>% filter(site_id == "Averages"),
      color = avg_color
    ) +
    data_points +
    above_cap_points +
    fill_colors +
    scale_color_manual(values = "transparent") +
    x_scale +
    shape_set +
    shape_guide +
    cap_guide +
    theme_minimal() +
    theme(
      panel.spacing.x = unit(5, "mm"),
      legend.position = "bottom",
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_text(angle = x_angle, hjust = 1),
      axis.title.x = element_blank(),
      axis.ticks.x = element_line()
    ) +
    labs(
      title = paste(lab_title, lab_title_val, lab_title_sub),
      subtitle = paste(lab_subtitle, lab_dates, sep = "\n"),
      caption = lab_caption,
      x = "Time",
      y = y_lab,
      fill = lab_fill,
      shape = "Monitor location:"
    )
  
  if (add_extrema == TRUE) {
    plot <- plot +
      geom_point(
        data = . %>% filter(max != ""),
        color = max_color,
        shape = 1
      ) +
      geom_point(
        data = . %>% filter(min != ""),
        color = min_color,
        shape = 1
      ) +
      geom_text_repel(
        aes(label = max),
        min.segment.length = 0,
        box.padding = 0.75,
        max.overlaps = Inf,
        color = max_color
      ) +
      geom_text_repel(
        aes(label = min),
        min.segment.length = 0,
        box.padding = 0.75,
        max.overlaps = Inf,
        nudge_y = max(dataset %>% pull({{variable_of_interest}}), na.rm = TRUE) / 5,
        color = min_color
      )
  }
  
  return(plot)
}