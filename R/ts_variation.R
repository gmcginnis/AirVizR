#' Time Series Variation for multiple monitors' temporal atmospheric data
#'
#' Please note that this function will take the longest of all the visualization options.
#' Visualize variation in temporal atmospheric data for multiple monitors and (optionally) multiple pollutants; a customization of \link[openair]{timeVariation} from \link[openair]{openair}.
#' Relevant information (such as type of pollutant(s) and temporal scale of data set) will be reported automatically in the visualization, however limitations to the plot settings prevent all relevant information from being included.
#' @param dataset The data set to visualize. This function works best with unaveraged or hourly-averaged data.
#' @param pollutants Character, optionally a list; in quotation marks, the pollutant(s) to map. Note that if multiple are specified, \code{group} must remain unspecified.
#' @param group Character; in quotation marks, the variable for which to group the data (i.e. what will distinguish values from one another). Note that if specified, \code{pollutants} must be singular.
#' @param subset Character; in quotation marks, the specific plot to report. Either all (default; c("hour", "day.hour", "day", "month")) or one (from the default list) is permitted.
#' @param include Character, optional; label(s) of monitors to include. See \link{filter_df} for more information.
#' @param exclude Character, optional; label(s) of monitors to exclude. See \link{filter_df} for more information.
#' @param location_data Data set containing label information
#' @param color Character; color palette to use. For a full list of other defaults, see \link[openair]{openColours}.
#' @return Data visualization: diurnal, hour of day, day of the week, and/or monthly time series variation of specified pollutant(s).
#' @examples 
#' ts_variation(july_api_hourly, "pm25_atm", include = "Lighthouse", group = "date_tag", location_data = july_api_meta)
#' ts_variation(july_api_hourly, c("pm25_atm", "temperature_ambient"), location_data = july_api_meta, subset = "hour")
#' @importFrom openair timeVariation
#' @export
ts_variation <- function(dataset, pollutants, group,
                         subset = c("hour", "day.hour", "day", "month"),
                         include, exclude,
                         location_data = data_meta,
                         color = "Dark2"){
  
  if (missing(include) == FALSE) {
    dataset <- filter_df(dataset, include = include, location_data = location_data)
  }
  if (missing(exclude) == FALSE) {
    dataset <- filter_df(dataset, exclude = exclude, location_data = location_data)
  }
  
  if (length(pollutants) == 1) {
    
    unit_results <- settings_units(dataset = dataset, var = pollutants,
                                   cap_color = NA, lab_title = "Timeseries variation of")
    
    lab_title <- unit_results$lab_title
    lab_title_val <- unit_results$lab_title_val
    y_lab <- unit_results$lab_fill
    
  } else {
    print("Multiple pollutants selected. Take caution if mixing units, as all will be plotted on ONE y-axis!")
    lab_title <- "Timeseries variation of"
    # y_lab <- paste(pollutants, collapse = ", ")
    
    y_lab <- ""
    lab_title_val <- ""
    corr <- ""
    
    for (i in pollutants) {
      unit_results <- settings_units(dataset = dataset %>% slice(1), var = i,
                                     cap_color = NA, lab_title = "Timeseries variation of")
      
      lab_title_val <- paste(lab_title_val, unit_results$lab_title_val, sep = ", ") %>% 
        str_replace("(?<=,|^)([^,]*)(,\\1)+(?=,|$)", "\\1")
      
      if (i != "temperature" & i != "humidity") {
        corr_new <- paste(i) %>%
          str_replace("pm25", "") %>%
          str_replace("temperature", "") %>%
          str_replace("^_", "")
      } else { corr_new <- paste("raw") }
      
      corr <- paste(corr, corr_new, sep = ", ")
    }
    
    corr_final <- paste0("[", corr %>% str_replace("^, ", ""), "]")
    
    lab_title_val <- lab_title_val %>%
      str_replace_all(", ", " & ") %>% 
      str_replace("^ & ", "")
    
    lab_title_val <- paste(lab_title_val, corr_final)
  }
  
  scale_results <- settings_dt_scale(dataset = dataset)
  lab_title_sub <- scale_results$lab_title_sub
  
  if ("date" %in% colnames(dataset) == FALSE) {
    # dataset <- (settings_dt_scale(dataset))$dataset %>% rename(date = timestamp)
    dataset <- scale_results$dataset %>% rename(date = timestamp)
  }
  
  title <- paste(lab_title, lab_title_val, lab_title_sub) %>% str_replace(" across time", "")
  
  if (missing(group) == TRUE) {
    plot_result <- timeVariation(dataset, pollutant = pollutants, cols = color, main = title, ylab = y_lab)
  } else {
    if (group %in% colnames(dataset) == TRUE) {
      plot_result <- timeVariation(dataset, pollutant = pollutants, cols = color, main = title, ylab = y_lab, group = group)
    } else {
      print(paste("ERROR: grouping variable", group, "not found in dataset. Plot will not be grouped."))
      plot_result <- timeVariation(dataset, pollutant = pollutants, cols = color, main = title, ylab = y_lab)
    }
  }
  
  if (length(subset) == 1) {
    if (subset %in% c("hour", "day.hour", "day", "month") == FALSE) {
      print("Invalid plotting subset value. Defaulting to plotting all.")
      pluck <- NULL
    } else {
      print(paste("Plot selected:", subset))
      pluck <- subset
    }
  } else { pluck <- NULL }
  remove(subset)
  
  plot_final <- plot_result %>%
    plot(subset = pluck)
  
  return(plot_final)
}