#' Visualization Settings for Specified Units
#'
#' A series of arguments to detect the appropriate visualization labels and settings to apply to a dataset provided said dataset and a varaible of interest.
#' Additionally, an optional "cap" value (and custom color) can be provided in order to distinguish values at or above said specified values to prevent color scale washout (e.g. if a few observations are extremely high to the point where all other values are washed out on the color scale).
#' @family {visualization settings}
#' @param dataset The dataset which to evaluate
#' @param var_qt Character; The variable of interest within the dataset, in quotation marks
#' @param cap_value Numeric; The value of the color "cap" to be applied
#' @param cap_color Character; The color of values at or above the \code{cap_value}.
#' @param digits Numeric; the number of digits to round data labels to.
#' @param lab_title Character; the first portion of the visualization title. It is recommended to adjust this to the type of visualization to be made.
#' @param lab_fill Character; the default unit label for the variable of interest (standalone, e.g. y-axis label of 'Units')
#' @param lab_unit Character; the default unit label for the variable of interest (in context of a sentence, e.g. 'X units')
#' @return List of values to be applied to various visualization methods:
#' \describe{
#'   \item{lab_title}{First portion of the visualization title.}
#'   \item{lab_title_val}{Shorthand unit description intended to be appended to the title.}
#'   \item{lab_subtitle}{Information with which varaible was applied, in addition to the minimum and maximum values of the set. Intended to be used as a subtitle or caption.}
#'   \item{lab_fill}{Full unit description intended to be applied to the fill label (or axis labels).}
#'   \item{fill_colors}{Colorblind-friendly fill color to be applied to a plot using \link[viridis]{scale_fill_viridis}, colored appropriately based on the variable of interest with matching min & max values if applicable:
#'     \itemize{
#'       \item Default: inferno, with the minimum set to 0
#'       \item PM: plasma, with the minimum set to 0
#'       \item humidity: mako (reversed direction), with the scale having a minimum of 0 and maximum of 100
#'       \item temperature (ºF or ºC): cividis, with no scale minimum or maximum
#'     }
#'   }
#'   \item{cap_guide}{Legend settings (see \link[ggplot2]{guides}) for arranging legend order in order to properly arrange values at or above the \code{cap_value}, and to set the aesthetics to match \code{cap_color} where appropraite.}
#' }
#' @examples 
#' \donttest{
#' units_results_example <- settings_units(july_api_daily, "pm25_atm", cap_value = 17, cap_color = "green")
#' ggplot(july_api_daily, aes(x = date, y = site_id, fill = pm25_atm)) +
#'   geom_tile() +
#'   labs(
#'     title = paste(units_results_example$lab_title, units_results_example$lab_title_val),
#'     fill = units_results_example$lab_fill
#'   )
#' remove(units_results_example)
#' }
#' @importFrom magrittr %>%
#' @export
settings_units <- function(dataset = dataset, var_qt = variable_of_interest_qt,
                           cap_value = NA, cap_color = "red", digits = 2,
                           lab_title = "Graph of", lab_fill = "Units", lab_unit = "units") {
  
  require(viridis)
  
  # Defaults
  lab_title_val <- var_qt
  # Color scale will default set to start at 0
  fill_colors <- viridis::scale_fill_viridis(option = "inferno", limits = c(0, NA), na.value = cap_color)
  
  if (stringr::str_detect(var_qt, "pm") == TRUE) {
    fill_colors <- viridis::scale_fill_viridis(option = "plasma", limits = c(0, NA), na.value = cap_color)
    lab_title_val <- "Particulate Matter (PM)"
    lab_unit <- '*mu*"g/m"^3*'
    
    if (stringr::str_detect(var_qt, "(25)|(2.5)") == TRUE) {
      pm_val <- 2.5
      print("PM 2.5 detected")
    } else if (stringr::str_detect(var_qt, "(1.0)|(01)|(\\D1$)|(1\\D)") == TRUE) {
      pm_val <- 1.0
      print("PM 1.0 detected")
    } else if (stringr::str_detect(var_qt, "(10)") == TRUE) {
      pm_val <- 10
      print("PM 10 detected")
    } else {
      pm_val <- NULL
      print("PM unit undetermined")
    }
    lab_fill <- parse(text = paste0('PM[', pm_val, ']~"("', lab_unit,'")"'))
    lab_unit <- "units"
  } else if (stringr::str_detect(var_qt, "((H|h)umid)|(rh)|(RH)") == TRUE) {
    # Adjusting color scale and labels if the variable of interest is RH
    lab_title_val <- "humidity"
    lab_unit <- "%"
    lab_fill <- paste0("Relative humidity (", lab_unit, ")")
    fill_colors <- viridis::scale_fill_viridis(option = "mako", direction = -1, limits = c(0, 100), end = 0.9, na.value = cap_color)
    print("RH detected as variable of interest; adjusting labels accordingly")
  } else if (stringr::str_detect(var_qt, "temp") == TRUE) {
    # Adjusting color scale and labels if the variable of interest is ambient temperature
    lab_title_val <- "ambient temperature"
    fill_colors <- viridis::scale_fill_viridis(option = "cividis", begin = 0.15, na.value = cap_color)
    print("Temperature detected as variable of interest; adjusting labels accordingly")
    lab_unit <- "\u00B0F"
    
    if (stringr::str_detect(var_qt, "_c") == TRUE) {
      lab_unit <- "\u00B0C"
      print("Temperature detected to be in Celsius")
    } else { print("Temperature assumed to be in Fahrenheit") }
    
    lab_fill <- paste0("Ambient temperature (", lab_unit, ")")
    
    if (stringr::str_detect(var_qt, "internal") == TRUE) {
      print("Internal temperature (not raw/internal) detected")
      lab_title_val <- "internal temperature"
      lab_fill <- paste0("Internal temperature (", lab_unit, ")")
    }
  }
  
  # Getting min and max values from the data set
  val_min <- dataset %>%
    dplyr::ungroup() %>%
    dplyr::select_at(dplyr::vars(var_qt)) %>%
    min(na.rm = TRUE)
  val_max <- dataset %>%
    dplyr::ungroup() %>%
    dplyr::select_at(dplyr::vars(var_qt)) %>%
    max(na.rm = TRUE)
  
  lab_subtitle <- paste0("Variable plotted: ",
                         var_qt,
                         ", with a reported range of ",
                         round(val_min, digits = digits),
                         " to ",
                         round(val_max, digits = digits),
                         " ", lab_unit, ".")
  
  cap_guide <- guides(fill = guide_colorbar(order = 1, barwidth = 10),
                      color = "none")
  
  # If manually applying a max filter value
  if (is.na(cap_value) == FALSE) {
    # Getting number of rows at or above the cap
    nrow_hi <- dataset %>% 
      dplyr::filter_at(dplyr::vars({{var_qt}}),  ~.>= cap_value) %>% 
      nrow()
    
    if ((nrow_hi > 0) == TRUE) {
      # Replacing the values above the set max to be NA so that they will be colored differently on the map
      dataset <- dataset %>% 
        dplyr::mutate_at(dplyr::vars({{var_qt}}), ~replace(., which(.>={{cap_value}}), NA))
      
      # Updated lab caption to include the filter
      lab_subtitle <- paste0("Color scale manually capped at ",
                             cap_value, " units; all higher values colored ", cap_color,
                             ".\n", lab_subtitle)
      
      # Feedback
      print(paste(
        "Values greater than or equal to",
        {{cap_value}}, "in", {{var_qt}},
        "will be colored", cap_color
      ))
      
      cap_guide <- ggplot2::guides(fill = ggplot2::guide_colorbar(order = 1, barwidth = 10),
                          color = ggplot2::guide_legend(
                            title = paste0(cap_value, "+"),
                            order = 2,
                            title.position = "bottom",
                            title.theme = ggplot2::element_text(size = 10),
                            override.aes = list(color = cap_color, fill = cap_color)
                          ))
    } else {
      print(paste0(
        "No values greater than or equal to ",
        {{cap_value}}, " found in ", {{var_qt}},
        "; color cap will not be applied."
      ))
    }
  }
  
  return(list(
    lab_title = lab_title,
    lab_title_val = lab_title_val,
    lab_subtitle = lab_subtitle,
    lab_fill = lab_fill,
    fill_colors = fill_colors,
    cap_guide = cap_guide
  ))
}