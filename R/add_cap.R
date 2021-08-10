#' Cap a Numeric Variable
#'
#' Apply a cap value of a specified color to numeric variables, either by replacing them with 'NA', or flagging them in a new column.
#' @param dataset Data set for which to apply the cap to
#' @param var_qt Character; the variable of interest (in quotation marks) for which to apply the cap
#' @param cap_value Numeric; the value of the specified variable (\code{var_qt}) at and above to be capped
#' @param cap_color Character; the color which will be applied to values at or above \code{cap_value}
#' @param type Character choice: "filter" or "flag"; the former will replace values at or above \code{cap_value} with 'NA', while the latter will add a logical column ("above_cap") for if the respective row's value is at or above the \code{cap_value}.
#' @return Character versions rounded to the number of digits specified, including trailing zeroes 
#' @examples 
#' cap_results <- add_cap(data_pm25, "pm25_atm", 150, "red")
#' ggplot() + cap_results$cap_guide + labs(subtitle = cap_results$lab_subtitle_cap)
#' @export
add_cap <- function(dataset, var_qt, cap_value, cap_color, type = "filter") {
  
  # Defaults
  lab_subtitle_cap <- ""
  cap_guide <- guides(fill = guide_colorbar(order = 1, barwidth = 10),
                      color = "none")
  
  # If manually applying a max filter value
  if (is.na(cap_value) == FALSE) {
    
    nrow_ds <- nrow(dataset)
    
    # Getting number of rows at or above the cap
    nrow_hi <- dataset %>% 
      filter_at(vars({{var_qt}}), ~.>= cap_value) %>% 
      nrow()
    
    if (nrow_hi == 0) {
      print(paste0(
        "No values of ", {{var_qt}}, " are greater than or equal to ",
        {{cap_value}}, "; color cap not applied."
      ))
    } else if (nrow_hi == nrow_ds) {
      print(paste0(
        "All values of ", {{var_qt}}," are greater than or equal to ",
        {{cap_value}}, "; color cap not applied."
      ))
    } else if ((nrow_hi > 0) == TRUE) {
      
      if (type == "filter") {
        # Replacing the values above the set max to be NA so that they will be colored differently
        dataset <- dataset %>% 
          mutate_at(vars({{var_qt}}), ~replace(., which(.>={{cap_value}}), NA))
      } else if (type == "flag") {
        dataset <- dataset %>% 
          mutate(above_cap = case_when(
            !!sym({{var_qt}}) >= {{cap_value}} ~ TRUE,
            !!sym({{var_qt}}) < {{cap_value}} ~ FALSE
          ))
      }
      
      # Updated lab caption to include the filter
      lab_subtitle_cap <- paste0("Color scale manually capped at ",
                                 cap_value, " units; all higher values colored ", cap_color)
      
      # Feedback
      print(paste(
        "Values greater than or equal to",
        {{cap_value}}, "in", {{var_qt}},
        "will be colored", cap_color
      ))
      
      cap_guide <- guides(
        fill = guide_colorbar(
          order = 1,
          barwidth = 10
        ),
        color = guide_legend(
          title = paste0(cap_value, "+"),
          order = 2,
          title.position = "bottom",
          title.theme = element_text(size = 8.5),
          override.aes = list(
            color = cap_color,
            fill = cap_color,
            shape = 22,
            size = 7
          )
        ))
    } else { print("Error.") }
  }
  
  return(list(
    dataset = dataset,
    lab_subtitle_cap = lab_subtitle_cap,
    cap_guide = cap_guide
  ))
}