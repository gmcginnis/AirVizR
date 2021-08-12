#' Apply Corrections & Tags to Dataset
#'
#' Average a dataset by specified time stamp groupings, apply correction factors (using \link{apply_corrections}, which uses \link{apply_epa} and \link{apply_lrapa}), and tag dates/hours (\link{apply_date_tags} and \link{apply_hour_tags}, respectively) as appropriate.
#' See \link{apply_date_tags} and \link{apply_hour_tags} for more information individually.
#' @param dataset The dataset for which to apply the date tags to.
#' @param by_day Logical; Average by day
#' @param by_hour Logical; Average by hour
#' @param tag_dates Logical; Apply date tags
#' @param tag_hours Logical; Apply hour tags
#' @return Dataframe with new columns for corrected values and appropriate tagged values ("date_tag" and "hour_tag") containing the appropriate hour/date tag with respect to each row's timestamp.
#' @examples 
#' \donttest{
#' apply_functions(july_api_full, by_day = TRUE, tag_dates = FALSE, tag_hours = FALSE)
#' }
#' @export
apply_functions <- function(dataset, by_day = TRUE, by_hour = FALSE, tag_dates = run_date_grouping, tag_hours = run_hour_grouping){
  
  dataset <- apply_corrections(dataset, daily = by_day, hourly = by_hour)
  print("Correction factors applied.")
  
  if (by_day == TRUE & tag_dates == TRUE) {
    dataset <- apply_date_tags(dataset)
    print("Data now tagged by provided date groupings")
  } else { print("Date groups not applied") }
  
  if (by_hour == TRUE & tag_hours == TRUE) {
    dataset <- apply_hour_tags(dataset)
    print("Data now tagged by provided hour groupings")
  } else { print("Hour groups not applied") }
  
  return(dataset)
}