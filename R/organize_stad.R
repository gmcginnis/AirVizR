#' Combine and/or Pivot STAD Data Set(s)
#' 
#' Choose to combine multiple STAD data sets, and/or pivot columns. Useful for combining FRM and PA data frames, or pivoting columns of the same pollutant type.
#' @family {miscellaneous functions}
#' @seealso \code{\link{filter_df()}}
#' @param dataset Dataset(s) to pivot. If more than one data set is provided, they will be row-bound. Use \code{list()} if more than one.
#' @param measurements Character, optional. Column headers to pivot. Use \code{c()}.
#' @param col_rename Character, optional, only run if \code{measurements} provided. The character to rename the 'value' column output of the pivot to.
#' @param keep_cols Logical, only run if \code{measurements} provided. Keep (\code{TRUE}) or disgard (\code{FALSE}) the non-identifying columns that have not been pivoted.
#' @param drop_missing Logical, only run if \code{measurements} provided. Drop missing values from pivoted column. Recommended especially if \code{keep_cols} is \code{FALSE}.
#' @return Data set.
#' @examples 
#' # Pivot columns of a similar pollutant:
#' head(organize_stad(july_api_daily, c(pm25_epa_2021, pm25_epa_2020, pm25_lrapa), "pm25"))
#' # Combine PA and FRM data:
#' head(organize_stad(list(july_api_daily, july_frm_daily), c(pm25_epa_2021, pm25_frm)))
#' # Combine meta data:
#' tail(organize_stad(list(july_api_meta, july_frm_meta)))
#' @export
organize_stad <- function(dataset, measurements, col_rename, keep_cols = FALSE, drop_missing = TRUE){
  
  if(is.list(dataset) & length(dataset) > 1){
    print("Data sets now row-bound")
    dataset <- plyr::rbind.fill(dataset)
  }
  
  if(missing(measurements) == FALSE){
    
    dataset <- tidyr::pivot_longer(dataset, cols = {{measurements}}, names_to = "measurement")
    
    if(keep_cols == FALSE){
      print("Keeping only identifying and newly-pivoted columns.")
      dataset <- dplyr::select(dataset,
                               site_id,
                               intersect(
                                 c("datetime", "date_hour", "date", "hour", "hour_minute", "time",
                                   "date_tag", "hour_tag"),
                                 colnames(dataset)
                               ),
                               measurement, value)
      
    } else {print("Keeping all columns.")}
    
    if(drop_missing == TRUE){
      print("Dropping missing values from newly-pivoted column 'value'.")
      dataset <- tidyr::drop_na(dataset, value)
    } else { print("Not dropping missing values from newly-pivoted column 'value'.") }
    
    if(missing(col_rename) == FALSE){
      print(paste0("Renaming newly-pivoted column 'value' to '", col_rename, "'."))
      names(dataset)[names(dataset) == 'value'] <- col_rename
    }
    
  }
  
  return(dataset)
}

