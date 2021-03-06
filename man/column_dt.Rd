% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/column_dt.R
\name{column_dt}
\alias{column_dt}
\title{Adding new date/time column(s)}
\usage{
column_dt(dataset, unit)
}
\arguments{
\item{dataset}{The dataset for which to convert timezone data (column: "datetime")}

\item{unit}{The unit(s) for which to apply to the dataset. Options: "date", "date_hour", "hour", "hour_minute", "time"}
}
\value{
Dataset with column(s) of newly specified time unit by row:
\describe{
\item{date}{Column of the time stamp's date; class 'Date'.}
\item{date_hour}{Column of the time stamp's date and time, rounded down by hour; class 'POSIXct','POSIXt'.}
\item{hour}{Column of the time stamp's time, rounded down by hour; class 'hms', 'difftime'.}
\item{hour_minute}{Column of the time stamp's time, rounded by minute; class 'hms', 'difftime'.}
\item{time}{Column of the time stamp's time; class 'hms', 'difftime'.}
}
}
\description{
Creating a new column in a data set with a specified date or time rounding/conversion
}
\examples{
column_dt(head(july_api_full)[1:3], c("date", "hour_minute"))
column_dt(head(july_api_hourly)[1:3], "hour")
}
\seealso{
Other {miscellaneous functions}: 
\code{\link{adjust_timezone}()},
\code{\link{ambient_temperature}()},
\code{\link{apply_date_tags}()},
\code{\link{apply_hour_tags}()},
\code{\link{filter_df}()},
\code{\link{group_stad}()},
\code{\link{organize_stad}()},
\code{\link{rounding_w_zeroes}()},
\code{\link{unit_convert}()},
\code{\link{wrangle_meta}()}
}
\concept{{miscellaneous functions}}
