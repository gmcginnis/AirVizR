% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_variation.R
\name{ts_variation}
\alias{ts_variation}
\title{Time Series Variation for multiple monitors' temporal atmospheric data}
\usage{
ts_variation(
  dataset,
  pollutants,
  group,
  subset = c("hour", "day.hour", "day", "month"),
  include,
  exclude,
  location_data = data_meta,
  color = "Dark2"
)
}
\arguments{
\item{dataset}{The data set to visualize. This function works best with unaveraged or hourly-averaged data.}

\item{pollutants}{Character, optionally a list; in quotation marks, the pollutant(s) to map. Note that if multiple are specified, \code{group} must remain unspecified.}

\item{group}{Character; in quotation marks, the variable for which to group the data (i.e. what will distinguish values from one another). Note that if specified, \code{pollutants} must be singular.}

\item{subset}{Character; in quotation marks, the specific plot to report. Either all (default; c("hour", "day.hour", "day", "month")) or one (from the default list) is permitted.}

\item{include}{Character, optional; label(s) of monitors to include. See \link{filter_df} for more information.}

\item{exclude}{Character, optional; label(s) of monitors to exclude. See \link{filter_df} for more information.}

\item{location_data}{Data set containing label information}

\item{color}{Character; color palette to use. For a full list of other defaults, see \link[openair]{openColours}.}
}
\value{
Data visualization: diurnal, hour of day, day of the week, and/or monthly time series variation of specified pollutant(s).
}
\description{
Please note that this function will take the longest of all the visualization options.
Visualize variation in temporal atmospheric data for multiple monitors and (optionally) multiple pollutants; a customization of \link[openair]{timeVariation} from \link[openair]{openair}.
Relevant information (such as type of pollutant(s) and temporal scale of data set) will be reported automatically in the visualization, however limitations to the plot settings prevent all relevant information from being included.
}
\examples{
\donttest{
ts_variation(july_api_hourly, "pm25_atm", include = "Lighthouse", group = "date_tag", location_data = july_api_meta)
ts_variation(july_api_hourly, c("pm25_atm", "temperature_ambient"), location_data = july_api_meta, subset = "hour")
}
}
\seealso{
\code{\link[openair]{timeVariation()}}

Other {STAD visualizations}: 
\code{\link{heatmap_cross}()},
\code{\link{heatmap_single}()},
\code{\link{map_oa}()},
\code{\link{map_stad}()},
\code{\link{ts_line}()}
}
\concept{{STAD visualizations}}
