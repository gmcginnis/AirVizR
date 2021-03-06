% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_oa.R
\name{map_oa}
\alias{map_oa}
\title{View moving data on an interactive map}
\usage{
map_oa(
  dataset,
  variable_of_interest,
  palette_color = "viridis",
  provider_tiles = "CartoDB.DarkMatter"
)
}
\arguments{
\item{dataset}{Data set to visualize. Requires \code{latitude} and \code{longitude} columns.}

\item{variable_of_interest}{blah}

\item{palette_color}{Character; any valid input for the \code{palette} argument for \code{\link{colorNumeric()}}.}

\item{provider_tiles}{Character or provider source; see \code{provider} argument for \code{\link{addProviderTiles()}} for a link to lists.}
}
\value{
Interactive leaflet map.
}
\description{
Visualize data that involves lat/long movement.
}
\examples{
map_oa(oa_moving_full, black_carbon)
}
\seealso{
\code{\link{map_stad()}}

Other {STAD visualizations}: 
\code{\link{heatmap_cross}()},
\code{\link{heatmap_single}()},
\code{\link{map_stad}()},
\code{\link{ts_line}()},
\code{\link{ts_variation}()}
}
\concept{{STAD visualizations}}
