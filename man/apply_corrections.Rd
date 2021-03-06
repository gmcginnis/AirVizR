% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_corrections.R
\name{apply_corrections}
\alias{apply_corrections}
\title{Apply EPA & LRAPA Correction Factors}
\usage{
apply_corrections(dataset, daily = TRUE, hourly = FALSE, epa = 75)
}
\arguments{
\item{dataset}{The dataset for which to apply the correction factors to}

\item{by_day}{Logical; average data by day}

\item{by_hour}{Logical; average data by hour}

\item{epa_percent}{Numeric; Minimum percentage of data required to be included}

\item{keep_cols}{Logical; Keep or disgard extra columns. If FALSE, only identifying columns and EPA-corrected columns will remain}
}
\value{
Dataframe with new columns for EPA-corrected and LRAPA-corrected PM2.5.
}
\description{
Apply EPA correction factors to PurpleAir PM2.5 data: a combination of \link{apply_lrapa} and \link{apply_epa}.
}
\examples{
apply_corrections(july_api_full)
\dontrun{apply_corrections(july_api_full, hourly = TRUE)}
}
\seealso{
Other {PA functions}: 
\code{\link{apply_epa}()},
\code{\link{apply_functions}()},
\code{\link{apply_lrapa}()},
\code{\link{apply_qc}()}
}
\concept{{PA functions}}
