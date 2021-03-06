% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{readNWMdata}
\alias{readNWMdata}
\title{NWM Reanalysis Extraction}
\usage{
readNWMdata(
  comid = NULL,
  siteID = NULL,
  startDate = "1993-01-01",
  endDate = "2018-12-31",
  tz = "UTC",
  version = 2
)
}
\arguments{
\item{comid}{a NHD common identifier}

\item{startDate}{a start date (YYYY-MM-DD) or (YYYY-MM-DD HH)}

\item{endDate}{an end date (YYYY-MM-DD) or (YYYY-MM-DD HH)}

\item{tz}{the desired timezone of the data. Can be found with `OlsonNames()`}

\item{version}{the NWM version to extract (current = 1.2 or 2.0 (default))}

\item{siteNumber}{a USGS NWIS site number (eight digits)}
}
\value{
data.frame
}
\description{
Download hourly flow values for an NHD COMID from the National 
Water Model version 1.2 or 2.0. Returned data is available between 
"1993-01-01 00" and "2017-12-31 23" but can be 
subset using a startDate and endDate.
}
