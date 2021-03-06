% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-extraction.R
\name{error.checks}
\alias{error.checks}
\title{Meta Object Creation with Error Checks}
\usage{
error.checks(startDate, endDate, tz, version)
}
\arguments{
\item{startDate}{a user defined startDate ("YYYY-MM-DD")}

\item{endDate}{a user defined end Date ("YYYY-MM-DD")}

\item{tz}{a user defined timezone}

\item{version}{a user defined model version}
}
\value{
a list containing the model version, NCML path, and time requests
}
\description{
this function ensures that the all input objects are correct.
User input is cohersed to the desired TZ, model versions are checked, and the NCML file path returned
}
\keyword{internal}
