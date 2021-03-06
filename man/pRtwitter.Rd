% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pRtwitter.R
\name{pRtwitter}
\alias{pRtwitter}
\title{Puerto Rico Function}
\usage{
pRtwitter(searchterm = "Puerto Rico", n = 1e+05, since = "2017-10-25",
  until = "2017-10-26",
  olddataname = "Puerto Rico 23 Sept - 25 OCT best.csv",
  newdataname = "Puerto Rico 23 Sept - 26 OCT best.csv")
}
\arguments{
\item{searchterm}{"Puerto Rico"}

\item{n}{"100000"}

\item{since}{'2017-10-25'}

\item{until}{'2017-10-26'}

\item{olddataname}{"Puerto Rico 23 Sept - 25 OCT best.csv"}

\item{newdataname}{"Puerto Rico 23 Sept - 26 OCT best.csv"}
}
\description{
pRtwitter: This function scrapes twitter for the specified keyword, appends it to your dataset, and saves an updated CSV to the working directory
}
\examples{
pRtwitter()
}
\keyword{Puerto}
\keyword{Rico}
