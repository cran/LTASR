% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapDeaths.R
\name{mapDeaths}
\alias{mapDeaths}
\title{Map ICD codes to grouped minors}
\usage{
mapDeaths(persondf, rateobj)
}
\arguments{
\item{persondf}{Person data.frame}

\item{rateobj}{A rate object created from \code{parseRate}, or the included rate object \code{us_119ucod_19602021}.}
}
\value{
A data.frame for each death observed in the person file with the following variables:
id, code, rev: from the \code{persondf}
minor: the minor/outcome from the rate file that the death was mapped to
}
\description{
Map ICD codes to grouped minors
}
\examples{
library(LTASR)

#Import example person file
person <- person_example

#Import default rate object
rateobj <- us_119ucod_19602021

#Check mapping of deaths to minors/outcomes
mapDeaths(person, rateobj)

}
