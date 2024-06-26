% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smr_minor.R
\name{smr_minor}
\alias{smr_minor}
\title{Calculate SMRs for Minors}
\usage{
smr_minor(py_table, rateobj)
}
\arguments{
\item{py_table}{A stratified cohort created by \code{get_table}, or the included rate object \code{us_119ucod_19602021}.}

\item{rateobj}{A rate object created by \code{parseRate}}
}
\value{
A dataframe/tibble containing the expected and observed number of deaths
as well as SMRs, lower CI and upper CI for each minor found in the rate object
\code{rateobj}
}
\description{
\code{smr_minor} calculates SMRs for all minor groupings found within the rate
object, \code{rateobj}, for the stratified cohort \code{py_table}
}
\examples{
library(LTASR)
library(dplyr)

#Import example person file
person <- person_example \%>\%
  mutate(dob = as.Date(dob, format='\%m/\%d/\%Y'),
         pybegin = as.Date(pybegin, format='\%m/\%d/\%Y'),
         dlo = as.Date(dlo, format='\%m/\%d/\%Y'))

#Import default rate object
rateobj <- us_119ucod_19602021

#Stratify person table
py_table <- get_table(person, rateobj)

#Calculate SMRs for all minors
smr_minor(py_table, rateobj)

}
