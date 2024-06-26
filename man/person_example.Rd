% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_person.R
\docType{data}
\name{person_example}
\alias{person_example}
\title{Example Person File for Testing}
\format{
A tibble with 3 observations and 9 variables:
\describe{
\item{id}{unique identifier; character}
\item{gender}{Gender/Sex; character 'M' or 'F'}
\item{race}{Race; character 'W' or 'N'}
\item{dob}{Date of Birth; character to be converted to date}
\item{pybegin}{date to begin follow-up/at-risk accumulation, character to be converted to date}
\item{dlo}{Date last observed; character to be converted to date}
\item{vs}{indicator identifying the vital status of the cohort. A value of 'D' indicates an observed death; character}
\item{rev}{ICD revision of the ICD code; numeric}
\item{code}{ICD-code for the cause of death; character}
...
}
}
\source{
{Internally Generated}
}
\usage{
person_example
}
\description{
A tibble containing example person file data to be used for testing and
demonstration of the package
}
\keyword{datasets}
