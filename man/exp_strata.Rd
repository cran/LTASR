% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exp_strata.R
\name{exp_strata}
\alias{exp_strata}
\title{Create exp_strata object}
\usage{
exp_strata(var = character(), cutpt = numeric(), lag = 0)
}
\arguments{
\item{var}{character naming the variable within the history data.frame to consider.}

\item{cutpt}{numeric vector defining the cutpoints to use to stratify the calculated cumulative exposure for variable \code{var}.
Should include min and max values (typically -Inf and Inf).}

\item{lag}{numeric defining the lag, in years, to be applied to exposure variables. Default is 0 yrs (i.e. unlagged).
Must be a whole number.}
}
\value{
an object of class \code{exp_strata} to be used in the \code{get_table_history()}.
}
\description{
\code{exp_strata()} creates an exp_strata that defines which variable to consider,
any lag to be applied, and cutpoints for the strata.
}
\examples{
library(LTASR)
exp1 <- exp_strata(var = 'employed',
                   cutpt = c(-Inf, 365, Inf),
                   lag = 10)

}
