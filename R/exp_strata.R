#' Create exp_strata object
#'
#' `exp_strata()` creates an exp_strata that defines which variable to consider,
#' any lag to be applied, and cutpoints for the strata.
#'
#' @param var character naming the variable within the history data.frame to consider.
#' @param cutpt numeric vector defining the cutpoints to use to stratify the calculated cumulative exposure for variable `var`.
#'  Should include min and max values (typically -Inf and Inf).
#' @param lag numeric defining the lag, in years, to be applied to exposure variables. Default is 0 yrs (i.e. unlagged).
#'  Must be a whole number.
#'
#' @return
#' an object of class `exp_strata` to be used in the `get_table_history()`.
#' @export
#'
#' @examples
#' library(LTASR)
#' exp1 <- exp_strata(var = 'employed',
#'                    cutpt = c(-Inf, 365, Inf),
#'                    lag = 10)
#'
exp_strata <- function(var = character(),
                       cutpt = numeric(),
                       lag = 0) {
  stopifnot(is.character(var))
  stopifnot(is.numeric(cutpt))
  stopifnot(is.numeric(lag))
  stopifnot(floor(lag) == lag)

  structure(list(var = var,
                 cutpt = cutpt,
                 lag = lag),
            class = 'exp_strata')
}

