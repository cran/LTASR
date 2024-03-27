#' Calculate SMRs for Major groupings
#'
#' `smr_major` will collapse minor outcomes into "major" groupings as defined in the
#' rate object, `rateobj`.
#'
#' @param smr_minor_table A data.frame/tibble as created by `smr_minor` containing
#' observed and expected number of deaths for each minor outcome
#' @param rateobj A rate object created by `parseRate`, or the included rate object `us_119ucod_19602021`.
#'
#' @return A data.frame/tibble containing the expected and observed number of deaths
#' as well as SMRs, lower CI and upper CI for each major as defined in the rate object
#' `rateobj`
#' @export
#'
#' @examples
#' library(LTASR)
#' library(dplyr)
#'
#' #Import example person file
#' person <- person_example %>%
#'   mutate(dob = as.Date(dob, format='%m/%d/%Y'),
#'          pybegin = as.Date(pybegin, format='%m/%d/%Y'),
#'          dlo = as.Date(dlo, format='%m/%d/%Y'))
#'
#' #Import default rate object
#' rateobj <- us_119ucod_19602021
#'
#' #Stratify person table
#' py_table <- get_table(person, rateobj)
#'
#' #Calculate SMRs for all minors
#' smr_minor_table <- smr_minor(py_table, rateobj)
#'
#' #Calculate SMRs major groupings found within rate file
#' smr_major(smr_minor_table, rateobj)
#'
smr_major <- function(smr_minor_table, rateobj){
  exact_lower <- purrr::map_dbl(0:20,
                                \(.x) {stats::optim(.x, function(x) abs(1-stats::ppois((.x - 1), x) - .05/2))$par})
  exact_upper <- purrr::map_dbl(0:20,
                                \(.x) {stats::optim(.x, function(x) abs(stats::ppois(.x, x) - .05/2))$par})
  smr_minor_table %>%
    dplyr::left_join(rateobj$MinorDesc, by='minor') %>%
    dplyr::group_by(major, maj_desc) %>%
    dplyr::summarize(observed = sum(observed, na.rm = TRUE),
              expected = round(sum(expected, na.rm = TRUE), 2)) %>%
    dplyr::mutate(smr = observed / expected,
                  lower = if_else(observed <= 20,
                                  exact_lower[observed+1] /expected,
                                  (observed*(1 - (1/(9*observed)) - (1.96/(3*sqrt(observed))))**3)  /expected),
                  upper = if_else(observed <= 20,
                                  exact_upper[observed+1] /expected,
                                  ((observed+1)*(1 - (1/(9*(observed+1))) + (1.96/(3*sqrt((observed+1)))))**3)/expected)) %>%
    dplyr::select(major, Description = maj_desc, observed, expected, smr, lower, upper) %>%
    dplyr::arrange(major)
}
