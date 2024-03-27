#' Calculate SMRs for Minors
#'
#' `smr_minor` calculates SMRs for all minor groupings found within the rate
#' object, `rateobj`, for the stratified cohort `py_table`
#'
#' @param py_table A stratified cohort created by `get_table`, or the included rate object `us_119ucod_19602021`.
#' @param rateobj A rate object created by `parseRate`
#'
#' @return A dataframe/tibble containing the expected and observed number of deaths
#' as well as SMRs, lower CI and upper CI for each minor found in the rate object
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
#' smr_minor(py_table, rateobj)
#'
smr_minor <- function(py_table, rateobj){
  exact_lower <- purrr::map_dbl(0:20,
                         \(.x) {stats::optim(.x, function(x) abs(1-stats::ppois((.x - 1), x) - .05/2))$par})
  exact_upper <- purrr::map_dbl(0:20,
                               \(.x) {stats::optim(.x, function(x) abs(stats::ppois(.x, x) - .05/2))$par})

  # Collapse py_table
  py_table <- py_table %>%
    #dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, .data$expCat, !!!strata) %>%
    dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race) %>%
    dplyr::summarise_at(.vars=dplyr::vars(.data$pdays, dplyr::starts_with('_o')), .funs = sum, na.rm=T)

  rateobj$rates %>%
    tidyr::pivot_wider(values_from = rate, names_from = minor, names_prefix = '_r') %>%
    dplyr::right_join(py_table, by=c('gender',
                              'race',
                              'ageCat',
                              'CPCat' )) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with('_')) %>%
    dplyr::mutate(type = substr(name, 1, 2),
           minor = as.numeric(substr(name, 3, 5))) %>%
    tidyr::pivot_wider(id_cols = c(gender, race, ageCat, CPCat, pdays, minor),
                names_from = type,
                values_from = value) %>%
    dplyr::mutate(`_e` = `_r`*pdays/365.25) %>%
    dplyr::group_by(minor) %>%
    dplyr::summarize(observed = sum(`_o`, na.rm = TRUE),
              expected = round(sum(`_e`, na.rm = TRUE), 2)) %>%
    dplyr::mutate(smr = observed / expected,
           lower = if_else(observed <= 20,
                           exact_lower[observed+1] /expected,
                           (observed*(1 - (1/(9*observed)) - (1.96/(3*sqrt(observed))))**3)  /expected),
           upper = if_else(observed <= 20,
                           exact_upper[observed+1] /expected,
                           ((observed+1)*(1 - (1/(9*(observed+1))) + (1.96/(3*sqrt((observed+1)))))**3)/expected)) %>%
    dplyr::left_join(rateobj$MinorDesc, by=c('minor')) %>%
    dplyr::select(minor, Description, observed, expected, smr, lower, upper)
}
