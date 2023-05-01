#' Calculate SMRs for Custom minor groupings
#'
#' `smr_major` will collapse minor outcomes into "major" groupings as defined in the
#' rate object, `rateobj`.
#'
#' @param smr_minor_table A data.frame/tibble as created by `smr_minor` containing
#' observed and expected number of deaths for each minor outcome
#' @param minor_grouping A numeric vector defining which minors to group together
#'
#' @return A data.frame/tibble containing the expected and observed number of deaths
#' as well the SMR, lower CI and upper CI for the outcome by the user
#'
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
#' #Calculate custom minor grouping for all deaths
#' smr_custom(smr_minor_table, 1:119)
#'
#' #' #Calculate custom minor grouping for all deaths
#' smr_custom(smr_minor_table, 4:40)
#'
smr_custom <- function(smr_minor_table, minor_grouping){
  smr_minor_table %>%
    dplyr::filter(minor %in% minor_grouping) %>%
    dplyr::summarize(observed = sum(observed, na.rm = TRUE),
              expected = round(sum(expected, na.rm = TRUE), 2)) %>%
    dplyr::mutate(smr = observed / expected,
           lower = (observed*(1 - (1/(9*observed)) - (1.96/(3*sqrt(observed))))**3)  /expected,
           upper = ((observed+1)*(1 - (1/(9*(observed+1))) + (1.96/(3*sqrt((observed+1)))))**3)/expected)

}
