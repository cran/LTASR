#' Map ICD codes to grouped minors
#'
#' @param persondf Person data.frame
#' @param rateobj A rate object created from `parseRate`
#'
#' @return A data.frame for each death observed in the person file with the following variables:
#' id, code, rev: from the `persondf`
#' minor: the minor/outcome from the rate file that the death was mapped to
#'
#' @export
#'
#' @examples
#' library(LTASR)
#'
#' #Import example person file
#' person <- person_example
#'
#' #Import default rate object
#' rateobj <- us_119ucod_19602020
#'
#' #Check mapping of deaths to minors/outcomes
#' mapDeaths(person, rateobj)
#'
mapDeaths <- function(persondf, rateobj){
  #filter the deaths as flagged by 'vs'
  #remove decimal point from codes
  persondf <- persondf %>%
    dplyr::filter(vs=='D') %>%
    dplyr::mutate(code = stringr::str_remove_all(code, '\\.'))
  mapping <- rateobj$mapping %>%
    dplyr::mutate(code = stringr::str_remove_all(code, '\\.'))
  deaths_minors <- persondf %>%
    dplyr::mutate(rev = stringr::str_pad(rev, 2, pad='0')) %>%
    dplyr::left_join(mapping, by=c('rev', 'code')) %>%
    dplyr::mutate(minor = dplyr::if_else(is.na(minor), rateobj$residual, minor)) %>%
    dplyr::select(id, code, rev, minor)
  recode <- nrow(persondf %>%
                   dplyr::mutate(rev = stringr::str_pad(rev, 2, pad='0')) %>%
                   dplyr::left_join(mapping, by=c('rev', 'code')) %>%
                   dplyr::filter(is.na(minor)))
  if (recode > 0){message("- ", recode, ' unknown causes of death mapped to residual minor ' , rateobj$residual,
                          '\n  Use mapDeaths() function to output which minor/outcome
  each death is mapped to in your cohort')}
  return(deaths_minors)
}
