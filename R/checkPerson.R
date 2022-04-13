

#' Check person dataframe
#'
#' `checkPerson` checks that the person dataframe is formatted correctly
#'
#' @param person person dataframe
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' library(LTASR)
#' library(dplyr)
#'
#' #Import example peron file
#' person <- person_example %>%
#'   mutate(dob = as.Date(dob, format='%m/%d/%Y'),
#'          pybegin = as.Date(pybegin, format='%m/%d/%Y'),
#'          dlo = as.Date(dlo, format='%m/%d/%Y'))
#'
#' #Returns nothing
#' checkPerson(person)
#'
#' @importFrom rlang .data
checkPerson <- function(person){
  ###################################################
  # Check for necessary variables
  number <- c('id', 'gender', 'race',
    'dob', 'pybegin', 'dlo',
    'vs', 'rev', 'code') %in% colnames(person) %>%
    sum()
  if (number != 9) stop("Missing necessary variables. Person file must include:
       id, gender, race, dob, pybegin, dlo, vs, rev, code,
       Note: variable names are CASE-SENSITIVE")

  ###################################################
  # Check if dob, pybegin and dlo are date values
  if (!(lubridate::is.Date(person$dob) &
        lubridate::is.Date(person$pybegin) &
        lubridate::is.Date(person$dlo))) stop("Either dob, pybegin or dlo is not a date value")

  ###################################################
  # Are dates in proper order (dob <= pybegin <= dlo)
  dtorder <- person %>%
    dplyr::mutate(dob_gt_pybegin = (.data$dob > .data$pybegin),
           pybegin_gt_dlo = (.data$pybegin > .data$dlo)) %>%
    dplyr::summarize(dob_gt_pybegin = sum(.data$dob_gt_pybegin),
              pybegin_gt_dlo = sum(.data$pybegin_gt_dlo))
  if (dtorder$dob_gt_pybegin != 0) stop('At least one person has a date of birth (dob)
       later than their person time begin date (pybegin)')
  if (dtorder$pybegin_gt_dlo != 0) warning('- At least one person has a person time begin date (pybegin)
        later than their date last observed (dlo)
        these people will be removed')

  ###################################################
  # Check formatting of gender and race
  # Ensure they match the formatting of the rate file
  # Gender: 1 = male
  #         2 = female
  # Race:   1 = white
  #         2 = non-white
  # Ensure no NAs
  message('- Below are the gender and race distributions of your person file:')
  person %>%
    dplyr::count(.data$gender) %>%
    knitr::kable() %>%
    print()
  person %>%
    dplyr::count(.data$race) %>%
    knitr::kable()%>%
    print()

  ###################################################
  # Check rev and code variables
  # A missing value indicates a censored person:
  #      either alive at study end date or
  #      lost-to-followup
  # Count number of non-missing values (deaths)
  # Also ensure rev and code are both non-missing
  rev_chk <- person %>%
    dplyr::mutate(code_and_rev = (is.na(.data$code) & !is.na(.data$rev)) |
             (!is.na(.data$code) & is.na(.data$rev))) %>%
    dplyr::summarize(code_and_rev = sum(.data$code_and_rev))
  if (rev_chk$code_and_rev != 0) warning('- Persons in your file have a missing code but non-missing rev (or vice-versa)
       these persons will be mapped to the residual minor
       use the function mapDeaths() to output a list of the minors by outcome')
}


