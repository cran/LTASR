

#' Check person dataframe
#'
#' `checkPerson` checks that the person dataframe is formatted correctly
#'
#' @param person person dataframe
#' @param rateobj rateobject
#'
#' @return No return value, called for side effects
#'
#' @noRd
#'
#' @importFrom rlang .data
checkPerson <- function(person, rateobj){
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
  # Check if dob, pybegin and dlo are date values
  if (nrow(person) != length(unique(person$id))){
    stop("Person file must contain one row per person.
       There are multiple rows with the same id value in your person file.")
  }

  ###################################################
  # Check for missing values
  na_num <- person %>%
    dplyr::select('id', 'gender', 'race',
           'dob', 'pybegin', 'dlo') %>%
    dplyr::summarize(dplyr::across(dplyr::everything(),
                     ~ sum(is.na(.)))) %>%
    tidyr::pivot_longer(everything())

  if (sum(na_num$value) != 0){
    nn <- na_num %>%
      dplyr::filter(value > 0) %>%
      `$`(name) %>%
      paste0(collapse=', ')
    stop('The following variables:\n       ',
         nn,
         '\n       contains missing values.')
  }

  ###################################################
  # Are dates in proper order (dob <= pybegin <= dlo)
  dtorder <- person %>%
    dplyr::mutate(dob_gt_pybegin = (.data$dob > .data$pybegin),
           pybegin_gt_dlo = (.data$pybegin > .data$dlo)) %>%
    dplyr::summarize(dob_gt_pybegin = sum(.data$dob_gt_pybegin, na.rm = T),
              pybegin_gt_dlo = sum(.data$pybegin_gt_dlo, na.rm = T))
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
  missing_gender <- setdiff(person$gender, rateobj$rates$gender)
  if (length(missing_gender) > 0){
    message('- The person file contains the following gender codes:')
    cat('  ', missing_gender)
    message('\n  which are not in the rate file. These persons will be dropped.')
  }
  missing_race <- setdiff(person$race, rateobj$rates$race)
  if (length(missing_race) > 0){
    message('- The person file contains the following race codes:')
    cat('  ', missing_race)
    message('\n  which are not in the rate file. These persons will be dropped.')
  }

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


