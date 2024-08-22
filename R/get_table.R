
#' Stratify Person Table
#'
#' `get_table` reads in a data.frame/tibble containing basic demographic information
#' for each person of the cohort and stratifies the person-time and deaths into 5-year age,
#' 5-year calendar period, race, and sex strata. See `Details` for information on how the
#' person file must be formatted.
#'
#' The persondf tibble must contain the variables:
#' * id,
#' * gender (character: 'M'/'F'),
#' * race (character: 'W'/'N'),
#' * dob (date),
#' * pybegin (date),
#' * dlo	(date),
#' * vs (character: indicator identifying deaths as 'D')
#' * rev (numeric: values 5-10),
#' * code (character: ICD code)
#'
#' @param persondf data.frame like object containing one row per person with the required demographic information
#' @param rateobj a rate object created by the `parseRate` function, or the included rate object `us_119ucod_19602021`
#' @param strata any additional variables contained in persondf on which to stratify.
#' Must be wrapped in a `vars()` call from `dplyr`.
#' @param batch_size a number specifying how many persons to stratify at a time. Default is 500
#'
#' @return A data.frame with a row for each strata containing the number of observed
#' deaths within each of the defined minors/outcomes (`_o1`-`_oxxx`) and the number of person days.
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
#' @importFrom rlang .data
get_table <- function(persondf,
                      rateobj,
                      strata=dplyr::vars(),
                      batch_size = 500){

  get_dates <- function(dob, pybegin, dlo) {
    full <- sort(c(
      pybegin,
      dob + ceiling(rateobj$age_cut * 365.24 - 1),
      lubridate::mdy(paste0('1/1/', rateobj$cp_cut)),
      dlo
    )) %>%
      unique()
    return(full[dplyr::between(full, pybegin, dlo)])
  }

  if (length(strata) == 0) ..names <- c() else ..names <- purrr::map_chr(strata, rlang::as_name)

  persondf <- persondf %>%
    dplyr::ungroup() %>%           #Ungroup and grouping variables
    dplyr::select('id',
                  'gender',
                  'race',
                  'vs',
                  'dob',
                  'pybegin',
                  'dlo',
                  'rev',
                  'code',
                  !!!strata) #Keep only necessary variables

  checkPerson(persondf, rateobj)
  person_all <- persondf %>%
    dplyr::filter(.data$dlo >= .data$pybegin) #Drop those who died before study begin date
  drop <- nrow(persondf %>%
                 dplyr::filter(.data$dlo < .data$pybegin))
  if (drop > 0){message('- Dropping ', drop, 'persons with dlo < pybegin')}

  pytot <- person_all %>%
    dplyr::mutate(py = .data$dlo - .data$pybegin + 1) %>%
    dplyr::summarize(py = sum(.data$py))
  pytot <- as.numeric(pytot[[1]])


  #Map Outcomes
  deaths_minors <- mapDeaths(person_all, rateobj) %>%
    dplyr::select(.data$id, .data$minor)
  person_all <- dplyr::left_join(person_all, deaths_minors, by='id')

  #Loop through people
  loops <- ceiling(nrow(person_all)/batch_size)
  py_table <- dplyr::tibble()

  #Set-up progress bar
  pb <- utils::txtProgressBar(min = 0, max = 70, width=70, style = 2)
  init <- numeric(loops)
  end <- numeric(loops)

  for (it in 1:loops){
    init[it] <- Sys.time()

    #################################
    # restrict to batch
    person <- person_all %>%
      dplyr::filter(dplyr::between(dplyr::row_number(),(it-1)*batch_size+1,it*batch_size)) %>%
      dplyr::filter(!is.na(.data$id))
    #################################

    # Get long person file
    person_long <- person %>%
      dplyr::mutate(date = purrr::pmap(list(.data$dob, .data$pybegin, .data$dlo), ~get_dates(..1, ..2, ..3))) %>%
      tidyr::unnest(date) %>%
      dplyr::mutate(age = floor(as.numeric(.data$date - .data$dob + 1)/365.24),
                    year = lubridate::year(.data$date),
                    ageCat = cut(.data$age, c(-Inf, rateobj$age_cut), right=FALSE), #5-year age
                    CPCat = cut(.data$year, c(-Inf, rateobj$cp_cut), right=FALSE)) %>%  #5-year CP
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(pdays = difftime(dplyr::lead(.data$date), .data$date, units = 'days') %>% as.numeric(),
                    pdays = dplyr::if_else(dplyr::row_number() == dplyr::n(), 1L, as.integer(.data$pdays)))


    # Collapse into categories
    py_days <- person_long %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata) %>%
      dplyr::summarize(pdays = sum(pdays))
    py_min <- person_long %>%
      dplyr::filter(.data$dlo == .data$date) %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata, .data$minor) %>%
      dplyr::filter(!is.na(.data$minor)) %>%
      dplyr::summarize(obs = dplyr::n()) %>%
      tidyr::pivot_wider(names_from = .data$minor, names_prefix = '_o', values_from = .data$obs)
    py_table <- dplyr::full_join(py_days, py_min, by=c('ageCat', 'CPCat', 'gender', 'race', ..names)) %>%
      dplyr::bind_rows(py_table)
    rm(person_long, person)

    #Update Progress Bar
    end[it] <- Sys.time()

    utils::setTxtProgressBar(pb, floor(70*it/loops))
    time <- round(lubridate::seconds_to_period(sum(end - init)), 0)

    est <- loops * (mean(end[end != 0] - init[init != 0])) - time
    remainining <- round(lubridate::seconds_to_period(est), 0)

    cat(paste(" ", floor(100*it/loops), "% // Execution time:", time,
              " // Estimated time remaining:", remainining), "")

  }
  py_table <- py_table %>%
    dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata) %>%
    dplyr::summarise_at(.vars=dplyr::vars(.data$pdays, dplyr::starts_with('_o')), .funs = sum, na.rm=T)
  close(pb)


  # Unfactor ageCat and CPCat
  py_table <- py_table %>%
    dplyr::mutate(ageCat = as.character(.data$ageCat),
           CPCat= as.character(.data$CPCat))

  # Check all categories are within rate file
  py_table <- checkStrata(py_table, rateobj)[[1]]

  ct <- py_table %>%
    dplyr::ungroup() %>%
    dplyr::mutate(obs = rowSums(dplyr::select(., dplyr::starts_with('_o')))) %>%
    dplyr::summarize(obs = sum(.data$obs),
              py = sum(.data$pdays)/365.25)
  message("- Person Table successfully created\n  ",
          round(ct$py,0), ' person-years and ', ct$obs,
          " deaths in final table")

  return(py_table)
}
