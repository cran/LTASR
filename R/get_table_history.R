
#' Stratify Person Table with Time Varying Co-variate
#'
#' `get_table_history` reads in a data.frame/tibble containing basic demographic information
#' for each person of the cohort as well as a data.frame/tibble containing time varying exposure
#' information and stratifies the person-time and deaths into 5-year age, 5-year calendar period,
#' race, sex and exposure categories. See `Details` for information on how the person file
#' and history file must be formatted.
#'
#' The person file must contain the variables:
#' * id,
#' * gender (numeric: 'M'/'F'),
#' * race (numeric: 'W'/'N'),
#' * dob (date),
#' * pybegin (date),
#' * dlo	(date),
#' * rev (numeric: 5-10),
#' * code (character: ICD code)
#'
#' Additionally, a history file is read in containing daily exposures levels and the begin and
#' end dates of that level. The history file must contain the following variables:
#' * id,
#' * begin_dt (date),
#' * end_dt (date),
#' * <daily exposure levels>
#'
#' @param persondf data.frame like object containing one row per person with the required demographic information
#' @param rateobj a rate object created by the `parseRate` function
#' @param batch_size a number specifying how many persons to stratify at a time. Default is 25.
#' @param strata any additional variables contained in `persondf` on which to stratify.
#' Must be wrapped in a `vars()` call from `dplyr`.
#' @param historydf data.frame like object containing temporal exposure data. Each row represents a period
#' of time during which the values f the temporal stratifiers remain constant. Multiple rows
#' per `id` are typical.
#' @param exp the name of the exposure variable found within `historydf`
#' @param cutpt a numeric vector containing the cut-points to use to stratify the
#' calculated cumulative exposure for variable `exp`. Should include min and max values (typically -Inf and Inf).
#' @param lag An optional numeric variable indicating a lag, in years,
#' to be applied to exposure variables. Specified in years. Default is 0 yrs (un-lagged).
#'
#' @return A data.frame with a row for each strata containing the number of observed
#' deaths within each of the defined minors/outcomes (`_o1`-`_oxxx`) and the number of person days.
#' @export
#'
#' @examples
#' library(LTASR)
#' library(dplyr)
#' library(lubridate)
#'
#' #Import example person file
#' person <- person_example %>%
#'   mutate(dob = as.Date(dob, format='%m/%d/%Y'),
#'          pybegin = as.Date(pybegin, format='%m/%d/%Y'),
#'          dlo = as.Date(dlo, format='%m/%d/%Y'))
#'
#' #Import example history file
#' history <- history_example %>%
#'   mutate(begin_dt = as.Date(begin_dt, format='%m/%d/%Y'),
#'          end_dt = as.Date(end_dt, format='%m/%d/%Y'))
#'
#' #Import default rate object
#' rateobj <- us_119ucod_19602020
#'
#' #Stratify cohort employed variable. The employed variable indicates (0/1) periods of employment
#' #and will be summed each day of each exposure period. Therefore, this calculates
#' #duration of employment in days. The cut-points used below will stratify
#' #by person-time with less than and greater than a year of employment (365 days of employment).
#' py_table <- get_table_history(persondf = person,
#'                               rateobj = rateobj,
#'                               historydf = history,
#'                               exp = employed,
#'                               cutpt = c(-Inf, 365, Inf))
#'
#' #Investigate cumulative exposure values for the exposure_level variable.
#' #This aids in determining appropriate cut-points for stratification.
#' history_example %>%
#'   mutate(begin_dt = as.Date(begin_dt, format='%m/%d/%Y'),
#'          end_dt = as.Date(end_dt, format='%m/%d/%Y')) %>%
#'   mutate(dur = difftime(end_dt, begin_dt, units = 'days') %>%
#'            as.numeric() %>%
#'            `+`(1),
#'          cum = dur*exposure_level) %>%
#'   group_by(id) %>%
#'   summarize(cum = sum(cum))
#'
#' #Stratify cohort with exposure variable. This will stratify by person-time with
#' #with less than and greater than 15000 unit-days of cumulative exposure.
#' py_table <- get_table_history(persondf = person,
#'                               rateobj = rateobj,
#'                               historydf = history,
#'                               exp = exposure_level,
#'                               cutpt = c(-Inf, 15000, Inf),
#'                               lag = 10)
#'
get_table_history <- function(persondf, rateobj, batch_size = 25, strata=dplyr::vars(),
                      historydf, exp, cutpt, lag=0){
  exp_var <- rlang::enquo(exp)
  if (length(strata) == 0) ..names <- c() else ..names <- purrr::map_chr(strata, rlang::as_name)

  #Format Person
  checkPerson(persondf)
  person_all <- persondf %>%
    dplyr::filter(.data$dlo >= .data$pybegin) #Drop those who died before study begin date
  drop <- nrow(persondf %>%
                 dplyr::filter(.data$dlo < .data$pybegin))
  if (drop > 0){message('- Dropping ', drop, 'persons with dlo < pybegin')}

  pytot <- person_all %>%
    dplyr::mutate(py = .data$dlo - .data$pybegin + 1) %>%
    dplyr::summarize(py = sum(.data$py))
  pytot <- as.numeric(pytot[[1]])

  #Format History
  historydf <- historydf %>%
    dplyr::arrange(.data$id, .data$begin_dt)
  ###$$$$CHECK HISTORY FILE FOR DATES, OVERLAPS, ????####

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
    history <- historydf %>%
      dplyr::filter(dplyr::between(dplyr::row_number(),(it-1)*batch_size+1,it*batch_size)) %>%
      dplyr::filter(!is.na(.data$id))
    #################################

    # Get long person file
    person_long <- person %>%
      dplyr::group_by(.data$id) %>%
      expand_dates(.data$pybegin, .data$dlo) %>%
      dplyr::mutate(age = floor(as.numeric(.data$date - .data$dob + 1)/365.24),
             year = lubridate::year(.data$date),
             ageCat = cut(.data$age, c(-Inf, 3:17 * 5, Inf), right=FALSE), #5-year age
             CPCat = cut(.data$year, c(-Inf, 380:600 * 5), right=FALSE)) #5-year CP

    # Get long history file
    history_long <- history %>%
      expand_dates(.data$begin_dt, .data$end_dt) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(exp = as.double(cumsum(!!exp_var)))

    # Merge History with Person
    person_long <- person_long %>%
      dplyr::left_join(history_long %>%
                         dplyr::mutate(date=date+lubridate::days(floor(lag*365.25))) %>%
                         dplyr::select(.data$id, .data$date, .data$exp), by=c('id', 'date')) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(exp = zoo::na.locf(.data$exp, na.rm = FALSE),
                    exp = dplyr::if_else(is.na(.data$exp), 0, .data$exp)) %>%
      dplyr::mutate(expCat = cut(.data$exp, cutpt))


    # Collapse into categories
    options(dplyr.summarise.inform = FALSE)
    py_days <- person_long %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, .data$expCat, !!!strata) %>%
      dplyr::summarize(pdays = dplyr::n())
    py_min <- person_long %>%
      dplyr::filter(.data$dlo == .data$date) %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, .data$expCat, !!!strata, .data$minor) %>%
      dplyr::filter(!is.na(.data$minor)) %>%
      dplyr::summarize(obs = dplyr::n()) %>%
      tidyr::pivot_wider(names_from = .data$minor, names_prefix = '_o', values_from = .data$obs)
    py_table <- dplyr::full_join(py_days, py_min, by=c('ageCat', 'CPCat', 'gender', 'race', 'expCat', ..names)) %>%
      dplyr::bind_rows(py_table)
    py_table <- py_table %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, .data$expCat, !!!strata) %>%
      dplyr::summarise_at(.vars=dplyr::vars(.data$pdays, dplyr::starts_with('_o')), .funs = sum, na.rm=T)
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
  close(pb)


  # Unfactor ageCat and CPCat
  py_table <- py_table %>%
    dplyr::mutate(ageCat = as.character(.data$ageCat),
                  CPCat= as.character(.data$CPCat),
                  expCat= as.character(.data$expCat))


  # Check all categories are within rate file
  py_table <- checkStrata(py_table, rateobj)[[1]]

  ct <- py_table %>%
    dplyr::ungroup() %>%
    dplyr::mutate(obs = rowSums(dplyr::select(., dplyr::starts_with('_o')))) %>%
    dplyr::summarize(obs = sum(.data$obs),
              py = sum(.data$pdays)/365.25)
  message("- Person Table successfully created\n ",
          round(ct$py,0), ' person-years and ', ct$obs,
          " deaths in final table")

  return(py_table)
}
