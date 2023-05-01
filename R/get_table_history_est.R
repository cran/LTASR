#' Stratify Person Table with Time Varying Co-variate
#'
#' `get_table_history_est` reads in a data.frame/tibble (`persondf`) containing basic demographic information for
#' each person of the cohort as well as a data.frame/tibble (`historydf`) containing time varying exposure
#' information and stratifies the person-time and deaths into 5-year age, 5-year calendar period, race, sex and
#' exposure categories. Additionally, average cumulative exposure values for each strata and each exposure
#' variable are included. These strata are more crudely calculated by taking regular steps (such as every 7 days)
#' as opposed to evaluating every individual day. See `Details` for information on how the person file and history file must be
#' formatted.
#'
#' The persondf tibble must contain the variables:
#' * id,
#' * gender (character: 'M'/'F'),
#' * race (character: 'W'/'N'),
#' * dob (date),
#' * pybegin (date),
#' * dlo	(date),
#' * rev (numeric: values 5-10),
#' * code (character: ICD code)
#'
#' The historydf tibble must contain the variables:
#' * id,
#' * begin_dt (date),
#' * end_dt (date),
#' * *&lt;daily exposure levels&gt;*
#'
#' @param persondf data.frame like object containing one row per person with the required demographic information.
#' @param rateobj a rate object created by the `parseRate` function, or the included rate object `us_119ucod_19602021`.
#' @param historydf data.frame like object containing one row per person and exposure period. An exposure period is a
#' period of time where exposure levels remain constant. See `Details` for required variables.
#' @param exps a list containing exp_strata objects created by `exp_strata()`.
#' @param strata any additional variables contained in persondf on which to stratify.
#' Must be wrapped in a `vars()` call from `dplyr`.
#' @param step numeric defining number of days to jump when calculating cumulative exposure values. Exact stratification
#' specifies a step of 1 day.
#' @param batch_size a number specifying how many persons to stratify at a time.
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
#' mutate(dob = as.Date(dob, format='%m/%d/%Y'),
#'          pybegin = as.Date(pybegin, format='%m/%d/%Y'),
#'          dlo = as.Date(dlo, format='%m/%d/%Y'))
#'
#' #Import example history file
#' history <- history_example %>%
#'   mutate(begin_dt = as.Date(begin_dt, format='%m/%d/%Y'),
#'          end_dt = as.Date(end_dt, format='%m/%d/%Y'))
#'
#' #Import default rate object
#' rateobj <- us_119ucod_19602021

#' #Define exposure of interest. Create exp_strata object.The `employed` variable
#' #indicates (0/1) periods of employment and will be summed each day of each exposure
#' #period. Therefore, this calculates duration of employment in days. The cut-points
#' #used below will stratify by person-time with less than and greater than a
#' #year of employment (365 days of employment).
#' exp1 <- exp_strata(var = 'employed',
#'                    cutpt = c(-Inf, 365, Inf),
#'                    lag = 0)
#'
#' #Stratify cohort by employed variable.
#' py_table <- get_table_history_est(persondf = person,
#'                                   rateobj = rateobj,
#'                                   historydf = history,
#'                                   exps = list(exp1))
#'
#' #Multiple exposures can be considered.
#' exp1 <- exp_strata(var = 'employed',
#'                    cutpt = c(-Inf, 365, Inf),
#'                    lag = 0)
#' exp2 <- exp_strata(var = 'exposure_level',
#'                    cutpt = c(-Inf, 0, 10000, 20000, Inf),
#'                    lag = 10)
#'
#' #Stratify cohort by employed variable.
#' py_table <- get_table_history_est(persondf = person,
#'                                   rateobj = rateobj,
#'                                   historydf = history,
#'                                   exps = list(exp1, exp2))
#'
#' @import rlang
#' @import dplyr
#'
get_table_history_est <- function(persondf,
                                  rateobj,
                                  historydf,
                                  exps,
                                  strata = dplyr::vars(),
                                  step = 7,
                                  batch_size = 25 * step) {

  persondf <- persondf %>%
    ungroup() %>%           #Ungroup and grouping variables
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

  exp_var <- vars()
  for (..e in exps){
    new_var <- paste0(..e$var, 'Cat')
    exp_var <- append(exp_var, vars(!!sym(..e$var)))
    strata <- append(strata, vars(!!sym(new_var)))
  }
  if (length(strata) == 0) ..names <- c() else ..names <- purrr::map_chr(strata, rlang::as_name)


  #Format Person
  checkPerson(persondf, rateobj)
  person_all <- persondf %>%
    dplyr::filter(.data$dlo >= .data$pybegin)  #Drop those who died before study begin date

  drop <- nrow(persondf %>%
                 dplyr::filter(.data$dlo < .data$pybegin))
  if (drop > 0){message('- Dropping ', drop, 'persons with dlo < pybegin')}

  pytot <- person_all %>%
    dplyr::mutate(py = .data$dlo - .data$pybegin + 1) %>%
    dplyr::summarize(py = sum(.data$py),
                     .groups='drop')
  pytot <- as.numeric(pytot[[1]])

  #Format History
  historydf <- historydf %>%
    dplyr::arrange(.data$id, .data$begin_dt) %>%
    dplyr::ungroup()

  checkHistory(historydf, exp_var)

  # Check all persons have a history entry
  missing_history <- setdiff(person_all$id, historydf$id)
  if (length(missing_history) > 0){
    message('- Dropping ', length(missing_history), ' persons not found in history file')
    person_all <- person_all %>%
      dplyr::filter(!id %in% missing_history)
  }

  #Map Outcomes
  deaths_minors <- mapDeaths(person_all, rateobj) %>%
    dplyr::select(.data$id, .data$minor)
  person_all <- dplyr::left_join(person_all, deaths_minors, by='id')

  #Set up mapping dates
  stop_pts_n <- floor(365 / step)
  md_tmplt <- c(as.Date('12/31/2014', format='%m/%d/%Y') + step * (1:stop_pts_n),
                as.Date('12/31/2015', format='%m/%d/%Y')) %>%
    unique() %>%
    `[`(lubridate::year(.) == 2015)

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
      filter(id %in% person$id) %>%
      dplyr::filter(!is.na(.data$id))
    #################################


    # Get long person file
    person_long <- person %>%
      dplyr::group_by(.data$id) %>%
      expand_dates(.data$pybegin, .data$dlo, md_tmplt) %>%
      dplyr::mutate(age = floor(as.numeric(.data$date - .data$dob + 1)/365.24),
                    year = lubridate::year(.data$date),
                    ageCat = cut(.data$age, c(-Inf, rateobj$age_cut), right=FALSE), #5-year age
                    CPCat = cut(.data$year, c(-Inf, rateobj$cp_cut, Inf), right=FALSE)) %>% #5-year CP
      dplyr::mutate(days = difftime(date, lag(date),
                                    units = 'days') %>%
                      as.numeric(),
                    year = lubridate::year(date)) %>%
      filter(!is.na(.data$period))

    # Get long history file
    history_long <- history %>%
      expand_dates(.data$begin_dt, .data$end_dt, md_tmplt) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(days = difftime(date, lag(date),
                                    units = 'days') %>%
                      as.numeric(),
                    year = lubridate::year(date)) %>%
      dplyr::filter(!is.na(.data$period)) %>%
      dplyr::select(-date) %>%
      dplyr::mutate_at(purrr::map_chr(exps, ~.$var), ~as.double(cumsum(. * .data$days))) %>%
      dplyr::group_by(.data$id, .data$year, .data$period) %>%
      dplyr::filter(row_number() == n()) %>%
      dplyr::group_by(.data$id)


    # Merge History with Person
    for (..e in exps){
      person_long <- person_long %>%
        dplyr::left_join(history_long %>%
                           dplyr::mutate(year=.data$year+..e$lag) %>%
                           dplyr::select(.data$id, .data$period, .data$year, ..e$var), by=c('id', 'year', 'period')) %>%
        dplyr::group_by(.data$id) %>%
        dplyr::mutate(!!sym(..e$var) := zoo::na.locf(!!sym(..e$var), na.rm = FALSE),
                      !!sym(..e$var) := if_else(is.na(!!sym(..e$var)), 0, !!sym(..e$var))) %>%
        dplyr::mutate(!!sym(paste0(..e$var, 'Cat')) := cut(!!sym(..e$var), ..e$cutpt))
    }



    # Collapse into categories
    #options(dplyr.summarise.inform = FALSE)
    py_days <- person_long %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata) %>%
      dplyr::summarize(pdays = sum(.data$days),
                       .groups='drop')
    py_exp <- person_long %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata) %>%
      dplyr::summarize_at(exp_var, ~sum(. * .data$days))
    py_min <- person_long %>%
      dplyr::filter(.data$dlo == .data$date) %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata, .data$minor) %>%
      dplyr::filter(!is.na(.data$minor)) %>%
      dplyr::summarize(obs = dplyr::n(),
                       .groups='drop') %>%
      tidyr::pivot_wider(names_from = .data$minor, names_prefix = '_o', values_from = .data$obs)
    py_table <- dplyr::full_join(py_days, py_min, by=c('ageCat', 'CPCat', 'gender', 'race', ..names)) %>%
      dplyr::full_join(py_exp,by=c('ageCat', 'CPCat', 'gender', 'race', ..names)) %>%
      dplyr::bind_rows(py_table)
    py_table <- py_table %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata) %>%
      dplyr::summarise_at(.vars=dplyr::vars(.data$pdays, dplyr::starts_with('_o'), purrr::map_chr(exps, ~.$var)), .funs = sum, na.rm=T)
    rm(person_long, person, history_long, history)

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

  # Calculate Means of exposures
  py_table <- py_table %>%
    mutate_at(purrr::map_chr(exps, ~.$var), ~./.data$pdays)

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
                     py = sum(.data$pdays)/365.25,
                     .groups='drop')
  message("- Person Table successfully created\n ",
          round(ct$py,0), ' person-years and ', ct$obs,
          " deaths in final table")

  return(py_table)
}
