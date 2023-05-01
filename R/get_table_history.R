
#' Stratify Person Table with Time Varying Co-variate
#'
#' `get_table_history` reads in a data.frame/tibble (`persondf`) containing basic demographic information for
#' each person of the cohort as well as a data.frame/tibble (`historydf`) containing time varying exposure
#' information and stratifies the person-time and deaths into 5-year age, 5-year calendar period, race, sex and
#' exposure categories. See `Details` for information on how the person file and history file must be
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
#' @param batch_size a number specifying how many persons to stratify at a time. Default is 500.
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
#'
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
#' py_table <- get_table_history(persondf = person,
#'                               rateobj = rateobj,
#'                               historydf = history,
#'                               exps = list(exp1))
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
#' py_table <- get_table_history(persondf = person,
#'                               rateobj = rateobj,
#'                               historydf = history,
#'                               exps = list(exp1, exp2))
#'
#' @import rlang

get_table_history <- function(persondf,
                              rateobj,
                              historydf,
                              exps=list(),
                              strata=dplyr::vars(),
                              batch_size = 500){

  find_cuts <- function(bd, bcum, ec, x, cutpts){
    dts <- as.Date(c())
    bc <- cut(bcum, cutpts) %>% as.numeric()
    if (x != 0){
      for (cat in (bc+1) : ec){
        n <- ((cutpts[cat] - bcum + x) / x + bd - 1)
        if (n == bd - 1) n <- bd
        dts <- c(dts, n)
      }
    }
    return(unique(dts))
  }
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

  exp_var <- dplyr::vars()
  for (..e in exps){
    new_var <- paste0(..e$var, 'Cat')
    exp_var <- append(exp_var, dplyr::vars(!!rlang::sym(..e$var)))
    strata <- append(strata, dplyr::vars(!!rlang::sym(new_var)))
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
    dplyr::summarize(py = sum(.data$py))
  pytot <- as.numeric(pytot[[1]])

  #Format History
  historydf <- historydf %>%
    dplyr::arrange(.data$id, .data$begin_dt)%>%
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
    dplyr::select('id', 'minor')
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
      dplyr::filter(id %in% person$id) %>%
      dplyr::filter(!is.na(.data$id))
    #################################

    # Get long person file
    person_long <- person %>%
      dplyr::mutate(date = purrr::pmap(list(.data$dob, .data$pybegin, .data$dlo), ~get_dates(..1, ..2, ..3))) %>%
      tidyr::unnest(date)


    # Get long history file
    hiss <- list()
    for (..e in exps){
      his <- history %>%
        dplyr::select(.data$id, .data$begin_dt, .data$end_dt, ..e$var) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(dur = difftime(.data$end_dt, .data$begin_dt, units = 'days') %>% as.numeric() + 1,
               endcum = !!rlang::sym(..e$var)*.data$dur,
               endcum = cumsum(.data$endcum),
               begcum = stats::lag(.data$endcum) + !!rlang::sym(..e$var),
               begcum = dplyr::if_else(dplyr::row_number() == 1, !!rlang::sym(..e$var), .data$begcum),
               begcat = cut(.data$begcum, ..e$cutpt) %>% as.numeric(),
               endcat = cut(.data$endcum, ..e$cutpt) %>% as.numeric())

      ##########################################################################
      # Check for valid cumulative exposures
      missing_cat <- his %>%
        ungroup() %>%
        filter(is.na(.data$begcat) | is.na(.data$endcat))

      if (nrow(missing_cat) != 0){
        a <- '\nThere is person-time with exposure values
that fall outside of the user-supplied cutpoints.\n
Below is an example:'
        b <- missing_cat %>%
          dplyr::ungroup() %>%
          dplyr::filter(dplyr::row_number() == 1) %>%
          dplyr::select('id', 'begin_dt', 'end_dt', ..e$var) %>%
          knitr::kable() %>%
          paste0(collapse='\n')
        c <- missing_cat %>%
          dplyr::ungroup() %>%
          dplyr::filter(row_number() == 1) %>%
          dplyr::select(`Cumulative Exposure at Beginning of Period` = 'begcum',
                 `Cumulative Exposure at End of Period` = 'endcum') %>%
          knitr::kable() %>%
          paste0(collapse='\n')
        d <- 'These values fall outside:'
        e <- (paste0(..e$cutpt, collapse=', '))
        f <- 'Are the cutpoints padded by -Inf and Inf?'

        stop(paste(a, b, '', c, '', d, e, f, sep='\n'))
      }
      ##########################################################################

      his <- his %>%
        dplyr::mutate(d = purrr::pmap(list(.data$begin_dt, .data$begcum, .data$endcat, !!rlang::sym(..e$var)), ~find_cuts(..1, ..2, ..3, ..4, ..e$cutpt)),
               d = if_else(.data$begcat == .data$endcat, list(c()), .data$d),
               begin_dt = purrr::map2(.data$begin_dt, .data$d, ~ c(.x, .y+1) + ..e$lag*365.25),
               end_dt = purrr::map2(.data$end_dt, .data$d, ~ c(as.Date(.y), .x) + ..e$lag*365.25)) %>%
        dplyr::select(-.data$dur, -.data$endcum, -.data$begcum, -.data$endcat, -.data$begcat, -.data$d) %>%
        tidyr::unnest(c(.data$begin_dt, .data$end_dt)) %>%
        dplyr::mutate(dur = difftime(.data$end_dt, .data$begin_dt, units = 'days') %>% as.numeric() + 1,
               endcum = !!rlang::sym(..e$var)*.data$dur,
               endcum = cumsum(.data$endcum),
               endcat = cut(.data$endcum, ..e$cutpt)) %>%
        dplyr::select(id, date = .data$begin_dt, !!rlang::sym(paste0(..e$var, 'Cat')) := .data$endcat)
      hiss <- append(hiss, list(his))
    }

    hiss <- hiss %>%
      purrr::reduce(dplyr::full_join, by=c('id', 'date')) %>%
      dplyr::arrange(id, .data$date)

    # Merge History with Person
    person_long <- dplyr::full_join(person_long, hiss, by=c('id', 'date')) %>%
      dplyr::arrange(id, .data$date) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate_at(c(..names,
                         'gender', 'race', 'dob', 'pybegin', 'dlo'), ~ zoo::na.locf(., na.rm = FALSE)) %>%
      dplyr::filter(.data$pybegin <= .data$date,
                    .data$date <= .data$dlo)
    for (.x in exps) {
      vcat <- paste0(.x$var, 'Cat')
      person_long <- person_long %>%
        dplyr:: mutate(!!rlang::sym(vcat) := if_else(is.na(!!rlang::sym(vcat)), cut(0, .x$cutpt), !!rlang::sym(vcat)))
    }
    person_long <- person_long %>%
      dplyr::mutate(age = floor(as.numeric(.data$date - .data$dob + 1)/365.24),
                    year = lubridate::year(.data$date),
                    ageCat = cut(.data$age, c(-Inf, 3:17 * 5, Inf), right=FALSE), #5-year age
                    CPCat = cut(.data$year, c(-Inf, 380:600 * 5), right=FALSE)) %>%  #5-year CP
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(pdays = difftime(dplyr::lead(.data$date), .data$date, units = 'days') %>% as.numeric(),
                    pdays = dplyr::if_else(dplyr::row_number() == dplyr::n(), 1L, as.integer(.data$pdays)))


    # Collapse into categories
    py_days <- person_long %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata) %>%
      dplyr::summarize(pdays = sum(pdays))
    # py_exp <- person_long %>%
    #   dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata) %>%
    #   dplyr::summarize_at(exp_var, sum)
    py_min <- person_long %>%
      dplyr::filter(.data$dlo == .data$date) %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata, .data$minor) %>%
      dplyr::filter(!is.na(.data$minor)) %>%
      dplyr::summarize(obs = dplyr::n()) %>%
      tidyr::pivot_wider(names_from = .data$minor, names_prefix = '_o', values_from = .data$obs)
    py_table <- dplyr::full_join(py_days, py_min, by=c('ageCat', 'CPCat', 'gender', 'race', ..names)) %>%
      #dplyr::full_join(py_exp,by=c('ageCat', 'CPCat', 'gender', 'race', ..names)) %>%
      dplyr::bind_rows(py_table)
    py_table <- py_table %>%
      dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race, !!!strata) %>%
      dplyr::summarise_at(.vars=dplyr::vars(.data$pdays, dplyr::starts_with('_o')), .funs = sum, na.rm=T) #, map_chr(exps, ~.$var)
    rm(person_long, person, hiss, history)

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
  # py_table <- py_table %>%
  #   mutate_at(map_chr(exps, ~.$var), ~./.data$pdays)

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
  message("- Person Table successfully created\n ",
          round(ct$py,0), ' person-years and ', ct$obs,
          " deaths in final table")

  return(py_table)
}
