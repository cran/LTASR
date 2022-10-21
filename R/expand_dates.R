#' Expand data through range of date values
#'
#' Expand a data.frame to include all dates between a start and end value
#' defined by parameters x and y
#'
#' @param df Input data.frame
#' @param start start date
#' @param end end date
#' @param md_tmplt Date vector that defines which dates within a year to output.
#'
#' @return A data.frame/tibble containing all variables of the input data.frame
#' as well as a new variable, `date`, with repeated rows for each date between
#' `start` and `end` spaced as defined by `md_tmplt.`
#' @export
#'
#' @examples
#' library(LTASR)
#' data <- data.frame(id = 1,
#'                    start = as.Date('3/1/2015', format='%m/%d/%Y'),
#'                    end = as.Date('3/15/2015', format='%m/%d/%Y'))
#' expand_dates(data, start, end)
#'
#' @import rlang
expand_dates <-
  function(df,
           start,
           end,
           md_tmplt = seq(as.Date('1/1/2015', '%m/%d/%Y'),
                          as.Date('12/31/2015', '%m/%d/%Y'),
                          by = 'day')) {
    xv <- rlang::enquo(start)
    yv <- rlang::enquo(end)

    df %>%
      dplyr::mutate(
        date = purrr::map2(
          !!xv,
          !!yv,
          ~ purrr::map(
            lubridate::year(.x):lubridate::year(.y),
            ~ lubridate::make_date(
              year = .,
              month = lubridate::month(md_tmplt),
              day = lubridate::day(md_tmplt)
            )
          ) %>%
            purrr::reduce( ~ c(.x, .y))
        ),
        date = purrr::pmap(list(!!xv, date,!!yv),
                          ~ c(..1-1, ..2, ..3)),
        period = purrr::map2(
          !!xv,
          !!yv,
          ~ purrr::map(lubridate::year(.x):lubridate::year(.y),
                      ~ 1:length(md_tmplt) - 1) %>%
            purrr::reduce( ~ c(.x, .y))
        ),
        period = purrr::map(.data$period,
                           ~ c(NA, ., NA))
      ) %>%
      tidyr::unnest(c(.data$date, .data$period)) %>%
      dplyr::distinct(id, date, .keep_all = TRUE) %>%
      dplyr::filter(!!xv - 1 <= date & date <= !!yv) %>%
      dplyr::mutate(period = dplyr::case_when(
        is.na(.data$period) & date == !!yv ~ lag(.data$period) + 1 %% length(md_tmplt),
        TRUE ~ .data$period
      )) %>%
      dplyr::select(-!!xv)
  }
