#' Expand a data.frame to include all dates between a start and end value
#' defined by parameters x and y
#'
#' @param df Input data.frame
#'
#' @param start start date
#' @param end end date
#'
#' @return A data.frame/tibble containing all variables of the input data.frame
#' as well as a new variable, `date`, with repeated rows for each date between
#' `start` and `end`.
#'
#' @export
#' @examples library(LTASR)
#' data <- data.frame(start = as.Date('3/1/2015', format='%m/%d/%Y'),
#'                      end = as.Date('3/15/2015', format='%m/%d/%Y'))
#' expand_dates(data, start, end)
#'
#' @importFrom rlang !!
expand_dates <- function(df, start, end){
  xv <- rlang::enquo(start)
  yv <- rlang::enquo(end)

  df %>%
    dplyr::mutate(date = purrr::map2(!!xv, !!yv, ~ seq(.x, .y, by='day'))) %>%
    dplyr::select(-!!xv, !!yv) %>%
    tidyr::unnest(date)
}
