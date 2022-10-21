

#' Check History File
#'
#' `checkHistory` checks that the history dataframe is formatted correctly
#'
#' @param history history dataframe
#'
#' @return No return value, called for side effects
#'
#' @noRd
#'
#' @importFrom rlang .data
checkHistory <- function(history){
  ###################################################
  # Check for necessary variables
  number <- c('id', 'begin_dt', 'end_dt') %in% colnames(history) %>%
    sum()
  if (number != 3) stop("Missing necessary variables. History file must include:
       id, begin_dt, end_dt,
       Note: variable names are CASE-SENSITIVE")


  ###################################################
  # Check if dob, pybegin and dlo are date values
  if (!(lubridate::is.Date(history$begin_dt) &
        lubridate::is.Date(history$end_dt))) stop("Either begin_dt or end_dt of history file is not a date value")


  ###################################################
  # Are dates in proper order (begin_dt <= end_dt)
  dtorder <- history %>%
    ungroup() %>%
    dplyr::mutate(beg_gt_end = (.data$begin_dt > .data$end_dt)) %>%
    dplyr::summarize(beg_gt_end = sum(.data$beg_gt_end))
  if (dtorder$beg_gt_end != 0) stop('At least one person has a period in their history file
       in which end date occurs before the begin date (i.e. end_dt < begin_dt)')


  ###################################################
  # Are there overlapping history periods
  overlap <- history %>%
    arrange(id, .data$begin_dt) %>%
    group_by(id) %>%
    mutate(bad = .data$begin_dt <= lead(.data$begin_dt) & lead(.data$begin_dt) <= .data$end_dt) %>%
    mutate(report = if_else(.data$bad | lag(.data$bad), 1, 0))

  if (sum(overlap$bad, na.rm = TRUE) != 0) {
    overlap %>%
      filter(.data$report == 1) %>%
      select(-.data$bad, -.data$report) %>%
      utils::head(4) %>%
      knitr::kable() %>%
      print()
    stop('There are overlapping exposure periods in the history file
       Above is an example.')
  }
}



