#' Checks all strata in py_table are contained in rate file
#'
#' @param py_table A stratified cohort created by `get_table`
#' @param rateobj A rate object created by `parseRate`
#'
#' @return A list containing:
#'    1. The py_table with strata removed not found in rateobj
#'    2. The observations from py_table that were removed
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
#' rateobj <- us_119ucod_19602020
#'
#' #Stratify person table
#' py_table <- get_table(person, rateobj)
#'
#' #Check Strata are in rate file
#' checkStrata(py_table, rateobj)
#'
#' @importFrom rlang .data
checkStrata <- function(py_table, rateobj) {
  cats <- rateobj$rates %>%
    dplyr::group_by(.data$ageCat, .data$CPCat, .data$gender, .data$race) %>%
    dplyr::filter(dplyr::row_number() == 1)
  out <- dplyr::anti_join(py_table,
                   cats,
                   by=c('ageCat', 'CPCat', 'gender', 'race'))
  if (nrow(out) > 0){
    ct <- out %>%
      dplyr::ungroup() %>%
      dplyr::mutate(obs = rowSums(dplyr::select(., dplyr::starts_with('_o')))) %>%
      dplyr::summarize(obs = sum(.data$obs),
                py = sum(.data$pdays)/365.25)
    message("- ", round(ct$py,0), ' person-years and ', ct$obs,
            " deaths removed\n because not in strata of rate file\n below are example categories:")
    out %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$ageCat, .data$CPCat, .data$gender, .data$race) %>%
      dplyr::filter(dplyr::row_number() < 5) %>%
      knitr::kable()%>%
      print()

    py_table <- dplyr::semi_join(py_table,
                                 dplyr::filter(rateobj$rates, .data$minor==1),
                                 by=c('ageCat', 'CPCat', 'gender', 'race'))
  }
  return(list(py_table, out))
}
