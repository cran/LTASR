#' 119 UCOD U.S. Death Rate, 1960-2020
#'
#' A list containing referent underlying cause of death (UCOD) rate information
#' for the US population from 1960-2020 for the 119 minor/outcome LTAS groupings
#'
#' @format A list with 4 elements:
#' \describe{
#'   \item{residual}{the minor/outcome number to which unknown/uncategorized outcomes will be mapped to}
#'   \item{MinorDesc}{a data.frame containing descriptions for each minor and major grouping}
#'   \item{mapping}{a tibble detailing which minor number each icd-code and revision combination will be mapped to}
#'   \item{rates}{the population referent rate for each minor for each gender/race/calendar period/age strata}
#'   ...
#' }
#' @source {Available upon request from nioshltas@cdc.gov}
"us_119ucod_19602020"
