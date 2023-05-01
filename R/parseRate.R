#' Parses LTAS rate file in .xml format
#'
#' @param xmlpath path of LTAS rate file
#'
#' @return returns a list containing:
#' 1. $residual: the minor number where all unknown deaths will be assigned
#'
#' 2. $MinorDesc: a data.frame/tibble giving descriptions of minor numbers as well as
#' how minors are mapped to majors
#'
#' 3. $mapping: a data.frame/tibble listing how each icd-code and revision will be mapped
#' to each minor number
#'
#' 4. $age_cut: a numeric specifying cut-points for age strata
#'
#' 5. $cp_cut: a numeric specifying cut-points for calendar period strata
#'
#' @export
#'
parseRate <- function(xmlpath){
  rate <-  XML::xmlParse(file = xmlpath)
  rootnode <- XML::xmlRoot(rate)

  Residual <- as.numeric(XML::xmlValue(rootnode[["Residual"]]))

  # Get Descriptions
  description <- dplyr::tibble()
  for (m in which(names(rootnode[['Causes']]) == 'Major')) {
    description <- XML::xmlToDataFrame(rootnode[['Causes']][[m]][-1:-3]) %>%
      dplyr::mutate(minor = as.numeric(Id),
             major = as.numeric(XML::xmlValue(rootnode[['Causes']][[m]][[1]])),
             maj_desc = XML::xmlValue(rootnode[['Causes']][[m]][[2]])) %>%
      dplyr::select(-Id) %>%
      dplyr::bind_rows(description)
  }
  for (m in which(names(rootnode[['Causes']][['Cancers']]) == 'Major')) {
    description <- XML::xmlToDataFrame(rootnode[['Causes']][['Cancers']][[m]][-1:-3]) %>%
      dplyr::mutate(cancer = 1,
             minor = as.numeric(Id),
             can_label = XML::xmlValue(rootnode[['Causes']][['Cancers']][['Label']]),
             major = as.numeric(XML::xmlValue(rootnode[['Causes']][['Cancers']][[m]][[1]])),
             maj_desc = XML::xmlValue(rootnode[['Causes']][['Cancers']][[m]][[2]])) %>%
      dplyr::select(-Id) %>%
      dplyr::bind_rows(description)
  }
  description <- description %>%
    dplyr::arrange(minor)


  # Get Rate Name
  name <- XML::xmlValue(rootnode[['Name']])


  # Get Details/Definitions
  for (i in 8:11){
    assign(names(rootnode[i]), XML::xmlToDataFrame(rootnode[[i]]))
  }



  # Get Mapping
  mapping <- readr::read_csv((XML::xmlValue(rootnode[["CauseMapData"]][[1]])),
                             col_types = 'ccn',
                             col_names = c('rev', 'code', 'minor')) %>%
    dplyr::mutate(minor = as.numeric(minor))

  # Get Rates
  order <- names(rootnode)[8:11]
  mm <- c("Genders" = 'n', "Races" = 'n',"Ages" = 'c',"CalendarPeriods" = 'c')
  mm <- mm[order]
  rates <- readr::read_csv((XML::xmlValue(rootnode[["RateData"]][[1]])),
                           col_types = paste0(c(mm, 'nn'), collapse = ''),
                           col_names = c(order, 'minor', 'rate')) %>%
    dplyr::mutate(minor = as.numeric(minor))

  # # Extend rates 5 calendar years assuming constant rates
  CalendarPeriods <- CalendarPeriods %>%
    dplyr::mutate(BeginDate = lubridate::ymd(.data$BeginDate),
                  EndDate = lubridate::ymd(.data$EndDate))
  maxCP <- max(as.numeric(CalendarPeriods$Id))
  n <- CalendarPeriods %>%
    dplyr::filter(as.numeric(.data$Id) == maxCP) %>%
    dplyr::mutate(
      upper = lubridate::year(.data$EndDate),

      Description = paste(as.numeric(.data$upper) + 1, '-', as.numeric(.data$upper) + 5),
      Id = .data$Id %>%
        as.numeric() %>%
        `+`(1) %>%
        as.character(),

      BeginDate = lubridate::ymd(paste0(.data$upper + 1, '-01-01')),
      EndDate = lubridate::ymd(paste0(.data$upper + 5, '-12-31'))
    ) %>%
    dplyr::select(.data$Id, .data$Description, .data$BeginDate, .data$EndDate)
  CalendarPeriods <- dplyr::bind_rows(CalendarPeriods, n)
  nr <- rates %>%
    dplyr::filter(as.numeric(.data$CalendarPeriods) == maxCP) %>%
    dplyr::mutate(CalendarPeriods = n$Id)
  rates <- dplyr::bind_rows(rates, nr)

  # Map age category indicators in rate file
  age_cut <- c(as.numeric(Ages$EntryPointYears), Inf)
  rates <- Ages %>%
    dplyr::mutate(lower = as.numeric(.data$EntryPointYears),
                  upper = lead(.data$lower),
                  ageCat = paste0("[", trimws(.data$lower), ',', trimws(.data$upper), ")"),
                  ageCat = dplyr::if_else(is.na(.data$upper),
                                          stringr::str_replace(.data$ageCat, 'NA', ' Inf'),
                                          .data$ageCat),
                  Ages = as.numeric(.data$Id))  %>%
    dplyr::select(.data$Ages, .data$ageCat) %>%
    dplyr::right_join(dplyr::mutate(rates, Ages = as.numeric(.data$Ages)), by='Ages') %>%
    dplyr::select(-Ages)

  # Map CP category indicators in rate file
  cp_cut <- c(lubridate::year(CalendarPeriods$BeginDate),
              utils::tail(lubridate::year(CalendarPeriods$EndDate) + 1, 1))
  rates <- CalendarPeriods %>%
    dplyr::mutate(lower = lubridate::year(CalendarPeriods$BeginDate),
           upper = lubridate::year(CalendarPeriods$EndDate) + 1,
           CPCat = paste0("[", trimws(.data$lower), ',', trimws(.data$upper), ")"),
           CalendarPeriods = as.numeric(.data$Id)) %>%
    dplyr::select(.data$CalendarPeriods, .data$CPCat) %>%
    dplyr::right_join(dplyr::mutate(rates, CalendarPeriods = as.numeric(.data$CalendarPeriods)), by='CalendarPeriods') %>%
    dplyr::select(-.data$CalendarPeriods)

  # Map Race category indicators in rate file
  if (identical(Races$Description, c('White', 'All other races'))){
    r_map <- c('White' = 'W', 'All other races' = 'N')
    Races$Description <- r_map[Races$Description]
  }
  rates <- Races %>%
    dplyr::mutate(Races = as.numeric(.data$Id)) %>%
    dplyr::select(.data$Races,
           race = .data$Description) %>%
    dplyr::right_join(rates, by='Races') %>%
    dplyr::select(-Races) %>%
    dplyr::rename(Races = race)

  # Map Gender category indicators in rate file
  if (all.equal(Genders$Description, c('Male', 'Female'))){
    g_map <- c('Male' = 'M', 'Female' = 'F')
    Genders$Description <- g_map[Genders$Description]
  }
  rates <- Genders %>%
    dplyr::mutate(Genders = as.numeric(.data$Id)) %>%
    dplyr::select(.data$Genders,
                  gen = .data$Description) %>%
    dplyr::right_join(rates, by='Genders') %>%
    dplyr::select(-Genders) %>%
    dplyr::rename(Genders = .data$gen)

  rates <- rates %>%
    dplyr::rename(gender = Genders,
           race = Races)

  return(list(residual = Residual,
              MinorDesc = description,
              mapping = mapping,
              rates = rates,
              age_cut = age_cut,
              cp_cut = cp_cut))
}
