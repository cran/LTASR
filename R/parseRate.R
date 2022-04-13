#' Parses LTAS rate file in .xml format
#'
#' @param xmlpath path of LTAS rate file
#'
#' @return returns a list containing:
#' 1. $residual: the minor number where all unknown deaths will be assigned
#'
#' 2. $MinorDesc: a dataframe/tibble giving descriptions of minor numbers as well as
#' how minors are mapped to majors
#'
#' 3. $mapping: a dataframe/tibble listing how each icd-code and revision will be mapped
#' to each minor number
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
  maxCP <- max(as.numeric(CalendarPeriods$Id))
  n <- CalendarPeriods %>%
    dplyr::filter(as.numeric(Id) == maxCP) %>%
    dplyr::mutate(
      lower = stringr::word(Description, 1, sep=' - '),
      upper = stringr::word(Description, 2, sep=' - '),

      Description = paste(as.numeric(lower) + 5, '-', as.numeric(upper) + 5),
      Id = Id %>%
        as.numeric() %>%
        `+`(1) %>%
        as.character()
    ) %>%
    dplyr::select(Id, Description)
  CalendarPeriods <- dplyr::bind_rows(CalendarPeriods, n)
  nr <- rates %>%
    dplyr::filter(as.numeric(CalendarPeriods) == maxCP) %>%
    dplyr::mutate(    CalendarPeriods = CalendarPeriods %>%
                 as.numeric() %>%
                 `+`(1) %>%
                 as.character())
  rates <- dplyr::bind_rows(rates, nr)

  # Map age category indicators in rate file
  rates <- Ages %>%
    dplyr::mutate(lower = stringr::word(Description, 1, sep=' - '),
           upper = stringr::word(Description, 2, sep=' - ') %>%
             as.numeric() + 1,
           ageCat = paste0("[", trimws(lower), ',', trimws(upper), ")"),
           ageCat = dplyr::if_else(is.na(upper), stringr::str_replace(ageCat, ' \\+', '') %>%
                                     stringr::str_replace('NA', ' Inf'), ageCat),
           Ages = as.numeric(Id)) %>%
    dplyr::select(Ages, ageCat) %>%
    dplyr::right_join(dplyr::mutate(rates, Ages = as.numeric(Ages)), by='Ages') %>%
    dplyr::select(-Ages)

  # Map CP category indicators in rate file
  rates <- CalendarPeriods %>%
    dplyr::mutate(lower = stringr::word(Description, 1, sep=' - '),
           upper = stringr::word(Description, 2, sep=' - ') %>%
             as.numeric() + 1,
           CPCat = paste0("[", trimws(lower), ',', trimws(upper), ")"),
           CalendarPeriods = as.numeric(Id)) %>%
    dplyr::select(CalendarPeriods, CPCat) %>%
    dplyr::right_join(dplyr::mutate(rates, CalendarPeriods = as.numeric(CalendarPeriods)), by='CalendarPeriods') %>%
    dplyr::select(-CalendarPeriods)

  # Map Race category indicators in rate file
  rates <-dplyr::tibble(Races = c(1, 2),
                  race = c('W', 'N')) %>%
    dplyr::right_join(rates, by='Races') %>%
    dplyr::select(-Races) %>%
    dplyr::rename(Races = race)

  # Map Gender category indicators in rate file
  rates <-dplyr::tibble(Genders = c(1, 2),
                        gen = c('M', 'F')) %>%
    dplyr::right_join(rates, by='Genders') %>%
    dplyr::select(-Genders) %>%
    dplyr::rename(Genders = .data$gen)

  rates <- rates %>%
    dplyr::rename(gender = Genders,
           race = Races)

  return(list(residual = Residual,
              MinorDesc = description,
              mapping = mapping, rates = rates))
}