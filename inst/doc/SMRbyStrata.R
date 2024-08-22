## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, messages = FALSE, include = FALSE---------------------------------
library(LTASR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(stringr)
library(knitr)

## ----message=FALSE, results='hide'--------------------------------------------
#Define exposure cutpoints
exp <- exp_strata(var = 'exposure_level',
                   cutpt = c(-Inf, 0, 10000, 20000, Inf),
                   lag = 10)

#Read in and format person file
person <- person_example %>%
  mutate(dob = as.Date(dob, format='%m/%d/%Y'),
         pybegin = as.Date(pybegin, format='%m/%d/%Y'),
         dlo = as.Date(dlo, format='%m/%d/%Y'))

#Read in and format history file
history <- history_example %>%
  mutate(begin_dt = as.Date(begin_dt, format='%m/%d/%Y'),
         end_dt = as.Date(end_dt, format='%m/%d/%Y')) 

#Stratify cohort
py_table <- get_table_history(persondf = person,
                              rateobj = us_119ucod_recent,
                              historydf = history,
                              exps = list(exp))

## ----echo=FALSE---------------------------------------------------------------
py_table %>%
  head() %>%
  kable()

## -----------------------------------------------------------------------------
#Subset py_table to the highest exposed group
py_table_high <- py_table %>%
  filter(exposure_levelCat == '(2e+04, Inf]')

smr_minor_table_high <- smr_minor(py_table_high, us_119ucod_recent)
smr_major_table_high <- smr_major(smr_minor_table_high, us_119ucod_recent)

## ----echo=FALSE---------------------------------------------------------------
smr_minor_table_high %>%
  filter(minor %in% c(55, 52)) %>%
  head() %>%
  kable(digits = 2)
smr_major_table_high %>%
  filter(major %in% c(16)) %>%
  head() %>%
  kable(digits = 2)

## ----eval=FALSE---------------------------------------------------------------
#  #Define the name of the person year table (py_table)
#  #and the variable to calcualte SMRs accross
#  pyt <- py_table
#  var <- 'exposure_levelCat'
#  
#  #Loop through levels of the above variable
#  lvls <- unique(pyt[var][[1]])
#  smr_minors <-
#    map(lvls,
#      ~ {
#        pyt %>%
#          filter(!!sym(var) == .x) %>%
#          smr_minor(us_119ucod_recent)
#      }) %>%
#    setNames(lvls)
#  
#  smr_majors <-
#    map(smr_minors,
#        ~ smr_major(., us_119ucod_recent))%>%
#    setNames(names(smr_minors))
#  
#  #Adjust names of sheets
#  names(smr_minors) <- str_replace_all(names(smr_minors), "\\[|\\]", "_")
#  names(smr_majors) <- str_replace_all(names(smr_majors), "\\[|\\]", "_")
#  
#  #Save results
#  library(writexl)
#  write_xlsx(smr_minors, 'C:/SMR_Minors_by_exp.xlsx')
#  write_xlsx(smr_majors, 'C:/SMR_Majors_by_exp.xlsx')

