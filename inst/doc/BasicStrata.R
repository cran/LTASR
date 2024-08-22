## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)
library(readr)
library(knitr)
library(LTASR)
per_form <- tibble(Variable = c('id',
'gender',
'race',
'dob',
'pybegin',
'dlo',
'vs',
'code',
'rev'
),
Description = c('Unique identifier for each person',
'Sex of person ("M" = male / "F" = female)',
'Race of person ("W" = white / "N" = nonwhite)',
'date of birth',
'date to begin follow-up',
'date last observed. Minimum of end of study, date of death, date lost to follow-up',
'an indicator of ‘D’ for those who are deceased. All other values will be treated as alive or censored as of dlo.',
'ICD code of death; missing if person is censored or alive at study end date',
'revision of ICD code (7-10); missing if person is censored or alive at study end date'
),
Format = c('',
'character',
'character',
'character',
'character',
'character',
'character',
'character',
'numeric'
)
)

## ----tbl, fig.align='center', echo = FALSE------------------------------------
kable(per_form)

## ----fig.align='center', echo=FALSE, message=FALSE----------------------------
options(knitr.kable.NA = '')
person_example %>%
  kable()

