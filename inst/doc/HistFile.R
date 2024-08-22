## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(knitr)
library(LTASR)
per_form <- tibble(Variable = c('id',
'begin_dt',
'end_dt',
'\\<daily exposure variables\\>'
),
Description = c('Unique identifier for each person',
'Beginning date of exposure period',
'End date of exposure period',
'Exposure variable(s)'
),
Format = c('',
'character',
'character',
'numeric'
)
)

## ----tbl, fig.align='center', echo = FALSE------------------------------------
kable(per_form)

## ----fig.align='center', echo=FALSE, message=FALSE----------------------------
options(knitr.kable.NA = '')
history_example %>%
  kable()

## -----------------------------------------------------------------------------
person <- person_example %>%
  mutate(dob = as.Date(dob, format='%m/%d/%Y'),
         pybegin = as.Date(pybegin, format='%m/%d/%Y'),
         dlo = as.Date(dlo, format='%m/%d/%Y'))
         
history <- history_example %>%
  mutate(begin_dt = as.Date(begin_dt, format='%m/%d/%Y'),
         end_dt = as.Date(end_dt, format='%m/%d/%Y')) %>%
  group_by(id)

## ----fig.align='center', echo=FALSE, message=FALSE----------------------------
options(knitr.kable.NA = '')
person_example %>%
  filter(id == 1) %>%
  kable()

## ----pressure, echo=FALSE, fig.align='center', fig.width=7--------------------
pid <- 1
history %>%
  filter(id ==pid) %>%
  bind_rows(tibble(id=pid, 
                   begin_dt=filter(history, id==pid, row_number()==n())$end_dt + 1,
                   end_dt=filter(person, id==pid)$dlo,
                   employed=0,
                   exposure_level=0)) %>%
  expand_dates(begin_dt, end_dt) %>%
  filter(!is.na(period)) %>%
  mutate(Date = date, 
         Employed = cumsum(employed),
         `Exposure Level` = cumsum(exposure_level)) %>%
  select(id, Date, Employed, `Exposure Level`) %>%
  pivot_longer(c(Employed, `Exposure Level`)) %>%
  
  ggplot() +
  geom_line(aes(x=Date,
                y=value)) +
  facet_wrap(vars(name), nrow = 1, scales = "free") +
  theme_bw()


## -----------------------------------------------------------------------------
exp1 <- exp_strata(var = 'employed',
                   cutpt = c(-Inf, 365, Inf),
                   lag = 0)
exp2 <- exp_strata(var = 'exposure_level',
                   cutpt = c(-Inf, 0, 10000, 20000, Inf),
                   lag = 10)

## ----message=FALSE, results='hide'--------------------------------------------
py_table <- get_table_history(persondf = person,
                              rateobj = us_119ucod_recent,
                              historydf = history,
                              exps = list(exp1, exp2))

## ----echo=FALSE---------------------------------------------------------------
py_table %>%
  head() %>%
  kable()


## ----message=FALSE, results='hide'--------------------------------------------
py_table_est <- get_table_history_est(persondf = person,
                                  rateobj = us_119ucod_recent,
                                  historydf = history,
                                  exps = list(exp1, exp2),
                                  step = 7)

## ----echo=FALSE---------------------------------------------------------------
py_table_est %>%
  head() %>%
  kable(digits = 1)


## ----echo=FALSE, fig.align='center', fig.width=8, fig.height=4----------------
times <- c(360.33, 192.92, 138.79, 116.28, 101.35,  85.00,  79.82,  71.31,  68.42,  63.43,  43.82,  33.28) 

ggplot() +
  geom_point(aes(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 365),
                 y = times/60)) +
  theme_bw() +
  scale_x_continuous(name="Step", 
                     trans = 'log10',
                     breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 365))+
  scale_y_continuous(name="Minutes",
                     breaks = 0:6,
                     limits = c(0, 6.5))

