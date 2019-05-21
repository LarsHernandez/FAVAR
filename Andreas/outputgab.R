
library(forecast)
library(tidyverse)
library(Quandl)
library(tseries)
library(vars)
library(mFilter)
library(quantmod)


key <- "WB3WH-RUprDSyH2xaLbu"

PROD <- Quandl("FRED/INDPRO",api_key = key) %>% 
  filter(Date >= "1959-02-01", Date < "2019-01-01") %>% 
  arrange(Date)


PROD <- ts(PROD$Value, start = 1959)

prod <- hpfilter(log(PROD),freq=129600)
plot(prod)



cyc <- as.numeric(prod$cycle)
cyc