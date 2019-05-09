
library(forecast)
library(tidyverse)
library(Quandl)
library(tseries)
library(vars)


# Data --------------------------------------------------------------------

FFR <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

CPI <- Quandl("FRED/CPIAUCSL", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

PROD <- Quandl("FRED/INDPRO", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)



# Extra data --------------------------------------------------------------


# WALCL - data for centralbankens aktivopkøb


gdfgfdg

############################
