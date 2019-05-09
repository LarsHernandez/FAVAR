
library(forecast)
library(tidyverse)
library(Quandl)
library(tseries)
library(vars)


# Data --------------------------------------------------------------------

FFR <- Quandl("FRED/FEDFUNDS") %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

CPI <- Quandl("FRED/CPIAUCSL") %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

PROD <- Quandl("FRED/INDPRO") %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

Aktie <- read_delim("//student.aau.dk/Users/aklitg15/Desktop/aktiepriser_eikon - grundmodel.csv", ";", escape_double = FALSE, trim_ws = TRUE)

COM <- Quandl("") %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

UNE <- Quandl("") %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

# Variabel  --------------------------------------------------------------


# Datasæt   --------------------------------------------------------------

# Modeller  --------------------------------------------------------------