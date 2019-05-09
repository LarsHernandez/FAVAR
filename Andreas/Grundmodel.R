
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

UNE <- Quandl("FRED/UNRATE") %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

BOND <- Quandl("FED/SVENY", collapse = "m") %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

# Variabel  --------------------------------------------------------------

FFR <- FFR$Value
CPI <- CPI$Value
PROD <- PROD$Value
DOW <- Aktie$`US DOW JONES INDUSTRIALS SHARE PRICE INDEX (EP) NADJ`
COM <-
UNE <- UNE$Value
SPREAD <- BOND$SVENY05-BOND$SVENY02
SPREAD2 <- c(rnorm(29, mean = 0.4,sd = 0.1),SPREAD)
  
# Stationæritet-test  ----------------------------------------------------

adf.test(SPREAD2)

dFFR <- diff(FFR)
dCPI <- diff(log(CPI))
dPROD <- diff(log(PROD))
dDOW <- diff(log(DOW))
dCOM <- diff(log())
dUNE <- diff(UNE)

# Datasæt til model-------------------------------------------------------

Data <-  cbind(dCPI,dPROD,dFFR)
Data1 <- cbind(dCPI,dPROD,dFFR)
Data2 <- cbind(dCPI,dPROD,dFFR)
Data3 <- cbind(dCPI,dPROD,dFFR)
Data4 <- cbind(dCPI,dPROD,dFFR)

# Modeller  --------------------------------------------------------------

V <- VAR(Data, p=)
VARselect(V)
irf()
plot()
