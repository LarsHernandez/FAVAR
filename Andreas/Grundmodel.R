
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

Aktie <- read_delim("//student.aau.dk/Users/aklitg15/Desktop/aktiepriser_eikon - grundmodel.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ","), trim_ws = TRUE)

COM <- Quandl("FRED/PPIIDC") %>% 
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
COM <- COM$Value
UNE <- UNE$Value
SPREAD <- BOND$SVENY05-BOND$SVENY02
SPREAD2 <- c(rnorm(28, mean = 0.4,sd = 0.1),SPREAD)
  
# Stationæritet-test  ----------------------------------------------------

adf.test(SPREAD2)

dFFR <- diff(FFR)
dCPI <- diff(log(CPI))*100
dPROD <- diff(log(PROD))*100
dDOW <- diff(log(DOW))*100
dCOM <- diff(log(COM))*100
dUNE <- diff(UNE)

# Datasæt til model-------------------------------------------------------

Data <-  cbind(dCPI,dPROD,dFFR) #Grundmodel
Data1 <- cbind(dCPI,dCOM,dPROD,dFFR) #Grundmodel + commodity
Data2 <- cbind(dCPI,dDOW,dPROD,dFFR) #Grundmodel + aktie (D&J)
Data3 <- cbind(dCPI,dUNE,dPROD,dFFR) #Grundmodel + Unemployment
Data4 <- cbind(dCPI,SPREAD2,dPROD,dFFR) #Grundmodel + Spread

# Modeller  --------------------------------------------------------------

VARselect(Data, lag.max = 24)
V <- VAR(Data, p=13)
irf <- irf(V, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = T, n.ahead = 48)
plot(irf)

VARselect(Data1, lag.max = 24)
V1 <- VAR(Data1, p=13)
irf1 <- irf(V1, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = T, n.ahead = 48)
plot(irf1)

VARselect(Data2, lag.max = 24)
V2 <- VAR(Data2, p=13)
irf2 <- irf(V2, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = T, n.ahead = 48)
plot(irf2)

VARselect(Data3, lag.max = 24)
V3 <- VAR(Data3, p=13)
irf3 <- irf(V3, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = T, n.ahead = 48)
plot(irf3)

VARselect(Data4, lag.max = 24)
V4 <- VAR(Data4, p=13)
irf4 <- irf(V4, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = T, n.ahead = 48)
plot(irf4)




