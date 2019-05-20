
library(forecast)
library(tidyverse)
library(Quandl)
library(tseries)
library(vars)


# Data 1959-2019--------------------------------------------------------------------

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

BOND <- Quandl("FED/SVENY", collapse = "m") %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

# Variabel  --------------------------------------------------------------

FFR <- FFR$Value
CPI <- CPI$Value
PROD <- PROD$Value
DOW <- Aktie$`US DOW JONES INDUSTRIALS SHARE PRICE INDEX (EP) NADJ`
COM <- COM$Value
SPREAD <- BOND$SVENY05-BOND$SVENY02
SPREAD2 <- c(rnorm(28, mean = 0.4,sd = 0.1),SPREAD)
  
# Stationæritet-test  ----------------------------------------------------

adf.test(COM)

dFFR <- diff(FFR)
dCPI <- diff(log(CPI))*100
dPROD <- diff(log(PROD))*100
dDOW <- diff(log(DOW))*100
dCOM <- diff(log(COM))*100

# Datasæt til model-------------------------------------------------------

Data <-  cbind(dCPI,dPROD,dFFR) #Grundmodel
Data1 <- cbind(dCPI,dCOM,dPROD,dFFR) #Grundmodel + commodity
Data2 <- cbind(dCPI,dDOW,dPROD,dFFR) #Grundmodel + aktie (D&J)
Data3 <- cbind(dCPI,cyc,dFFR) #Grundmodel + outputgap
Data4 <- cbind(dCPI,SPREAD2,dPROD,dFFR) #Grundmodel + Spread

# Modeller  --------------------------------------------------------------

VARselect(Data, lag.max = 24)
V <- VAR(Data, p=13)
irf <- irf(V, impulse = "dFFR", response = "dFFR", ortho = T, cumulative = F, n.ahead = 48, ci=0.66)
plot(irf)

VARselect(Data1, lag.max = 24)
V1 <- VAR(Data1, p=13)
irf1 <- irf(V1, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48, ci=0.66)
plot(irf1)

VARselect(Data2, lag.max = 24)
V2 <- VAR(Data2, p=13)
irf2 <- irf(V2, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48, ci=0.66)
plot(irf2)

VARselect(Data3, lag.max = 24)
V3 <- VAR(Data3, p=13)
irf3 <- irf(V3, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48, ci=0.66)
plot(irf3)

VARselect(Data4, lag.max = 24)
V4 <- VAR(Data4, p=13)
irf4 <- irf(V4, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
plot(irf4)

# TEST af rækkefølge --------------------------------------------------------------------

irf1 <-  irf(VAR(Data4[,c(1,2,3,4)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf2 <-  irf(VAR(Data4[,c(1,2,4,3)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf3 <-  irf(VAR(Data4[,c(1,3,4,2)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf4 <-  irf(VAR(Data4[,c(1,3,2,4)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf5 <-  irf(VAR(Data4[,c(1,4,2,3)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf6 <-  irf(VAR(Data4[,c(1,4,3,2)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)

irf7 <-  irf(VAR(Data4[,c(2,1,3,4)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf8 <-  irf(VAR(Data4[,c(2,1,4,3)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf9 <-  irf(VAR(Data4[,c(2,3,4,1)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf10 <- irf(VAR(Data4[,c(2,3,1,4)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf11 <- irf(VAR(Data4[,c(2,4,3,1)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf12 <- irf(VAR(Data4[,c(2,4,1,3)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)

irf13 <- irf(VAR(Data4[,c(3,1,2,4)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf14 <- irf(VAR(Data4[,c(3,1,4,2)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf15 <- irf(VAR(Data4[,c(3,2,1,4)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf16 <- irf(VAR(Data4[,c(3,2,4,1)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf17 <- irf(VAR(Data4[,c(3,4,1,2)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf18 <- irf(VAR(Data4[,c(3,4,2,1)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)

irf19 <- irf(VAR(Data4[,c(4,1,2,3)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf20 <- irf(VAR(Data4[,c(4,1,3,2)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf21 <- irf(VAR(Data4[,c(4,2,1,3)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf22 <- irf(VAR(Data4[,c(4,2,3,1)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf23 <- irf(VAR(Data4[,c(4,3,1,2)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
irf24 <- irf(VAR(Data4[,c(4,3,2,1)], p=13), impulse = "dFFR", response = "dCPI", ortho = T, cumulative = F, n.ahead = 48)
F
a <- cbind(irf1$irf$dFFR,
           irf2$irf$dFFR,
           irf3$irf$dFFR,
           irf4$irf$dFFR,
           irf5$irf$dFFR,
           irf6$irf$dFFR,
           irf7$irf$dFFR,
           irf8$irf$dFFR,
           irf9$irf$dFFR,
           irf10$irf$dFFR,
           irf11$irf$dFFR,
           irf12$irf$dFFR,
           irf13$irf$dFFR,
           irf14$irf$dFFR,
           irf15$irf$dFFR,
           irf16$irf$dFFR,
           irf17$irf$dFFR,
           irf18$irf$dFFR,
           irf19$irf$dFFR,
           irf20$irf$dFFR,
           irf21$irf$dFFR,
           irf22$irf$dFFR,
           irf23$irf$dFFR,
           irf24$irf$dFFR)

a %>% as_tibble %>% mutate(n=c(1:49)) %>% gather(variable, value, -n) %>% ggplot(aes(n, value, group=variable, color=variable)) + geom_line()
a
