
library(forecast)
library(tidyverse)
library(Quandl)
library(tseries)
library(vars)


# Data 2000-2016 --------------------------------------------------------------------

FFR <- Quandl("FRED/FEDFUNDS") %>% 
  filter(Date >= "2000-01-01", Date < "2015-12-01") %>% 
  arrange(Date)

SHA <- Quandl("SHADOWS/US") %>% 
  filter(Date >= "2000-01-01", Date < "2016-01-01") %>% 
  arrange(Date)

CPI <- Quandl("FRED/CPIAUCSL") %>% 
  filter(Date >= "2000-01-01", Date < "2015-12-01") %>% 
  arrange(Date)

PROD <- Quandl("FRED/INDPRO") %>% 
  filter(Date >= "2000-01-01", Date < "2015-12-01") %>% 
  arrange(Date)

Aktie <- read_delim("//student.aau.dk/Users/aklitg15/Desktop/aktiepriser_eikon - negativmodel.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ","), trim_ws = TRUE)

COM <- Quandl("FRED/PPIIDC") %>% 
  filter(Date >= "2000-01-01", Date < "2015-12-01") %>% 
  arrange(Date)

UNE <- Quandl("FRED/UNRATE") %>% 
  filter(Date >= "2000-01-01", Date < "2015-12-01") %>% 
  arrange(Date)

BOND <- Quandl("FED/SVENY", collapse = "m") %>% 
  filter(Date >= "2000-01-01", Date < "2015-12-01") %>% 
  arrange(Date)

# Variabel  --------------------------------------------------------------

FFR <- FFR$Value
SHA <- SHA$`Policy Rate`
CPI <- CPI$Value
PROD <- PROD$Value
DOW <- Aktie$`US DOW JONES INDUSTRIALS SHARE PRICE INDEX (EP) NADJ`
COM <- COM$Value
UNE <- UNE$Value
SPREAD <- BOND$SVENY05-BOND$SVENY02

# Stationæritet-test  ---------------------------------------------------- PROBLEMER MED STATIONÆRITET!!

adf.test(PROD)

dFFR <- diff(FFR)
dSHA <- diff(SHA)
dCPI <- diff(log(CPI))*100
dPROD <- diff(log(PROD))*100
dDOW <- diff(log(DOW))*100
dCOM <- diff(log(COM))*100
dUNE <- diff(UNE)
dSPREAD <- diff(SPREAD)

# Datasæt til model-------------------------------------------------------

Data <-  cbind(dCPI,dPROD,dFFR) #Grundmodel
Data1 <- cbind(dCPI,dPROD,dSHA) #Negativ model
Data2 <- cbind(dCPI,dCOM,dPROD,dSHA) #Negativ model + commodity
Data3 <- cbind(dCPI,dDOW,dPROD,dSHA) #Negativ model + aktie (D&J)
Data4 <- cbind(dCPI,dUNE,dPROD,dSHA) #Negativ model + Unemployment
Data5 <- cbind(dCPI,dSPREAD,dPROD,dSHA) #Negativ model + Spread

# Modeller  --------------------------------------------------------------

VARselect(Data, lag.max = 24)
V <- VAR(Data, p=3)
irf_FFR <- irf(V, impulse = "dFFR", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
plot(irf_FFR)

VARselect(Data1, lag.max = 24)
V1 <- VAR(Data1, p=3)
irf_SHA <- irf(V1, impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
plot(irf_SHA)

VARselect(Data2, lag.max = 24)
V2 <- VAR(Data2, p=3)
irf2 <- irf(V2, impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
plot(irf2)

VARselect(Data3, lag.max = 24)
V3 <- VAR(Data3, p=3)
irf3 <- irf(V3, impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
plot(irf3)

VARselect(Data4, lag.max = 24)
V4 <- VAR(Data4, p=3)
irf4 <- irf(V4, impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
plot(irf4)

VARselect(Data5, lag.max = 24)
V5 <- VAR(Data5, p=3)
irf5 <- irf(V5, impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
plot(irf5)

# TEST af rækkefølge 3 Variable--------------------------------------------------------------------

irf1 <-  irf(VAR(Data1[,c(1,2,3)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf2 <-  irf(VAR(Data1[,c(1,3,2)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf3 <-  irf(VAR(Data1[,c(2,1,3)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf4 <-  irf(VAR(Data1[,c(2,3,1)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf5 <-  irf(VAR(Data1[,c(3,1,2)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf6 <-  irf(VAR(Data1[,c(3,2,1)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)

b <- cbind(irf1$irf$dSHA,
           irf2$irf$dSHA,
           irf3$irf$dSHA,
           irf4$irf$dSHA,
           irf5$irf$dSHA,
           irf6$irf$dSHA)

b %>% as_tibble %>% mutate(n=c(1:25)) %>% gather(variable, value, -n) %>% ggplot(aes(n, value, group=variable)) + geom_line()

# TEST af rækkefølge 4 Variable--------------------------------------------------------------------

irf1 <-  irf(VAR(Data2[,c(1,2,3,4)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf2 <-  irf(VAR(Data2[,c(1,2,4,3)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf3 <-  irf(VAR(Data2[,c(1,3,4,2)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf4 <-  irf(VAR(Data2[,c(1,3,2,4)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf5 <-  irf(VAR(Data2[,c(1,4,2,3)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf6 <-  irf(VAR(Data2[,c(1,4,3,2)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)

irf7 <-  irf(VAR(Data2[,c(2,1,3,4)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf8 <-  irf(VAR(Data2[,c(2,1,4,3)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf9 <-  irf(VAR(Data2[,c(2,3,4,1)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf10 <- irf(VAR(Data2[,c(2,3,1,4)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf11 <- irf(VAR(Data2[,c(2,4,3,1)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf12 <- irf(VAR(Data2[,c(2,4,1,3)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)

irf13 <- irf(VAR(Data2[,c(3,1,2,4)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf14 <- irf(VAR(Data2[,c(3,1,4,2)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf15 <- irf(VAR(Data2[,c(3,2,1,4)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf16 <- irf(VAR(Data2[,c(3,2,4,1)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf17 <- irf(VAR(Data2[,c(3,4,1,2)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf18 <- irf(VAR(Data2[,c(3,4,2,1)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)

irf19 <- irf(VAR(Data2[,c(4,1,2,3)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf20 <- irf(VAR(Data2[,c(4,1,3,2)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf21 <- irf(VAR(Data2[,c(4,2,1,3)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf22 <- irf(VAR(Data2[,c(4,2,3,1)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf23 <- irf(VAR(Data2[,c(4,3,1,2)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)
irf24 <- irf(VAR(Data2[,c(4,3,2,1)], p=3), impulse = "dSHA", response = "dCPI", ortho = T, cumulative = T, n.ahead = 24)

a <- cbind(irf1$irf$dSHA,
           irf2$irf$dSHA,
           irf3$irf$dSHA,
           irf4$irf$dSHA,
           irf5$irf$dSHA,
           irf6$irf$dSHA,
           irf7$irf$dSHA,
           irf8$irf$dSHA,
           irf9$irf$dSHA,
           irf10$irf$dSHA,
           irf11$irf$dSHA,
           irf12$irf$dSHA,
           irf13$irf$dSHA,
           irf14$irf$dSHA,
           irf15$irf$dSHA,
           irf16$irf$dSHA,
           irf17$irf$dSHA,
           irf18$irf$dSHA,
           irf19$irf$dSHA,
           irf20$irf$dSHA,
           irf21$irf$dSHA,
           irf22$irf$dSHA,
           irf23$irf$dSHA,
           irf24$irf$dSHA)

a %>% as_tibble %>% mutate(n=c(1:25)) %>% gather(variable, value, -n) %>% ggplot(aes(n, value, group=variable)) + geom_line()

# GGPLOT ------------------------------------------------------------------

SHA2 <- SHA %>% as_tibble %>% mutate(type="SHA", n=seq(from=as.Date("2000-01-01"), to =as.Date("2015-11-01"), by="months"))
FFR2 <- FFR %>% as_tibble %>% mutate(type="FFR", n=seq(from=as.Date("2000-01-01"), to =as.Date("2015-11-01"), by="months"))
negativ <- rbind(FFR2,SHA2)
negativ %>% ggplot(aes(n, value, linetype=type)) + geom_line() + geom_hline(aes(yintercept=0),linetype="dashed")

irf_FFR1 <- irf_FFR$irf$dFFR
irf_FFR2 <- irf_FFR$Lower$dFFR
irf_FFR3 <- irf_FFR$Upper$dFFR

samligning_FFR <- cbind(irf_FFR1,irf_FFR2,irf_FFR3) %>% as_tibble  %>% mutate(type="hej") %>% mutate(n=c(1:25))
samligning1 <- samligning_FFR %>% gather(variable, value, -type)

irf_SHA1 <- irf_SHA$irf$dSHA
irf_SHA2 <- irf_SHA$Lower$dSHA
irf_SHA3 <- irf_SHA$Upper$dSHA

samligning_SHA <- cbind(irf_SHA1,irf_SHA2,irf_SHA3) %>% as_tibble  %>% mutate(type="dav") %>% mutate(n=c(1:25))
samligning2 <- samligning_SHA %>% gather(variable, value, -type)


samligning <- rbind(samligning_FFR,samligning_SHA)

samligning %>% gather(variable, value, -type, -n) %>% 
  ggplot(aes(n, value, linetype=variable, color=type))+ 
  geom_line(size=0.6) + 
  geom_hline(aes(yintercept=0), linetype="dotted") + 
  scale_linetype_manual(values = c("solid", "dashed", "dashed"))
