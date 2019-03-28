

library(Quandl)
library(tidyverse)
library(magrittr)
library(tseries)
library(forecast)


# Ideas -------------------------------------------------------------------

# Federal funds rate
# IP: total index
# CPI-U: all items
# US Tbill, 3m.
# Money stock: M2
# Monetary base
# Unempl. rate: all wrks
# Tbond const 5yr.
# FX: Japan
# NAPM commodity prices
# NAPM new orders
# NAPM Empl. Index
# Capacity util rate
# Pers cons exp: total
# Pers cons exp: tot. dur
# Pers cons exp: nondur.
# Avg hr earnings constr.
# Housing starts: nonfarm
# SP500: dividend
# Consumer expec. (Mich.)

# removed due to not available at 1959.01.01 ,"CES2000000003" ,"CES3000000008","CES5553100001","IPDMAN" ,"IPNMAN","CES5552000001"

FRED_DATA <- c("AWHMAN","AWOTMAN","CLF16OV",
               "CPIAPPSL","CPIAUCSL","CPIMEDSL","CPITRNSL","CPIULFSL","CUMFNS","CUSR0000SA0L2","CUSR0000SA0L5",
               "CUSR0000SAC","CUSR0000SAD","CUSR0000SAS","DMANEMP","DSPIC96","INDPRO","IPBUSEQ","IPCONGD",
               "IPDCONGD","IPDMAT","IPFINAL","IPMANSICS","IPMAT","IPMINE","IPNCONGD","IPNMAT",
               "IPUTIL","LNS12035019","MANEMP","NDMANEMP","PAYEMS","PCE","PCEDG","PCEND","PCES",
               "WPSID62","WPSFD49502","WPSFD49207","WPSID61", #these 4 have been substituted
               "RPI","SRVPRD","UEMP15OV","UEMP15T26","UEMP5TO14","UEMPLT5",
               "UEMPMEAN","UNRATE","USCONS","USGOOD","USGOVT","USMINE","USPRIV","USTPU","USTRADE","USWTRADE")


FRED_TOP  <- c("GDP","GDPC1","GDPPOT","CPIAUCSL","CPILFESL","GDPDEF","BASE","M1SL","M2SL","M1V","M2V","DFF",
               "DTB3","DGS5","DGS10","DGS30","T5YIE","T10YIE","T5YIFR","TEDRATE","DPRIM","UNRATE","NROU",
               "NROUST","CIVPART","EMRATIO","UNEMPLOY","PAYEMS","MANEMP","ICSA","IC4WSA","MEHOINUSA672N",
               "DSPIC96","PCE","PCEDG","PSAVERT","RRSFS","DSPI","INDPRO","TCU","HOUST","GPDI","CP","STLFSI",
               "DCOILWTICO","USSLIND","DTWEXM","DTWEXB","GFDEBTN","GFDEGDQ188S","EXCSRESNW","TOTCI")


FAVAR <- Quandl(paste0("FRED/", FRED_DATA), api_key = key)

FAVAR1 <- FAVAR %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  mutate_each(.vars = -Date, funs(scale))

colnames(FAVAR)[-1] <- str_sub(colnames(FAVAR)[-1], 1, str_length(colnames(FAVAR)[-1]) - 8)
colnames(FAVAR)[-1] <- str_sub(colnames(FAVAR)[-1], 6)

# DATA --------------------------------------------------------------------

raw <- list(0)
key = "WB3WH-RUprDSyH2xaLbu"

#Growth
raw[["GDP"]]           <- Quandl("FRED/GDP", api_key = key)	          # Gross Domestic Product
raw[["GDPC1"]]         <- Quandl("FRED/GDPC1", api_key = key)	        # Real Gross Domestic Product
raw[["GDPPOT"]]        <- Quandl("FRED/GDPPOT", api_key = key)        # Real Potential Gross Domestic Product

#Prices and Inflation
raw[["CPIAUCSL"]]      <- Quandl("FRED/CPIAUCSL", api_key = key)      # Consumer Price Index for All Urban Consumers: All Items
raw[["CPILFESL"]]      <- Quandl("FRED/CPILFESL", api_key = key)      # Consumer Price Index for All Urban Consumers: All Items Less Food & Energy
raw[["GDPDEF"]]        <- Quandl("FRED/GDPDEF", api_key = key)	      # Gross Domestic Product: Implicit Price Deflator

#Money Supply
raw[["BASE"]]          <- Quandl("FRED/BASE", api_key = key)          # St. Louis Adjusted Monetary Base
raw[["M1SL"]]          <- Quandl("FRED/M1SL", api_key = key)	        # M1 Money Stock
raw[["M2SL"]]          <- Quandl("FRED/M2SL", api_key = key)	        # M2 Money Stock
raw[["M1V"]]           <- Quandl("FRED/M1V", api_key = key)	          # Velocity of M1 Money Stock
raw[["M2V]"]]           <- Quandl("FRED/M2V", api_key = key)          # Velocity of M2 Money Stock

#Interest Rates
raw[["DFF"]]           <- Quandl("FRED/DFF", api_key = key)           # Effective Federal Funds Rate
raw[["DTB3"]]          <- Quandl("FRED/DTB3", api_key = key)          # 3-Month Treasury Bill: Secondary Market Rate
raw[["DGS5"]]          <- Quandl("FRED/DGS5", api_key = key)          # 5-Year Treasury Constant Maturity Rate
raw[["DGS10"]]         <- Quandl("FRED/DGS10", api_key = key)         # 10-Year Treasury Constant Maturity Rate
raw[["DGS30"]]         <- Quandl("FRED/DGS30", api_key = key)         # 30-Year Treasury Constant Maturity Rate
raw[["T5YIE"]]         <- Quandl("FRED/T5YIE", api_key = key)         # 5-year Breakeven Inflation Rate
raw[["T10YIE"]]        <- Quandl("FRED/T10YIE", api_key = key)        # 10-year Breakeven Inflation Rate
raw[["T5YIFR"]]        <- Quandl("FRED/T5YIFR", api_key = key)        # 5-Year, 5-Year Forward Inflation Expectation Rate
raw[["TEDRATE"]]       <- Quandl("FRED/TEDRATE", api_key = key)       # TED Spread
raw[["DPRIME"]]        <- Quandl("FRED/DPRIME", api_key = key)        # Bank Prime Loan Rate

#Employment
raw[["UNRATE"]]        <- Quandl("FRED/UNRATE", api_key = key)        # Civilian Unemployment Rate
raw[["NROU"]]          <- Quandl("FRED/NROU", api_key = key)          # Natural Rate of Unemployment (Long-Term)
raw[["NROUST"]]        <- Quandl("FRED/NROUST", api_key = key)        # Natural Rate of Unemployment (Short-Term)
raw[["CIVPART"]]       <- Quandl("FRED/CIVPART", api_key = key)       # Civilian Labor Force Participation Rate
raw[["EMRATIO"]]       <- Quandl("FRED/EMRATIO", api_key = key)       # Civilian Employment-Population Ratio
raw[["UNEMPLOY"]]      <- Quandl("FRED/UNEMPLOY", api_key = key)      # Unemployed
raw[["PAYEMS"]]        <- Quandl("FRED/PAYEMS", api_key = key)        # All Employees: Total nonfarm
raw[["MANEMP"]]        <- Quandl("FRED/MANEMP", api_key = key)        # All Employees: Manufacturing
raw[["ICSA"]]          <- Quandl("FRED/ICSA", api_key = key)          # Initial Claims
raw[["IC4WSA"]]        <- Quandl("FRED/IC4WSA", api_key = key)        # 4-Week Moving Average of Initial Claims

#Income and Expenditure
raw[["MEHOINUSA672N"]] <- Quandl("FRED/MEHOINUSA672N", api_key = key) # Real Median Household Income in the United States
raw[["DSPIC96"]]       <- Quandl("FRED/DSPIC96", api_key = key)       # Real Disposable Personal Income
raw[["PCE"]]           <- Quandl("FRED/PCE", api_key = key)           # Personal Consumption Expenditures
raw[["PCEDG"]]         <- Quandl("FRED/PCEDG", api_key = key)         # Personal Consumption Expenditures: Durable Goods
raw[["PSAVERT"]]       <- Quandl("FRED/PSAVERT", api_key = key)       # Personal Saving Rate
raw[["RRSFS"]]         <- Quandl("FRED/RRSFS", api_key = key)         # Real Retail and Food Services Sales
raw[["DSPI"]]          <- Quandl("FRED/DSPI", api_key = key)          # Disposable personal income

#Other Economic Indicators
raw[["INDPRO"]]        <- Quandl("FRED/INDPRO", api_key = key)        # Industrial Production Index
raw[["TCU"]]           <- Quandl("FRED/TCU", api_key = key)           # Capacity Utilization: Total Industry
raw[["HOUST"]]         <- Quandl("FRED/HOUST", api_key = key)         # Housing Starts: Total: New Privately Owned Housing Units Started
raw[["GPDI"]]          <- Quandl("FRED/GPDI", api_key = key)          # Gross Private Domestic Investment
raw[["CP"]]            <- Quandl("FRED/CP", api_key = key)            # Corporate Profits After Tax (without IVA and CCAdj)
raw[["STLFSI"]]        <- Quandl("FRED/STLFSI", api_key = key)        # St. Louis Fed Financial Stress Index
raw[["DCOILWTICO"]]    <- Quandl("FRED/DCOILWTICO", api_key = key)    # Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma
raw[["USSLIND"]]       <- Quandl("FRED/USSLIND", api_key = key)       # Leading Index for the United States
raw[["DTWEXM"]]        <- Quandl("FRED/DTWEXM", api_key = key)        # Trade Weighted U.S. Dollar Index: Major Currencies
raw[["DTWEXB"]]        <- Quandl("FRED/DTWEXB", api_key = key)        # Trade Weighted U.S. Dollar Index: Broad

#Debt
raw[["GFDEBTN"]]       <- Quandl("FRED/GFDEBTN", api_key = key)       # Federal Debt: Total Public Debt
raw[["GFDEGDQ188S"]]   <- Quandl("FRED/GFDEGDQ188S", api_key = key)   # Federal Debt: Total Public Debt as Percent of Gross Domestic Product
raw[["EXCSRESNW"]]     <- Quandl("FRED/EXCSRESNW", api_key = key)     # Excess Reserves of Depository Institutions
raw[["TOTCI"]]         <- Quandl("FRED/TOTCI", api_key = key)         # Commercial and Industrial Loans, All Commercial Banks



# Variables in list form --------------------------------------------------

base <- list(GDP = GDP,GDPC1 = GDPC1,GDPPOT = GDPPOT,CPIAUCSL = CPIAUCSL,CPILFESL = CPILFESL,GDPDEF = GDPDEF)

subs <- list(GDP = GDP, GDPC1 = GDPC1, GDPPOT = GDPPOT, CPIAUCSL = CPIAUCSL, CPILFESL = CPILFESL, 
             GDPDEF = GDPDEF, BASE = BASE, M1 = M1, M2 = M2, M1V = M1V, M2V = M2V, DFF = DFF, 
             DTB3 = DTB3, DGS5 = DGS5, DGS10 = DGS10,
             TEDRATE = TEDRATE, DPRIME = DPRIME, UNRATE = UNRATE, NROU = NROU, 
             NROUST = NROUST, CIVPART = CIVPART, EMRATIO = EMRATIO, UNEMPLOY = UNEMPLOY, PAYEMS = PAYEMS, 
             MANEMP = MANEMP, ICSA = ICSA, IC4WSA = IC4WSA, DSPIC96 = DSPIC96, 
             PCE = PCE, PCEDG = PCEDG, PSAVERT = PSAVERT, RRSFS = RRSFS, DSPI = DSPI, INDPRO = INDPRO, 
             TCU = TCU, HOUST = HOUST, GPDI = GPDI, CP = CP, STLFSI = STLFSI, DCOILWTICO = DCOILWTICO, 
             USSLIND = USSLIND, DTWEXM = DTWEXM, DTWEXB = DTWEXB, GFDEBTN = GFDEBTN, GFDEGDQ188S = GFDEGDQ188S, 
             EXCSRESNW = EXCSRESNW, TOTCI = TOTCI)



full <- list(GDP = GDP, GDPC1 = GDPC1, GDPPOT = GDPPOT, CPIAUCSL = CPIAUCSL, CPILFESL = CPILFESL, 
             GDPDEF = GDPDEF, BASE = BASE, M1 = M1, M2 = M2, M1V = M1V, M2V = M2V, DFF = DFF, 
             DTB3 = DTB3, DGS5 = DGS5, DGS10 = DGS10, DGS30 = DGS30, T5YIE = T5YIE, T10YIE = T10YIE, 
             T5YIFR = T5YIFR, TEDRATE = TEDRATE, DPRIME = DPRIME, UNRATE = UNRATE, NROU = NROU, 
             NROUST = NROUST, CIVPART = CIVPART, EMRATIO = EMRATIO, UNEMPLOY = UNEMPLOY, PAYEMS = PAYEMS, 
             MANEMP = MANEMP, ICSA = ICSA, IC4WSA = IC4WSA, MEHOINUSA672N = MEHOINUSA672N, DSPIC96 = DSPIC96, 
             PCE = PCE, PCEDG = PCEDG, PSAVERT = PSAVERT, RRSFS = RRSFS, DSPI = DSPI, INDPRO = INDPRO, 
             TCU = TCU, HOUST = HOUST, GPDI = GPDI, CP = CP, STLFSI = STLFSI, DCOILWTICO = DCOILWTICO, 
             USSLIND = USSLIND, DTWEXM = DTWEXM, DTWEXB = DTWEXB, GFDEBTN = GFDEBTN, GFDEGDQ188S = GFDEGDQ188S, 
             EXCSRESNW = EXCSRESNW, TOTCI = TOTCI)



# Transformation ----------------------------------------------------------

start <- "1959-12-31"
stop <- "2018-01-01"

standard <- function(x) {
  x %>% 
    mutate(Quarter = quarter(Date, with_year = TRUE)) %>% 
    filter(Date < stop & Date > start) %>% 
    group_by(Quarter) %>% 
    summarize(Value = mean(Value)) %>% 
    mutate(Value = scale(Value))
}

base.std <- lapply(X = base, FUN = function(x) standard(x))
subs.std <- lapply(X = subs, FUN = function(x) standard(x))
full.std <- lapply(X = full, FUN = function(x) standard(x))



# Test of stationarity ----------------------------------------------------

do.call("rbind", lapply(X = base.std, FUN = function(x) adf.test(x$Value)))
do.call("rbind", lapply(X = base.std, FUN = function(x) auto.arima(x$Value)$arma[6]))


do.call("rbind", lapply(X = full.std, FUN = function(x) adf.test(x$Value)))
do.call("rbind", lapply(X = full.std, FUN = function(x) auto.arima(x$Value)$arma[6]))


do.call("rbind", lapply(X = subs.std, FUN = function(x) adf.test(x$Value)))
integrate <- do.call("rbind", lapply(X = subs.std, FUN = function(x) auto.arima(x$Value)$arma[6]))


lapply(X = subs.std, FUN = function(x) ifelse(integrate > 0, diff(x$Value), x$Value))
# Transformation ----------------------------------------------------------


subs_df <- data.frame(Quarter = subs.std[[1]][1], sapply(subs.std,'[[',2))



# zivot and andrews structural break unit root test
#

# PCA analisis ------------------------------------------------------------

subs_PCA <- prcomp(subs_df[,-1], rank. = 8)
factors <- subs_PCA$x
subs_PCA

summary(subs_PCA)

plot(subs_PCA, type = "l")

biplot(princomp(subs_df[,-1]))



# Plot of factors ---------------------------------------------------------

subs_df %>% 
  gather(variable, value, -Quarter) %>% 
  mutate(Quarter = yq(Quarter)) %>% 
  ggplot(aes(Quarter, value)) + 
  geom_line(color = "#4E79A7") + 
  facet_wrap(~variable, nrow=5) +
  labs(title="Standardized variables for Factor Augmented Vector Auto Regression", x="", y="", subtitle="From Federal Reserve Economic Database")




factors %>% 
  as.data.frame() %>% 
  mutate(tid = subs_df[,1]) %>% 
  gather(variable, value, -tid) %>% 
  ggplot(aes(tid, value, color=variable)) + 
  scale_colour_tableau(palette = "Tableau 10", type = "regular") +
  geom_line(alpha=0.5, size=1.8) +
  facet_wrap(~variable, nrow=2) +
  labs(title="Plot of principle component factors", subtitle="From 50 differet macroeconoomic timeseries", x="", y="", color="Factor")



FAVAR %>% 
  gather(variable, value, -Date) %>% 
  ggplot(aes(Date, value)) + 
  geom_line(color = "#4E79A7") + 
  facet_wrap(~variable, nrow=5) +
  labs(title="Standardized variables for Factor Augmented Vector Auto Regression", x="", y="", subtitle="From Federal Reserve Economic Database")

FAVAR_PCA$x %>% 
  as.data.frame() %>% 
  mutate(tid = FAVAR[,1]) %>% 
  gather(variable, value, -tid) %>% 
  ggplot(aes(tid, value, color=variable)) + 
  scale_colour_tableau(palette = "Tableau 10", type = "regular") +
  geom_line(alpha=0.5, size=1.8) +
  facet_wrap(~variable, nrow=2) +
  labs(title="Plot of principle component factors", subtitle="From 50 differet macroeconoomic timeseries", x="", y="", color="Factor")























GDP
GDPC1
GDPPOT
CPIAUCSL
CPILFESL
GDPDEF
BASE      
M1
M2
M1V
M2V
DFF
DTB3
DGS5
DGS10
DGS30
T5YIE
T10YIE
T5YIFR
TEDRATE
DPRIM
UNRATE
NROU
NROUST
CIVPART
EMRATIO
UNEMPLOY
PAYEMS
MANEMP
ICSA
IC4WSA
MEHOINUSA672N
DSPIC96
PCE
PCEDG
PSAVERT
RRSFS
DSPI
INDPRO
TCU
HOUST
GPDI
CP
STLFSI
DCOILWTICO
USSLIND
DTWEXM
DTWEXB
GFDEBTN
GFDEGDQ188S
EXCSRESNW
TOTCI