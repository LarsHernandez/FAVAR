

# Packages ----------------------------------------------------------------

library(Quandl)
library(tidyverse)
library(magrittr)
library(tseries)
library(forecast)
library(xtable)
library(RColorBrewer)
library(vars)



# Data --------------------------------------------------------------------

# Removed due to not available at 1959.01.01:
#     "CES2000000003" ,"CES3000000008","CES5553100001","IPDMAN" ,"IPNMAN","CES5552000001"
# These 4 have been substituted as original was disconteniued:
#     "WPSID62","WPSFD49502","WPSFD49207","WPSID61", 

FRED_DATA <- c("AWHMAN","AWOTMAN","CLF16OV","CPIAPPSL","CPIAUCSL","CPIMEDSL","CPITRNSL","CPIULFSL","CUMFNS",
               "CUSR0000SA0L2","CUSR0000SA0L5","CUSR0000SAC","CUSR0000SAD","CUSR0000SAS","DMANEMP","DSPIC96",
               "INDPRO","IPBUSEQ","IPCONGD","IPDCONGD","IPDMAT","IPFINAL","IPMANSICS","IPMAT","IPMINE",
               "IPNCONGD","IPNMAT","IPUTIL","LNS12035019","MANEMP","NDMANEMP","PAYEMS","PCE","PCEDG","PCEND",
               "PCES","WPSID62","WPSFD49502","WPSFD49207","WPSID61","RPI","SRVPRD","UEMP15OV","UEMP15T26",
               "UEMP5TO14","UEMPLT5","UEMPMEAN","UNRATE","USCONS","USGOOD","USGOVT","USMINE","USPRIV",
               "USTPU","USTRADE","USWTRADE")

key = "WB3WH-RUprDSyH2xaLbu"

FAVAR <- Quandl(paste0("FRED/", FRED_DATA), api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01")

colnames(FAVAR)[-1] <- str_sub(colnames(FAVAR)[-1], 1, str_length(colnames(FAVAR)[-1]) - 8)
colnames(FAVAR)[-1] <- str_sub(colnames(FAVAR)[-1], 6)



FAVAR_T <- FAVAR %>% 
  mutate_each(.vars = -Date, funs = function(x) if(auto.arima(x)$arma[6] == 1) c(NA, diff(x)) else c(NA,NA, diff(diff(x)))) %>% 
  mutate_each(.vars = -Date, funs(scale))

FAVAR_T <- FAVAR_T[-c(1,2),]


ADF <- do.call("rbind", lapply(X = FAVAR[,-1], FUN = function(x) adf.test(x)))
ADF_T <- do.call("rbind", lapply(X = FAVAR_T[,-1], FUN = function(x) adf.test(x)))



FAVAR_PCA <- prcomp(FAVAR_T[,-1], rank. = 3)
summary(FAVAR_PCA)

factors <- cbind(FAVAR_T[c("Date")], FAVAR_PCA$x)




FFR <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01")

INFL <- Quandl("RATEINF/CPI_USA", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01")



variables <- factors[,-1]
variables$FFR <- FFR[-c(1,2),]$Value
variables$INFL <- diff(INFL$Value)[-1]

library(vars)

m <- VAR(variables, p=12)

m


plot(irf(m, impulse = "FFR", response = "INFL"))














# Plots -------------------------------------------------------------------

FAVAR %>% 
  gather(variable, value, -Date) %>% 
  ggplot(aes(Date, value)) + 
  geom_line(color = "#4E79A7") + 
  facet_wrap(~variable, nrow=5, scale="free") +
  labs(title="Standardized variables for Factor Augmented Vector Auto Regression", x="", y="", subtitle="From Federal Reserve Economic Database")

FAVAR_T %>% 
  gather(variable, value, -Date) %>% 
  ggplot(aes(Date, value)) + 
  geom_line(color = "#4E79A7") + 
  facet_wrap(~variable, nrow=5) +
  labs(title="Standardized variables for Factor Augmented Vector Auto Regression", x="", y="", subtitle="From Federal Reserve Economic Database")

FAVAR_PCA$x %>% 
  as.data.frame() %>% 
  mutate(tid = FAVAR_T[,1]) %>% 
  gather(variable, value, -tid) %>% 
  ggplot(aes(tid, value, color=variable)) + 
  scale_colour_tableau(palette = "Tableau 10", type = "regular") +
  geom_line(alpha=0.5, size=1.8) +
  facet_wrap(~variable, nrow=2) +
  labs(title="Plot of principle component factors", subtitle="From 50 differet macroeconoomic timeseries", x="", y="", color="Factor")


