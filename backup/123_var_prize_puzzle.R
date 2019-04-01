
# Script to calculate Factor Augmented VAR model --------------------------
# 
# Lars, Andreas og Jens
# Redigeret: 26-03-19
# 
# 
# 


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


th <- theme()



tee <- c("RPI","W875RX1","DPCERA3M086SBEA","CMRMTSPLx","RETAILx","INDPRO","IPFPNSS","IPFINAL","IPCONGD","IPDCONGD",
         "IPNCONGD","IPBUSEQ","IPMAT","IPDMAT","IPNMAT","IPMANSICS","IPB51222S","IPFUELS","CUMFNS","HWI","HWIURATIO",
         "CLF16OV","CE16OV","UNRATE","UEMPMEAN","UEMPLT5","UEMP5TO14","UEMP15OV","UEMP15T26","UEMP27OV","CLAIMSx",
         "PAYEMS","USGOOD","CES1021000001","USCONS","MANEMP","DMANEMP","NDMANEMP","SRVPRD","USTPU","USWTRADE",
         "USTRADE","USFIRE","USGOVT","CES0600000007","AWOTMAN","AWHMAN","HOUST","HOUSTNE","HOUSTMW","HOUSTS","HOUSTW",
         "PERMIT","PERMITNE","PERMITMW","PERMITS","PERMITW","ACOGNO","AMDMNOx","ANDENOx","AMDMUOx","BUSINVx",
         "ISRATIOx","M1SL","M2SL","M2REAL","AMBSL","TOTRESNS","NONBORRES","BUSLOANS","REALLN","NONREVSL","CONSPI",
         "FEDFUNDS","CP3Mx","TB3MS","TB6MS","GS1","GS5","GS10","AAA","BAA","COMPAPFFx","TB3SMFFM","TB6SMFFM","T1YFFM",
         "T5YFFM","T10YFFM","AAAFFM","BAAFFM","TWEXMMTH","EXSZUSx","EXJPUSx","EXUSUKx","EXCAUSx","WPSFD49207",
         "WPSFD49502","WPSID61","WPSID62","OILPRICEx","PPICMM","CPIAUCSL","CPIAPPSL","CPITRNSL","CPIMEDSL",
         "CUSR0000SAC","CUSR0000SAD","CUSR0000SAS","CPIULFSL","CUSR0000SA0L2","CUSR0000SA0L5","PCEPI",
         "DDURRG3M086SBEA","DNDGRG3M086SBEA","DSERRG3M086SBEA","CES0600000008","CES2000000008","CES3000000008",
         "UMCSENTx","MZMSL","DTCOLNVHFNM","DTCTHFNM","INVEST","VXOCLSx")

tee <- toupper(tee)


FAVAR <- Quandl(paste0("FRED/", tee), api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01")

colnames(FAVAR)[-1] <- str_sub(colnames(FAVAR)[-1], 1, str_length(colnames(FAVAR)[-1]) - 8)
colnames(FAVAR)[-1] <- str_sub(colnames(FAVAR)[-1], 6)


FAVAR_S <- FAVAR[sapply(FAVAR, function(x) !any(is.na(x)))]

FAVAR_T <- FAVAR_S %>% 
  mutate_each(.vars = -Date, funs = function(x) if(auto.arima(x)$arma[6] == 1) c(NA, diff(x)) else c(NA,NA, diff(diff(x)))) %>% 
  mutate_each(.vars = -Date, funs(scale))

FAVAR_T <- FAVAR_T[-c(1,2),]


FAVAR_PCA <- prcomp(FAVAR_T[,-1], rank. = 5)
summary(FAVAR_PCA)

factors <- cbind(FAVAR_T[c("Date")], FAVAR_PCA$x)











FFR <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01")

INFL <- Quandl("RATEINF/CPI_USA", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01")

PROD <- Quandl("FRED/INDPRO", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01")



variables <- factors[,-1]
variables$FFR <- diff(FFR$Value)[-1]
variables$INFL <- INFL$Value[-c(1,2)]
variables$PROD <- diff(PROD$Value)[-1]


m <- VAR(variables[,-c(1:5)], p=12)

m


plot(irf(m, impulse = "FFR", response = "INFL",n.ahead = 300))

variables %>% mutate(Date = factors[,1]) %>% gather(variable, value, -Date) %>% ggplot(aes(Date, value)) + geom_line() + facet_wrap(~variable, nrow=2)

adf.test(FFR$Value)



adf.test(FFR$Value)
adf.test(INFL$Value)
adf.test(PROD$Value)








