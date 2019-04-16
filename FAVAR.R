
# Script to calculate Factor Augmented VAR model --------------------------
# 
# Lars, Andreas og Jens
# Redigeret: 26-03-19
#  test
# sddfgdfgdfg

# Packages ----------------------------------------------------------------

library(Quandl)
library(tidyverse)
library(magrittr)
library(tseries)
library(forecast)
library(xtable)
library(RColorBrewer)
library(vars)
library(lubridate)

key <- "WB3WH-RUprDSyH2xaLbu"

th <- theme(plot.title        = element_text(size = 20),
            plot.background   = element_rect(fill = "white", color = NA),
            panel.background  = element_rect(fill = NA,       color = NA), 
            legend.background = element_rect(fill = NA,       color = NA),
            legend.key        = element_rect(fill = NA,       color = NA),
            strip.background  = element_rect(fill = NA,       color = NA),
            panel.border      = element_rect(fill = NA,       color = "black", size = 0.3),
            panel.grid        = element_line(color = NA),
            title             = element_text(color = "black"),
            plot.subtitle     = element_text(color = "grey40"),
            plot.caption      = element_text(color = "grey70"),
            strip.text        = element_text(face  = "bold"),
            axis.text         = element_text(color = "black"),
            axis.ticks        = element_line(color = "black"),
            plot.margin       = unit(c(0.2, 0.1, 0.2, 0.1), "cm"))

# Data --------------------------------------------------------------------

FRED <- c("RPI","W875RX1","DPCERA3M086SBEA","CMRMTSPL","RETAIL","INDPRO","IPFPNSS","IPFINAL","IPCONGD","IPDCONGD",
          "IPNCONGD","IPBUSEQ","IPMAT","IPDMAT","IPNMAT","IPMANSICS","IPB51222S","IPFUELS","CUMFNS","HWI","HWIURATIO",
          "CLF16OV","CE16OV","UNRATE","UEMPMEAN","UEMPLT5","UEMP5TO14","UEMP15OV","UEMP15T26","UEMP27OV","CLAIMS",
          "PAYEMS","USGOOD","CES1021000001","USCONS","MANEMP","DMANEMP","NDMANEMP","SRVPRD","USTPU","USWTRADE",
          "USTRADE","USFIRE","USGOVT","CES0600000007","AWOTMAN","AWHMAN","HOUST","HOUSTNE","HOUSTMW","HOUSTS","HOUSTW",
          "PERMIT","PERMITNE","PERMITMW","PERMITS","PERMITW","ACOGNO","AMDMNO","ANDENO","AMDMUO","BUSINV",
          "ISRATIO","M1SL","M2SL","M2REAL","AMBSL","TOTRESNS","NONBORRES","BUSLOANS","REALLN","NONREVSL","CONSPI",
          "CP3M","TB3MS","TB6MS","GS1","GS5","GS10","AAA","BAA","COMPAPFF","TB3SMFFM","TB6SMFFM","T1YFFM",
          "T5YFFM","T10YFFM","AAAFFM","BAAFFM","TWEXMMTH","EXSZUS","EXJPUS","EXUSUK","EXCAUS","WPSFD49207",
          "WPSFD49502","WPSID61","WPSID62","OILPRICE","PPICMM","CPIAUCSL","CPIAPPSL","CPITRNSL","CPIMEDSL",
          "CUSR0000SAC","CUSR0000SAD","CUSR0000SAS","CPIULFSL","CUSR0000SA0L2","CUSR0000SA0L5","PCEPI",
          "DDURRG3M086SBEA","DNDGRG3M086SBEA","DSERRG3M086SBEA","CES0600000008","CES2000000008","CES3000000008",
          "UMCSENT","MZMSL","DTCOLNVHFNM","DTCTHFNM","INVEST","VXOCLS")

# "FEDFUNDS"
# Download data for the FAVAR - If too much is downloaded the API gets timeout
FAVAR <- Quandl(paste0("FRED/", FRED), api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01")

# Fixing column names
colnames(FAVAR)[-1] <- str_sub(colnames(FAVAR)[-1], 1, str_length(colnames(FAVAR)[-1]) - 8)
colnames(FAVAR)[-1] <- str_sub(colnames(FAVAR)[-1], 6)

# Remove rows with NA's
FAVAR_S <- FAVAR[sapply(FAVAR, function(x) !any(is.na(x)))]

# Transform according to auto.arima
degree <- function(x) if(auto.arima(x)$arma[6] == 1) c(NA, diff(x)) else if(auto.arima(x)$arma[6] == 2) c(NA,NA, diff(diff(x))) else x

FAVAR_T <- FAVAR_S %>% 
  mutate_at(vars(-Date), funs(degree)) %>% 
  mutate_at(vars(-Date), funs(scale))

# Remove NA's due to differencing 
FAVAR_T <- FAVAR_T[-c(1,2),]


#aa <- cor(cbind(FAVAR_T[,-1], FFR[-c(1,2),-1], INFL[-c(1,2),-1]))


IOrder <- FAVAR_S %>% dplyr::select(-Date) %>% apply(MARGIN = 2, FUN = function(x) auto.arima(x)$arma[6])

  


# PCA Factors -------------------------------------------------------------

FAVAR_PCA <- prcomp(FAVAR_T[,-1], rank. = 8)
#summary(FAVAR_PCA)

#factors <- cbind(FAVAR_T[c("Date")], FAVAR_PCA$x)
#xtable(summary(FAVAR_PCA)$importance[,1:8],digits=4) 
#screeplot(summary(prcomp(FAVAR_T[,-1])), type="lines", npcs = 15)





# Factor loadings ---------------------------------------------------------
# 
# aload <- abs(FAVAR_PCA$rotation)
# loadings <- sweep(aload, 2, colSums(aload), "/") %>% as.data.frame %>% rownames_to_column("name")
# 
# P1 <- loadings %>% dplyr::select(name, PC1) %>% top_n(8,PC1) %>% arrange(desc(PC1)) %>% mutate(PC1 = 100 * PC1, name = substr(name, start = 1, stop = 6))
# P2 <- loadings %>% dplyr::select(name, PC2) %>% top_n(8,PC2) %>% arrange(desc(PC2)) %>% mutate(PC2 = 100 * PC2, name = substr(name, start = 1, stop = 6))
# P3 <- loadings %>% dplyr::select(name, PC3) %>% top_n(8,PC3) %>% arrange(desc(PC3)) %>% mutate(PC3 = 100 * PC3, name = substr(name, start = 1, stop = 6))
# P4 <- loadings %>% dplyr::select(name, PC4) %>% top_n(8,PC4) %>% arrange(desc(PC4)) %>% mutate(PC4 = 100 * PC4, name = substr(name, start = 1, stop = 6))
# P5 <- loadings %>% dplyr::select(name, PC5) %>% top_n(8,PC5) %>% arrange(desc(PC5)) %>% mutate(PC5 = 100 * PC5, name = substr(name, start = 1, stop = 6))
# P6 <- loadings %>% dplyr::select(name, PC6) %>% top_n(8,PC6) %>% arrange(desc(PC6)) %>% mutate(PC6 = 100 * PC6, name = substr(name, start = 1, stop = 6))
# P7 <- loadings %>% dplyr::select(name, PC7) %>% top_n(8,PC7) %>% arrange(desc(PC7)) %>% mutate(PC7 = 100 * PC7, name = substr(name, start = 1, stop = 6))
# P8 <- loadings %>% dplyr::select(name, PC8) %>% top_n(8,PC8) %>% arrange(desc(PC8)) %>% mutate(PC8 = 100 * PC8, name = substr(name, start = 1, stop = 6))
# 
# xtable(cbind(P1,P2,P3,P4))
# xtable(cbind(P5,P6,P7,P8))



# Observable variables ----------------------------------------------------

FFR <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  mutate(Value = c(NA, diff(Value)))

INFL <- Quandl("RATEINF/CPI_USA", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  mutate(Value = c(NA, diff(log(Value))*100))



variables <- tibble(INFL =INFL$Value[-c(1,2)], 
                    FFR = FFR$Value[-c(1,2)])

variables <- cbind(FAVAR_T[c("Date")], variables, FAVAR_PCA$x)

#VARselect(variables[,-1])

m <- VAR(variables[,-c(1)], p = 13)
m


#plot(irf(m, impulse = "FFR", response = "INFL", n.ahead = 24))




# Persp plot ---------------------------------------------------------------

n.columns <- 598 # 718m - 72m  Time difference of 45.32564 mins
irff <- matrix(nrow = 44, ncol = n.columns)

start_time <- Sys.time()
for (i in 1:n.columns) {
  result <- variables %>% 
    filter(Date > as.Date("1960-02-01") %m+% months(i) & Date < as.Date("1960-02-01") %m+% months(120 + i)) %>% 
    dplyr::select(-Date) %>% VAR(p = 13)
  irff[,i] <- irf(result, impulse = "FFR", response = "INFL", n.ahead = 43)$irf$FFR
}
end_time <- Sys.time()
end_time - start_time



x <- c(1:44)
y <- seq(1970, 2019, length.out = 598)
z <- irff

# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette(brewer.pal(9,"YlGnBu"))

# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)

# Compute the z-value at the facet centres
zfacet <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4

# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)

a <- persp(x, y, z, col = color[facetcol],
           zlim = c(-0.10, 0.15),
           xlab = "Months", ylab = "", zlab = "%",
           theta = 40, phi = 30, expand = 0.45,
           ticktype = "detailed", lwd = 0.1)

text(trans3d(0, 1970.0, 0.145, a), "Burns",     col = "black")
text(trans3d(0, 1979.8, 0.145, a), "Volcker",   col = "black")
text(trans3d(0, 1987.8, 0.145, a), "Greenspan", col = "black")
text(trans3d(0, 2006.0, 0.145, a), "Bernanke",  col = "black")
text(trans3d(0, 2014.0, 0.145, a), "Yellen",    col = "black")

lines(trans3d(x = 0, y = 1970, z = c(0.13,-0.01),  pmat = a), lwd = 0.2, lty = 2,  col = "black")
lines(trans3d(x = 0, y = 1979, z = c(0.13, 0.045), pmat = a), lwd = 0.2, lty = 2,  col = "black")
lines(trans3d(x = 0, y = 1987, z = c(0.13, 0.05),  pmat = a), lwd = 0.2, lty = 2,  col = "black")
lines(trans3d(x = 0, y = 2006, z = c(0.13, 0.01),  pmat = a), lwd = 0.2, lty = 2,  col = "black")
lines(trans3d(x = 0, y = 2014, z = c(0.13, 0.04),  pmat = a), lwd = 0.2, lty = 2,  col = "black")




# Plot of stationary variables --------------------------------------------

variables %>% 
  mutate(Date = FAVAR_T[,1]) %>% 
  gather(variable, value, -Date) %>% 
  ggplot(aes(Date, value)) + 
  geom_line(size=0.4) + 
  facet_wrap(~variable, nrow=7, scales = "free") +
  th + theme(axis.title = element_blank())











# Structural shifts -------------------------------------------------------

Martin    <- variables %>% filter(Date > "1951-01-01", Date < "1970-02-01") %>% dplyr::select(-Date) %>% VAR(p = 4) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Burns     <- variables %>% filter(Date > "1970-02-01", Date < "1979-08-01") %>% dplyr::select(-Date) %>% VAR(p = 4) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Volcker   <- variables %>% filter(Date > "1979-08-01", Date < "1987-08-01") %>% dplyr::select(-Date) %>% VAR(p = 4) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Greenspan <- variables %>% filter(Date > "1987-08-01", Date < "2006-01-01") %>% dplyr::select(-Date) %>% VAR(p = 4) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Bernanke  <- variables %>% filter(Date > "2006-01-01", Date < "2014-01-01") %>% dplyr::select(-Date) %>% VAR(p = 4) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Yellen    <- variables %>% filter(Date > "2014-01-01", Date < "2018-02-01") %>% dplyr::select(-Date) %>% VAR(p = 3) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)



df <- rbind(tibble(IRF = Martin$irf$FFR[,1],    Lower = Martin$Lower$FFR[,1],    Upper = Martin$Upper$FFR[,1],    N = c(0:24), type = "Martin\n1951 - 1970"),
            tibble(IRF = Burns$irf$FFR[,1],     Lower = Burns$Lower$FFR[,1],     Upper = Burns$Upper$FFR[,1],     N = c(0:24), type = "Burns\n1970 - 1978"),
            tibble(IRF = Volcker$irf$FFR[,1],   Lower = Volcker$Lower$FFR[,1],   Upper = Volcker$Upper$FFR[,1],   N = c(0:24), type = "Volcker\n1979 - 1987"),
            tibble(IRF = Greenspan$irf$FFR[,1], Lower = Greenspan$Lower$FFR[,1], Upper = Greenspan$Upper$FFR[,1], N = c(0:24), type = "Greenspan\n1987 - 2006"),
            tibble(IRF = Bernanke$irf$FFR[,1],  Lower = Bernanke$Lower$FFR[,1],  Upper = Bernanke$Upper$FFR[,1],  N = c(0:24), type = "Bernanke\n2006 - 2014"),
            tibble(IRF = Yellen$irf$FFR[,1],    Lower = Yellen$Lower$FFR[,1],    Upper = Yellen$Upper$FFR[,1],    N = c(0:24), type = "Yellen\n2014 - 2018")) %>% 
  gather(variable, value, -type, -N)

df$type <- factor(df$type, levels = c("Martin\n1951 - 1970", "Burns\n1970 - 1978", "Volcker\n1979 - 1987","Greenspan\n1987 - 2006","Bernanke\n2006 - 2014","Yellen\n2014 - 2018"))

df %>% ggplot(aes(N, value, linetype = variable)) +
  geom_line()+
  facet_wrap(~type) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  geom_hline(aes(yintercept = 0), size = 0.5, alpha = 0.5) +
  scale_y_continuous(limits=c(-0.14,0.24)) +
  scale_x_continuous("Lags", limits = c(0,24), breaks = seq(0, 24, 4)) +
  labs(title = expression(paste(bold("Figur 4.7  "), "Strukturelle skift 1960 - 2018 (INFL, PROD, FFR)")), 
       color = "Procent", y = "", x = "Lags", caption = "Kilde: FRED + Egne beregninger", linetype = "") +
  th





# Hvor mange faktorere ----------------------------------------------------

p0 <- variables %>% dplyr::select(INFL, FFR)                                         %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
p1 <- variables %>% dplyr::select(INFL, FFR, PC1)                                    %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
p3 <- variables %>% dplyr::select(INFL, FFR, PC1, PC2, PC3)                          %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
p5 <- variables %>% dplyr::select(INFL, FFR, PC1, PC2, PC3, PC4, PC5)                %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
p8 <- variables %>% dplyr::select(INFL, FFR, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8) %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)

df <- rbind(tibble(IRF = p0$irf$FFR[,1],    Lower = p0$Lower$FFR[,1],    Upper = p0$Upper$FFR[,1],    N = c(0:48), type = "0 Faktorere"),
            tibble(IRF = p1$irf$FFR[,1],    Lower = p1$Lower$FFR[,1],    Upper = p1$Upper$FFR[,1],    N = c(0:48), type = "1 Faktorere"),
            tibble(IRF = p3$irf$FFR[,1],    Lower = p3$Lower$FFR[,1],    Upper = p3$Upper$FFR[,1],    N = c(0:48), type = "3 Faktorere"),
            tibble(IRF = p5$irf$FFR[,1],    Lower = p5$Lower$FFR[,1],    Upper = p5$Upper$FFR[,1],    N = c(0:48), type = "5 Faktorere"),
            tibble(IRF = p8$irf$FFR[,1],    Lower = p8$Lower$FFR[,1],    Upper = p8$Upper$FFR[,1],    N = c(0:48), type = "8 Faktorere")) %>% 
  gather(variable, value, -type, -N)

df %>% ggplot(aes(N, value, linetype = variable)) +
  geom_line(size = 0.4) +
  facet_wrap(~type, nrow = 1) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  geom_hline(aes(yintercept = 0), size = 0.5, alpha = 0.5) +
  scale_x_continuous("Lags", limits = c(0,48), breaks = seq(0, 48, 8)) +
  scale_y_continuous(limits = c(-0.03,0.08)) +
  labs(x = "Lags", linetype = "") +
  th + theme(axis.title.y = element_blank(), legend.position = "none")



# IRF ---------------------------------------------------------------------

data <- irf(m, n.ahead = 49, cumulative = F)
variables <- data$irf %>% names

ir <- lapply(1:length(variables), function(e){
  data_to_plot <- data.frame(data %>% `$`(irf) %>% `[[`(variables[e])) %>%
    mutate("t" = 1:NROW(.)) %>%
    gather(.,Variable, Value, -t)
  
  upper_ci <- data.frame(data %>% `$`(Upper) %>% `[[`(variables[e])) %>%
    mutate("t" = 1:NROW(.)) %>%
    gather(.,Variable, Upper, -t)
  
  lower_ci <- data.frame(data %>% `$`(Lower) %>% `[[`(variables[e]) ) %>%
    mutate("t" = 1:NROW(.)) %>%
    gather(.,Variable, Lower, -t)
  
  res <- inner_join(data_to_plot, upper_ci, c("t","Variable")) %>%
    inner_join(.,lower_ci, c("t","Variable")) %>%
    mutate(impulse = paste("Shock to", variables[e])) 
}) %>% bind_rows

ir$t <- ir$t-1

ggplot(ir, aes(x = t, y = Value, group = Variable))  +
  geom_line(size = 0.4) +
  geom_line(aes(x = t, y = Upper), linetype = "dashed", size = 0.2, alpha = 0.5) +
  geom_line(aes(x = t, y = Lower), linetype = "dashed", size = 0.2, alpha = 0.5) +
  geom_hline(aes(yintercept = 0), size = 0.5, alpha = 0.5) +
  scale_x_continuous("Lags", limits = c(0,48), breaks = seq(0, 48, 4)) +
  #scale_y_continuous("Percent\n ", position = "right", limits = c(-0.4,1), breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
  facet_grid(Variable ~ impulse, switch = "y", scales = "free") +
  labs(title=expression(paste(bold("Figure 4.1 "), " Impulse Response Functions")), 
       subtitle="Note: 95% Confidence intervals by bootstrap, 100 runs", 
       caption="Source: FRED + Egne beregninger") +
  th





# Interesting variables ---------------------------------------------------

TOP <- c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST","M2REAL","M2SL")

ttt <- Quandl(paste0("FRED/", TOP), api_key = key) %>% 
  filter(Date >= "1959-03-01", Date < "2019-01-01")

colnames(ttt)[-1] <- str_sub(colnames(ttt)[-1], 1, str_length(colnames(ttt)[-1]) - 8)
colnames(ttt)[-1] <- str_sub(colnames(ttt)[-1], 6)

colnames(ttt) <- c("Date",
                   "1. IPMANSICS\nIndustrial Production:\nManufacturing",
                   "2. CUSR0000SAC\nConsumer Price Index\nfor Urban Consumers",
                   "3. T10YFFM\n10-Year Treasury Constant\nMaturity Minus FFR",
                   "4. GS5\n5-Year Treasury Constant\nMaturity Rate",
                   "5. UEMP15OV\nNumber of Civilians Unemployed\nfor >15 Weeks",
                   "6. HOUST\nTotal New Privately Owned\nHousing Units Started",
                   "7. M2REAL\nReal M2 Money Stock",
                   "8. M2SL\nM2 Money Stock"
)

ttt %>% gather(variable, value, -Date) %>% 
  ggplot(aes(Date, value)) + 
  geom_line(size=0.4) + 
  facet_wrap(~variable, scale="free", nrow = 2) + 
  th  + theme(axis.title=element_blank())


# Summary statistics ------------------------------------------------------

stargazer(FAVAR, summary.stat = c("n", "mean", "sd", "min", "max"))
res <- apply(FAVAR_S[,-1], FUN = function(x) auto.arima(x)$arma[6], MARGIN = 2)
xtable(res)













# PCA reconstruction ------------------------------------------------------

obj <- irf(m, impulse = "FFR", n.ahead = 48)
eigen <- FAVAR_PCA$rotation

val <- obj[1]$irf$FFR[,-c(1,2)] %*% t(eigen) %>% 
  cbind(.,obj[1]$irf$FFR[,c(1,2)]) %>% 
  as.data.frame %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$FFR[,-c(1,2)] %*% t(eigen) %>% 
  cbind(.,obj[2]$Lower$FFR[,c(1,2)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$FFR[,-c(1,2)] %*% t(eigen) %>% 
  cbind(.,obj[3]$Upper$FFR[,c(1,2)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

val %>% 
  full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  filter(variable %in% TOP) %>% 
  gather(type, value, -variable,-N) %>% 
  group_by(type, variable) %>% 
  mutate(value2 = case_when(variable %in% c("M2SL","M2REAL")~ cumsum(value),
                            variable %in% c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST")~value)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels=TOP)) %>% 
  ggplot(aes(N, value2, linetype=type)) + 
  geom_line(size=0.4) +
  geom_blank(aes(value2=-value2)) + 
  scale_x_continuous("Lags", limits = c(0,48), breaks = seq(0, 48, 6)) +
  scale_linetype_manual(values = c("dotted", "solid", "dotted")) + 
  geom_hline(aes(yintercept = 0), color="grey") + 
  facet_wrap(~variable, scales = "free", nrow=2) + 
  th  + theme(axis.title.y = element_blank(), legend.position = "none")



# Correlation matrix ------------------------------------------------------

vars <- FAVAR_T %>% dplyr::select(IPMANSICS,CUSR0000SAC,T10YFFM,GS5,UEMP15OV,HOUST,M2REAL,M2SL)
fact <- FAVAR_PCA$x

cor(vars,fact) %>% xtable()





# FEVD - VARIABLES --------------------------------------------------------

decomp <- fevd(m, n.ahead = 60)
decomp$INFL[60,-c(1,2)] %*% t(eigen) %>% 
  t() %>% 
  as.tibble %>% 
  mutate(name = rownames(eigen)) %>% 
  arrange(desc(V1)) %>% 
  xtable(digits = 4)














