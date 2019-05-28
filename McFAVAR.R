
library(tidyverse)
library(scales)
library(nowcasting)
library(missMDA)
library(stargazer)
library(Quandl)
library(fredr)
library(tseries)
library(gridExtra)
library(vars)
library(forecast)

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




current <- read_csv("current.txt", col_types = cols(sasdate = col_date(format = "%m/%d/%Y"))) %>%
  filter(sasdate >= as.Date("1959-01-01"), sasdate < as.Date("2019-01-01"))

DateVec <- current[,1]
current <- current[,-1]
Transform <- read_csv("current.txt") %>% .[1,] %>% .[,-1]
current <- rbind(Transform, current) ## 2.


## 1. Raw data for loop
## 2. Removing first 2 observations to allow for differences - defining the y dataset
## 3. Number of variables + Date-vec
## 4. Number of observations + the transformation code

RawData <- current                
x = RawData                       ## 1.
y = RawData[-c(2:3),]             ## 2.
y = y[FALSE,]                     
n <- ncol(x)                      ## 3.
t <- as.integer(nrow(x))          ## 4.


# Stationarity ------------------------------------------------------------
for (i in 1:n) {
  
  if (x[1,i] == 1) {       ## no Transformation/already stationary series
    y[4:t,i] <- x[4:t,i]
  }
  else  if (x[1,i] == 2) { ## First difference
    y[4:t,i] <- x[4:t,i]-x[3:(t-1),i]
  }
  else if (x[1,i] == 3) {  ## Second difference
    y[4:t,i] <- x[4:t,i]-2*x[3:(t-1),i]+x[2:(t-2)]
  }
  else if (x[1,i] == 4) {  ## Ln
    y[4:t,i] <- log(x[4:t,i])
  }
  else if (x[1,i] == 5) {  ## First difference of ln
    y[4:t,i] <- log(x[4:t,i])-log(x[3:(t-1),i])
  }
  else if (x[1,i] == 6) {  ## Second difference of ln
    y[4:t,i] <- log(x[4:t,i])-2*log(x[3:(t-1),i])+log(x[2:(t-2),i])
  }
  else if (x[1,i] == 7) {  ## First difference of percentage change
    y[4:t,i] <- (x[4:t,i]-x[3:(t-1),i])/x[3:(t-1),i]-(x[3:(t-1),i]-x[2:(t-2),i])/x[2:(t-2),i]
  } 
}

rm(x, Transform) ## Clearing uneeded data
y = y[4:t,]      ## Removing the first 2 months because of differencing leading to NA's


##########################
## Determining outliers ##
##########################

## Outliers: abs(x-median)>10*interquartile_ranget
## 1. Determining the median of each series
## 2. Increasing the dimensions of the median set, by repeating 1,i entries t times
## 3. Determining the quantiles
## 4. Sample Interquantile range X_0.75-X_0.25 
## 5. Increasing dimensions of interquantile range, by repeating 1,i entries t times
## 6. Critical value for each entry
## 7. Defining the binary outlier dataset 1=outlier

Median_y  <- y[FALSE,]
qnt       <- y[FALSE,]
IQR_y     <- y[FALSE,]
Z         <- y[FALSE,]
outlier_y <- y[FALSE,]
for (i in 1:n) {
  Median_y[1,i] <- median(as.matrix(y[1:t, i]), na.rm = TRUE)                 ## 1.
  Median_y[1:t,i] <- rep(Median_y[1,i], each = t)                             ## 2.
  qnt[1:2,i] <- quantile(as.matrix(y[,i]), probs=c(0.25, 0.75), na.rm = TRUE) ## 3.
  IQR_y[1,i] = 10*(qnt[2,i]-qnt[1,i])                                         ## 4.
  IQR_y[1:t,i] = rep(IQR_y[1,i], each = t)                                    ## 5.
  Z[1:t,i]=abs(y[1:t,i]-Median_y[1:t,i])                                      ## 6. 
  outlier_y[1:t,i] = Z[1:t,i]>IQR_y[1:t,i]                                    ## 7.
}
rm(Median_y, qnt, Z, IQR_y)                                                  



#######################
## Removing outliers ##
#######################

y_no = y
outlier_y[is.na(outlier_y)] <- 0
for (i in 1:n) {
  for (k in 1:t) {
    if (outlier_y[k,i] == 1){
      y_no[k,i]  <- NA ## Replacing all values of y considered an outlier by NA
    }
  }
}
rm(outlier_y)

## EM - Algorithm for missing data ## 
y_no <- as.data.frame(y_no)
EM_up = imputePCA(y_no, maxiter = 150)
yt = EM_up$completeObs
yt = as.data.frame(yt)
colnames(yt) = colnames(y_no)



############################
## Demean and standardize ##
############################

yt_z = yt[FALSE,]
for (i in 1:ncol(yt)){
  yt_z[1:nrow(yt),i] <- (yt[,i]-mean(yt[,i]))/sd(yt[,i])
}
## These lines checks if the mean and variance of each column indeed is
## 0 and 1 respectively
# colMeans(yt_z)   
# apply(yt_z, 2, sd) 
## yt_z is hence the Z-standardized dataset.


fit.pca <- prcomp(yt_z, rank. = 8, scale. = T)




# NEW ---------------------------------------------------------------------

# Observable variables ----------------------------------------------------

FFR <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  mutate(Value = c(NA, diff(Value)))

INFL <- Quandl("RATEINF/CPI_USA", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  mutate(Value = c(NA, diff(log(Value))*100))

PROD <- Quandl("FRED/INDPRO", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  mutate(Value = c(NA, diff(log(Value))*100))


variables <- tibble(INFL =INFL$Value[-c(1,2)], 
                    FFR = FFR$Value[-c(1,2)])

variables <- cbind(DateVec[-c(1,2),], variables, fit.pca$x)

#VARselect(variables[,-1])

m <- VAR(variables[,-c(1)], p = 13)
m



p0 <- variables %>% dplyr::select(INFL, FFR)                                         %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
p1 <- variables %>% dplyr::select(INFL, PC1, FFR)                                    %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
p3 <- variables %>% dplyr::select(INFL, PC1, PC2, PC3, FFR)                          %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
p5 <- variables %>% dplyr::select(INFL, PC1, PC2, PC3, PC4, PC5, FFR)                %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
p8 <- variables %>% dplyr::select(INFL, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, FFR) %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)

df <- rbind(tibble(IRF = p0$irf$FFR[,1],    Lower = p0$Lower$FFR[,1],    Upper = p0$Upper$FFR[,1],    N = c(0:48), type = "0 Faktorere"),
            tibble(IRF = p1$irf$FFR[,1],    Lower = p1$Lower$FFR[,1],    Upper = p1$Upper$FFR[,1],    N = c(0:48), type = "1 Faktorere"),
            tibble(IRF = p3$irf$FFR[,1],    Lower = p3$Lower$FFR[,1],    Upper = p3$Upper$FFR[,1],    N = c(0:48), type = "3 Faktorere"),
            tibble(IRF = p5$irf$FFR[,1],    Lower = p5$Lower$FFR[,1],    Upper = p5$Upper$FFR[,1],    N = c(0:48), type = "5 Faktorere"),
            tibble(IRF = p8$irf$FFR[,1],    Lower = p8$Lower$FFR[,1],    Upper = p8$Upper$FFR[,1],    N = c(0:48), type = "8 Faktorere")) %>% 
  gather(variable, value, -type, -N)

p1 <- df %>% ggplot(aes(N, value, linetype = variable)) +
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size = 0.3) +
  facet_wrap(~type, nrow = 1) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 8)) +
  scale_y_continuous(limits = c(-0.03,0.08)) +
  labs(x = "Lags", linetype = "") +
  th + theme(axis.title.y = element_blank(), legend.position = "none")

ggsave(plot = p1, filename = "GENERATE/MC1.pdf", width = 30, height = 6, units = "cm", dpi = 320)


p2 <- df %>% group_by(type, variable) %>% 
  mutate(cum = cumsum(value)) %>% 
  ungroup() %>% 
  ggplot(aes(N, cum, linetype = variable)) +
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size = 0.3) +
  facet_wrap(~type, nrow = 1) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 8)) +
  labs(x = "Lags", linetype = "") +
  th + theme(axis.title.y = element_blank(), legend.position = "none")

p3 <- df %>% group_by(type, variable) %>% 
  mutate(cum = cumsum(value)) %>% 
  ungroup() %>% 
  ggplot(aes(N, cum, linetype = variable)) +
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size = 0.3) +
  facet_wrap(~type, nrow = 1) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 8)) +
  labs(x = "Lags", linetype = "") +
  th + theme(axis.title.y = element_blank(), legend.position = "none")

pp <- grid.arrange(p13 + labs(title="Quandl data - 96 variable")+ theme(plot.title = element_text(size = 13)),
             p2 + labs(title="McCracken data - 134 variable") + theme(plot.title = element_text(size = 13)),
             nrow=2)

ggsave(plot = pp, filename = "GENERATE/MCpp.pdf", width = 30, height = 12, units = "cm", dpi = 320)
# Forcasting --------------------------------------------------------------
fit.pca <- prcomp(yt_z, rank. = 5, scale. = T)

variables <- tibble(INFL =INFL$Value[-c(1,2)], 
                    FFR = FFR$Value[-c(1,2)])
variables <- cbind(DateVec[-c(1,2),], variables, fit.pca$x)



raw_var <- yt_z


Data.Frame <- xts(variables[,c(2,3)], order.by = variables[,1])

VAR.Data <- xts(cbind(PROD[,2], INFL[,2], FFR[,2]), order.by = FFR[,1])[-c(1,2),]
colnames(VAR.Data) <- c("PROD", "INFL", "FFR")



set.seed(2019)
init_p    <- 550
nahead    <- 3
noMods    <- 8
end_p     <- nrow(Data.Frame) - nahead
n_windows <- end_p - init_p 
mat       <- data.frame(matrix(0, nrow = (n_windows+1), ncol = noMods))
fcst      <- xts(mat, order.by = index(Data.Frame[(nrow(Data.Frame) - n_windows):nrow(Data.Frame), ]))

colnames(fcst) <- c("RW", "MM", "AR(1)", "ARMA(5,1)", "VAR(1)", "VAR(13)", "FAVAR(1)", "FAVAR(5)")

for (i in 1:n_windows) { 
  
  Data_set          <- raw_var[1:(i+init_p-1),]
  Fhat              <- xts(prcomp(Data_set, rank. = 5)$x, order.by = index(Data.Frame[1:(i+init_p-1)]))
  
  T_BASE            <- Data.Frame[1:(i + init_p - 1), 1:2]$INFL
  T_VAR             <- VAR.Data[1:(i + init_p - 1),]
  T_FAVAR           <- Data.Frame[1:(i + init_p - 1), 1:2] %>% merge.xts(Fhat)
  
  
  M_RW             <- arima(T_BASE, order = c(0,1,0))
  M_MM             <- arima(T_BASE, order = c(0,0,0), include.mean = T)  
  M_AR             <- arima(T_BASE, order = c(1,0,0))  
  M_ARMA           <- arima(T_BASE, order = c(5,0,1))
  M_VAR1           <- VAR(T_VAR,    type = "none", p = 1)
  M_VAR2           <- VAR(T_VAR,    type = "none", p = 13)
  M_FAVAR1         <- VAR(T_FAVAR,  type = "none", p = 1)
  M_FAVAR2         <- VAR(T_FAVAR,  type = "none", p = 5)
  
  fcst[i,1]        <- predict(M_RW,      n.ahead = nahead)$pred[nahead]  
  fcst[i,2]        <- predict(M_MM,      n.ahead = nahead)$pred[nahead]  
  fcst[i,3]        <- predict(M_AR,      n.ahead = nahead)$pred[nahead]
  fcst[i,4]        <- predict(M_ARMA,    n.ahead = nahead)$pred[nahead]
  fcst[i,5]        <- predict(M_VAR1,    n.ahead = nahead)$fcst$INFL[nahead, 1]
  fcst[i,6]        <- predict(M_VAR2,    n.ahead = nahead)$fcst$INFL[nahead, 1]  
  fcst[i,7]        <- predict(M_FAVAR1,  n.ahead = nahead)$fcst$INFL[nahead, 1]
  fcst[i,8]        <- predict(M_FAVAR2,  n.ahead = nahead)$fcst$INFL[nahead, 1]
}

pr <- Data.Frame[,1]
fcstErr <- cbind(pr,pr,pr,pr,pr,pr,pr,pr) - fcst
colnames(fcstErr) <- colnames(fcst)






## Forecast measures matrix
observed  <- Data.Frame[(init_p+nahead):nrow(Data.Frame),1]
fcst.sign = fcst

for (z in 1:noMods){
  for (i in 1:nrow(fcst)){
    if (sign(fcst[i,z]) == sign(observed[i])){
      fcst.sign[i,z] = 100
    }
    else if (sign(fcst[i,z])!=sign(observed[i])){
      fcst.sign[i,z] = 0
    }
  }
}

Bias = colMeans(fcstErr)*100
MSE  = colMeans(fcstErr^2)
MSFE = colMeans(sqrt(fcstErr^2))
Sign = colMeans(fcst.sign)
Std  = Sign

for (i in 1:8){
  Std[i] = sd(fcst[,i])
}

Std  = Std*100
dm   = cbind(Sign, 1:8)

colnames(dm) = c("DM test statistic", "DM p-value")

for (i in 1:8){
  if (i==1){
    dm[i,] = NA
  }
  else {
    dm[i,1] = dm.test(fcstErr[,1], fcstErr[,i], alternative = "g", h = 1, power = 1)$statistic
    dm[i,2] = dm.test(fcstErr[,1], fcstErr[,i], alternative = "g", h = 1, power = 1)$p.value
  }
}

ftab <- round(cbind(Std, Bias, MSE, MSFE, Sign, dm),4)
ftab
xtable(ftab)





fcst = fcst[-nrow(fcst),]
fcstErr = fcstErr[-nrow(fcstErr),]
colnames(fcst) <- colnames(fcstErr)

temp <- fcst %>% 
  as_tibble %>% 
  mutate(date = index(fcst)) %>% 
  gather(variable, value, -date) 

tempbg <- temp %>% rename("new"="variable")

temp %>% 
  ggplot(aes(date, value)) + 
  geom_line(data=tempbg, aes(group=new), alpha = 0.1, size = 0.3) + 
  geom_line(size = 0.3) + 
  #geom_line(data=INFL[-c(1:550),], aes(Date, Value)) +
  facet_wrap(~variable, nrow=2) + th + theme(axis.title = element_blank())



temp <- fcstErr %>% 
  as_tibble %>% 
  mutate(date = index(fcstErr)) %>% 
  gather(variable, value, -date) 

# tempbg <- temp %>% rename("new"="variable")
# 
# temp %>% 
#   ggplot(aes(date, value)) + 
#   geom_hline(aes(yintercept=0)) +
#   geom_line(data=tempbg, aes(group=new),color="grey") + 
#   geom_line() + 
#   facet_wrap(~variable, nrow=2) +
#   labs(title = "Forecast Errors", 
#        subtitle = "Expanding window, 1-period ahead",
#        color="Series",
#        caption = "Rasmus M. Jensen",
#        y = expression(epsilon[t+1*"|"*t]~"="~y[t+1]~"-"~f[t+1*"|"*t]))


temp %>% 
  ggplot(aes(date, abs(value))) + 
  geom_line(size = 0.3) + 
  geom_ribbon(aes(ymin=0, ymax=abs(value)), alpha = 0.1) +
  facet_wrap(~variable, nrow=2) + th + theme(axis.title = element_blank())







df %>% group_by(type, variable) %>% 
  mutate(cum = cumsum(value)) %>% 
  ungroup() %>% 
  ggplot(aes(N, cum, linetype = variable)) +
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size = 0.3) +
  facet_wrap(~type, nrow = 1) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_x_continuous("Lags (months)", limits = c(0,148), breaks = seq(0, 48, 8)) +
  labs(x = "Lags", linetype = "") +
  th + theme(axis.title.y = element_blank(), legend.position = "none")






















