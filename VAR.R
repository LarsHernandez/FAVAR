
# Script to calculate simple VAR model ------------------------------------
# 
# Lars, Andreas og Jens
# Redigeret: 26-03-19
# 
# 

# Packages ----------------------------------------------------------------

library(forecast)
library(tidyverse)
library(Quandl)
library(tseries)
library(vars)
library(xtable)
library(RColorBrewer)

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

FFR <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

CPI <- Quandl("FRED/CPIAUCSL", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

PROD <- Quandl("FRED/INDPRO", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)



# Model -------------------------------------------------------------------

# Ordering 1. Inflation, 2. Output, 3. Federal Funds Rate 
# Remember the first is most exogenious
pp <- data.frame(Date = FFR$Date[-c(1)],
                 INFL = diff(log(CPI$Value))*100,
                 PROD = diff(log(PROD$Value))*100,
                 FFR  = diff(FFR$Value))

# Decide number of lags in VAR model
VARselect(pp[,c(2:4)], lag.max = 24, type = c("const", "trend", "both", "none"))$selection

# Check for stationarity
rbind(adf.test(FFR$Value), adf.test(CPI$Value), adf.test(PROD$Value))
rbind(adf.test(pp$FFR),    adf.test(pp$INFL),   adf.test(pp$PROD))



pp$type <- "Transformeret"

tibble(Date = FFR$Date, FFR = FFR$Value, INFL = CPI$Value, PROD = PROD$Value) %>% 
  mutate(type = "Utransformeret") %>% 
  rbind(.,pp) %>% 
  mutate(type = factor(.$type, levels= c("Utransformeret", "Transformeret"))) %>% 
  gather(variable, value, -Date, -type) %>% 
  ggplot(aes(Date, value)) + 
  geom_line(size = 0.4) + 
  facet_grid(type~variable, scale="free") +
  labs(x = "", y = "") +
  th + theme(axis.title=element_blank())


# VAR(1) model ------------------------------------------------------------

model <- VAR(pp[,c(2:4)], p = 1, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))

stargazer::stargazer(model$varresult$FFR, 
                     model$varresult$INFL, 
                     model$varresult$PROD, 
                     title="VAR(1) summary statistics", 
                     no.space = TRUE, align=TRUE)


# VAR(13) model -----------------------------------------------------------

m <- VAR(pp[,c(2:4)], p = 13)

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
  scale_x_continuous("Lags", limits = c(0,48), breaks = seq(0, 48, 6)) +
  scale_y_continuous("Percent\n ", position = "right", limits = c(-0.4,1), breaks = c(-0.1,-0.05,0,0.05,0.10,0.15)) +
  facet_grid(Variable ~ impulse, switch = "y") +
  coord_cartesian(ylim = c(-0.1, 0.15)) +
  th



# Variance decomposition --------------------------------------------------

fe <- fevd(m, n.ahead = 12)

xtable(cbind(round(fe$INFL[c(1,4,8,12),],2)*100,
             round(fe$PROD[c(1,4,8,12),],2)*100,
             round(fe$FFR[c(1,4,8,12),],2)*100))





# Different orderings of variables ----------------------------------------

df <- tibble(R1 = as.vector(irf(VAR(pp[c(1,2,3,4)][,c(2:4)], p = 13), impulse = "FFR", response = "INFL", n.ahead = 48)$irf$FFR),
             R2 = as.vector(irf(VAR(pp[c(1,2,4,3)][,c(2:4)], p = 13), impulse = "FFR", response = "INFL", n.ahead = 48)$irf$FFR),
             R3 = as.vector(irf(VAR(pp[c(1,3,2,4)][,c(2:4)], p = 13), impulse = "FFR", response = "INFL", n.ahead = 48)$irf$FFR),
             R4 = as.vector(irf(VAR(pp[c(1,3,4,2)][,c(2:4)], p = 13), impulse = "FFR", response = "INFL", n.ahead = 48)$irf$FFR),
             R5 = as.vector(irf(VAR(pp[c(1,4,2,3)][,c(2:4)], p = 13), impulse = "FFR", response = "INFL", n.ahead = 48)$irf$FFR),
             R6 = as.vector(irf(VAR(pp[c(1,4,3,2)][,c(2:4)], p = 13), impulse = "FFR", response = "INFL", n.ahead = 48)$irf$FFR),
             N  = c(0:48))

df %>% gather(variable, Change, -N) %>% 
  group_by(variable) %>% 
  mutate(Accumulated = cumsum(Change)) %>% 
  gather(type, value, -N, -variable) %>% 
  ggplot(aes(N, value, linetype = variable)) + 
  facet_wrap(~type, scales = "free")+
  geom_line(size = 0.4) + 
  scale_x_continuous(breaks = seq(0,48, by = 6), limits = c(0, 48)) +
  labs(linetype = "Order", y = "", x = "Lags") +
  th + theme(axis.title.y=element_blank())






# Test of struktural shifts -----------------------------------------------



Martin    <- pp %>% filter(Date > "1951-01-01", Date < "1970-02-01") %>% dplyr::select(-Date, -type) %>% VAR(p = 2) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Burns     <- pp %>% filter(Date > "1970-02-01", Date < "1979-08-01") %>% dplyr::select(-Date, -type) %>% VAR(p = 2) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Volcker   <- pp %>% filter(Date > "1979-08-01", Date < "1987-08-01") %>% dplyr::select(-Date, -type) %>% VAR(p = 2) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Greenspan <- pp %>% filter(Date > "1987-08-01", Date < "2006-01-01") %>% dplyr::select(-Date, -type) %>% VAR(p = 2) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Bernanke  <- pp %>% filter(Date > "2006-01-01", Date < "2014-01-01") %>% dplyr::select(-Date, -type) %>% VAR(p = 2) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)
Yellen    <- pp %>% filter(Date > "2014-01-01", Date < "2018-02-01") %>% dplyr::select(-Date, -type) %>% VAR(p = 2) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 24)



df <- rbind(tibble(IRF = Martin$irf$FFR,    Lower = Martin$Lower$FFR,    Upper = Martin$Upper$FFR,    N = c(0:24), type = "Martin\n1951 - 1970"),
            tibble(IRF = Burns$irf$FFR,     Lower = Burns$Lower$FFR,     Upper = Burns$Upper$FFR,     N = c(0:24), type = "Burns\n1970 - 1978"),
            tibble(IRF = Volcker$irf$FFR,   Lower = Volcker$Lower$FFR,   Upper = Volcker$Upper$FFR,   N = c(0:24), type = "Volcker\n1979 - 1987"),
            tibble(IRF = Greenspan$irf$FFR, Lower = Greenspan$Lower$FFR, Upper = Greenspan$Upper$FFR, N = c(0:24), type = "Greenspan\n1987 - 2006"),
            tibble(IRF = Bernanke$irf$FFR,  Lower = Bernanke$Lower$FFR,  Upper = Bernanke$Upper$FFR,  N = c(0:24), type = "Bernanke\n2006 - 2014"),
            tibble(IRF = Yellen$irf$FFR,    Lower = Yellen$Lower$FFR,    Upper = Yellen$Upper$FFR,    N = c(0:24), type = "Yellen\n2014 - 2018")) %>% 
  gather(variable, value, -type, -N)

df$type <- factor(df$type, levels = c("Martin\n1951 - 1970", "Burns\n1970 - 1978", "Volcker\n1979 - 1987","Greenspan\n1987 - 2006","Bernanke\n2006 - 2014","Yellen\n2014 - 2018"))

df %>% ggplot(aes(N, value, linetype = variable)) +
  geom_line()+
  facet_wrap(~type) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  geom_hline(aes(yintercept = 0), size = 0.5, alpha = 0.5) +
  scale_y_continuous(breaks = c(0.10,0.05,0,-0.05,-0.10), limits = c(-0.19, 0.19)) +
  #labs(title = expression(paste(bold("Figur 4.7  "), "Strukturelle skift 1960 - 2018 (INFL, PROD, FFR)")), 
   #    color = "Procent", y = "", x = "Lags", caption = "Kilde: FRED + Egne beregninger", linetype = "") +
  coord_cartesian(ylim = c(-0.12, 0.12)) +
  labs(y="", x="Lags", linetype = "") +
  th + theme(axis.title.y=element_blank(), legend.position = "none")





# Forecasting -------------------------------------------------------------

actual <- pp %>% 
  filter(Date >= "2018-01-01") %>% 
  mutate(TYPE = "Actual") %>% 
  dplyr::select(-type)

dat <- pp %>% filter(Date < "2018-01-01")


m <- auto.arima(dat$INFL)
infl.arima <- predict(m, n.ahead = 12)

m <- auto.arima(dat$PROD)
prod.arima <- predict(m, n.ahead = 12)

m <- auto.arima(dat$FFR)
ffr.arima <- predict(m, n.ahead = 12)


arima <- tibble(Date = actual$Date,
                FFR = ffr.arima$pred,
                INFL = infl.arima$pred,
                PROD = prod.arima$pred, 
                TYPE = "ARIMA")

m <- pp %>% 
  filter(Date < "2018-01-01") %>% 
  dplyr::select(-Date, -type) %>% 
  VAR(p = 13)

pred <- predict(m, n.ahead = 12)

prediction <- tibble(Date = actual$Date,
                     INFL = pred$fcst$INFL[,1],
                     FFR = pred$fcst$FFR[,1],
                     PROD = pred$fcst$PROD[,1], 
                     TYPE = "VAR")


rbind(actual, prediction, arima) %>% 
  gather(variable, value, -Date, -TYPE) %>% 
  ggplot(aes(Date, value, linetype=TYPE)) +
  geom_line() +
  facet_wrap(~variable) + 
  labs(linetype="Model") +
  th + theme(axis.title=element_blank())



# RMSE --------------------------------------------------------------------

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

RMSE(pred$fcst$INFL[,1], actual$INFL)
RMSE(infl.arima$pred, actual$INFL)

RMSE(pred$fcst$FFR[,1], actual$FFR)
RMSE(ffr.arima$pred, actual$FFR)

RMSE(pred$fcst$PROD[,1], actual$PROD)
RMSE(prod.arima$pred, actual$PROD)



cor(FAVAR_T$IPMANSICS, FAVAR_PCA$x[,1])
