
library(forecast)
library(tidyverse)
library(Quandl)
library(tseries)
library(vars)
#devtools::install_github("mbalcilar/mFilter")
library(mFilter)

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

# Data 1959-2019 ----------------------------------------------------------
FFR <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  dplyr::select(Value)

CPI <- Quandl("FRED/CPIAUCSL", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  dplyr::select(Value)

PROD <- Quandl("FRED/INDPRO", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  dplyr::select(Value)


# Extra data --------------------------------------------------------------
COM <- Quandl("FRED/PPIIDC", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  dplyr::select(Value)

bond <- Quandl("FED/SVENY", collapse = "m", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

SPREAD <- data.frame(Value = c(rnorm(28, mean = 0.4,sd = 0.1),bond$SVENY05 - bond$SVENY02))

DOW <- read_delim("//student.aau.dk/Users/lbni13/Desktop/DOW2.csv", ";", 
                  escape_double = FALSE, locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE) %>% 
  rename(Value = `US DOW JONES INDUSTRIALS SHARE PRICE INDEX (EP) NADJ`) %>% 
  dplyr::select(Value)

#DOW <- read_delim("//student.aau.dk/Users/lbni13/Desktop/DOW2.csv", ";", 
#                  escape_double = FALSE, locale = locale(decimal_mark = ","), 
#                  trim_ws = TRUE) %>% 
#  rename(Value = `US DOW JONES INDUSTRIALS SHARE PRICE INDEX (EP) NADJ`) %>% 
#  dplyr::select(Value)

gap <- PROD %>% 
  ts(start = 1959) %>% 
  log() %>% 
  hpfilter(freq=129600)

GAP <- data.frame(Value = as.numeric(gap$cycle))


SHA <- Quandl("SHADOWS/US",api_key = key) %>% 
  filter(Date >= "2000-01-01", Date < "2016-01-01") %>% 
  arrange(Date) %>% 
  dplyr::select(`Policy Rate`)



# Plot --------------------------------------------------------------------

p1 <- data.frame(DOW     = DOW$Value[-1], 
                 GAP     = GAP$Value[-1], 
                 SPREAD  = SPREAD$Value, 
                 COM     = COM$Value[-1],
                 Date    = seq(as.Date("1959-02-01"), by = "month", length.out = 719)) %>% 
  gather(variable, value, -Date) %>% 
  ggplot(aes(Date,value)) + 
  geom_line(size = 0.3) + 
  facet_wrap(~variable, nrow = 1, scales = "free") + 
  scale_x_date(limits = as.Date(c('1960-01-01','2021-01-01'))) +
  th  + theme(axis.title = element_blank())

ggsave(plot = p1, filename = "GENERATE/COMP1.pdf", width = 24, height = 6, units = "cm", dpi = 320)





# Stationæritet-test  ----------------------------------------------------
adf.test(FFR$Value)
adf.test(CPI$Value)
adf.test(PROD$Value)
adf.test(DOW$Value)
adf.test(COM$Value)
adf.test(SPREAD$Value)
adf.test(GAP$Value)

dFFR   <- data.frame(FFR  = diff(FFR$Value))
dCPI   <- data.frame(CPI  = diff(log(CPI$Value))*100)
dPROD  <- data.frame(PROD = diff(log(PROD$Value))*100)
dDOW   <- data.frame(DOW  = diff(log(DOW$Value))*100)
dCOM   <- data.frame(COM  = diff(log(COM$Value))*100)
SPREAD <- data.frame(SPREAD = SPREAD$Value)
dSHA    <- data.frame(SHA = diff(SHA))

# Datasæt til model-------------------------------------------------------
Data0 <- data.frame(dCPI,          dPROD, dFFR) #Grundmodel
Data1 <- data.frame(dCPI, dCOM,    dPROD, dFFR) #Grundmodel + commodity
Data2 <- data.frame(dCPI, dDOW,    dPROD, dFFR) #Grundmodel + aktie (D&J)
Data3 <- data.frame(dCPI, GAP = GAP[-1,], dFFR) #Grundmodel + outputgap
Data4 <- data.frame(dCPI, SPREAD,  dPROD, dFFR) #Grundmodel + Spread

# Modeller  --------------------------------------------------------------
V0 <- VAR(Data0, p = 13)
V1 <- VAR(Data1, p = 13)
V2 <- VAR(Data2, p = 13)
V3 <- VAR(Data3, p = 13)
V4 <- VAR(Data4, p = 13)


irf0 <- irf(V0, impulse = "FFR", response = "CPI", ortho = T, cumulative = F, n.ahead = 48, ci = 0.66)
irf1 <- irf(V1, impulse = "FFR", response = "CPI", ortho = T, cumulative = F, n.ahead = 48, ci = 0.66)
irf2 <- irf(V2, impulse = "FFR", response = "CPI", ortho = T, cumulative = F, n.ahead = 48, ci = 0.66)
irf3 <- irf(V3, impulse = "FFR", response = "CPI", ortho = T, cumulative = F, n.ahead = 48, ci = 0.66)
irf4 <- irf(V4, impulse = "FFR", response = "CPI", ortho = T, cumulative = F, n.ahead = 48, ci = 0.66)


# Plot af modeller --------------------------------------------------------

d0 <- rbind(irf0$irf$FFR   %>% as_tibble %>% mutate(type = "irf", N = 1:49),
      irf0$Upper$FFR %>% as_tibble %>% mutate(type = "upper", N = 1:49),
      irf0$Lower$FFR %>% as_tibble %>% mutate(type = "lower", N = 1:49)) %>% as_tibble %>% mutate(model = "Benchmark Model")


d1 <- rbind(irf1$irf$FFR   %>% as_tibble %>% mutate(type = "irf", N = 1:49),
            irf1$Upper$FFR %>% as_tibble %>% mutate(type = "upper", N = 1:49),
            irf1$Lower$FFR %>% as_tibble %>% mutate(type = "lower", N = 1:49)) %>% as_tibble %>% mutate(model = "Commoditie Prices")


p2 <- rbind(d1,d0) %>% 
  ggplot(aes(N, CPI, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size = 0.3) + 
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_y_continuous(limits=c(-0.02,0.06)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 8)) +
  facet_wrap(~model) + 
  th + theme(axis.title.y = element_blank(), legend.position = "none")

ggsave(plot = p2, filename = "GENERATE/COMP2.pdf", width = 24, height = 6, units = "cm", dpi = 320)



d2 <- rbind(irf2$irf$FFR   %>% as_tibble %>% mutate(type = "irf", N = 1:49),
            irf2$Upper$FFR %>% as_tibble %>% mutate(type = "upper", N = 1:49),
            irf2$Lower$FFR %>% as_tibble %>% mutate(type = "lower", N = 1:49)) %>% as_tibble %>% mutate(model = "Dow Jones")


p3 <- rbind(d2,d0) %>% 
  ggplot(aes(N, CPI, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size = 0.3) + 
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_y_continuous(limits=c(-0.02,0.06)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 8)) +
  facet_wrap(~model) + 
  th + theme(axis.title.y = element_blank(), legend.position = "none")

ggsave(plot = p3, filename = "GENERATE/COMP3.pdf", width = 24, height = 6, units = "cm", dpi = 320)



d3 <- rbind(irf3$irf$FFR   %>% as_tibble %>% mutate(type = "irf", N = 1:49),
            irf3$Upper$FFR %>% as_tibble %>% mutate(type = "upper", N = 1:49),
            irf3$Lower$FFR %>% as_tibble %>% mutate(type = "lower", N = 1:49)) %>% as_tibble %>% mutate(model = "Output Gap")


p4 <- rbind(d3,d0) %>% 
  ggplot(aes(N, CPI, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size = 0.3) + 
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_y_continuous(limits=c(-0.02,0.06)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 8)) +
  facet_wrap(~model) + 
  th + theme(axis.title.y = element_blank(), legend.position = "none")

ggsave(plot = p4, filename = "GENERATE/COMP4.pdf", width = 24, height = 6, units = "cm", dpi = 320)



d4 <- rbind(irf4$irf$FFR   %>% as_tibble %>% mutate(type = "irf", N = 1:49),
            irf4$Upper$FFR %>% as_tibble %>% mutate(type = "upper", N = 1:49),
            irf4$Lower$FFR %>% as_tibble %>% mutate(type = "lower", N = 1:49)) %>% as_tibble %>% mutate(model = "Yield Curve Spread")


p5 <- rbind(d4,d0) %>% 
  ggplot(aes(N, CPI, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size = 0.3) + 
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_y_continuous(limits=c(-0.02,0.06)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 8)) +
  facet_wrap(~model) + 
  th + theme(axis.title.y = element_blank(), legend.position = "none")

ggsave(plot = p5, filename = "GENERATE/COMP5.pdf", width = 24, height = 6, units = "cm", dpi = 320)





# GGPLOT ------------------------------------------------------------------

FFR2 <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "2000-01-01", Date < "2015-12-01") %>% 
  arrange(Date)

SHA2 <- Quandl("SHADOWS/US", api_key = key) %>% 
  filter(Date >= "2000-01-01", Date < "2016-01-01") %>% 
  arrange(Date)

CPI2 <- Quandl("FRED/CPIAUCSL", api_key = key) %>% 
  filter(Date >= "2000-01-01", Date < "2015-12-01") %>% 
  arrange(Date)

PROD2 <- Quandl("FRED/INDPRO", api_key = key) %>% 
  filter(Date >= "2000-01-01", Date < "2015-12-01") %>% 
  arrange(Date)

FFR2 <- FFR2$Value
SHA2 <- SHA2$`Policy Rate`
CPI2 <- CPI2$Value
PROD2 <- PROD2$Value

dFFR2 <- diff(FFR2)
dSHA2 <- diff(SHA2)
dCPI2 <- diff(log(CPI2))*100
dPROD2 <- diff(log(PROD2))*100

Data0 <- cbind(dCPI2,dPROD2,dFFR2) #Grundmodel
Data1 <- cbind(dCPI2,dPROD2,dSHA2) #Negativ model


V <- VAR(Data0, p=3)
irf_FFR <- irf(V, impulse = "dFFR2", response = "dCPI2", ortho = T, cumulative = F, n.ahead = 24, ci=0.66)

V1 <- VAR(Data1, p=3)
irf_SHA <- irf(V1, impulse = "dSHA2", response = "dCPI2", ortho = T, cumulative = F, n.ahead = 24, ci=0.66)



SHA2a <- SHA2 %>% as_tibble %>% mutate(type="SHA", n=seq(from=as.Date("2000-01-01"), to =as.Date("2015-11-01"), by="months"))
FFR2a <- FFR2 %>% as_tibble %>% mutate(type="FFR", n=seq(from=as.Date("2000-01-01"), to =as.Date("2015-11-01"), by="months"))
negativ <- rbind(FFR2a,SHA2a)

p6 <- negativ %>% 
  as_tibble %>% 
  ggplot(aes(n, value, linetype=type)) + 
  geom_line(size = 0.3) + 
  geom_hline(aes(yintercept=0),linetype="solid", color="grey") + 
  scale_linetype_manual(labels=c("Federal Funds\nRate", "Shaddow Rate"), values=c("solid", "dotted"), name="Type") +
  th + theme(axis.title = element_blank())

ggsave(plot = p6, filename = "GENERATE/COMP6.pdf", width = 24, height = 6, units = "cm", dpi = 320)



irf_FFR1 <- irf_FFR$irf$dFFR
irf_FFR2 <- irf_FFR$Lower$dFFR
irf_FFR3 <- irf_FFR$Upper$dFFR

samligning_FFR <- cbind(irf_FFR1,irf_FFR2,irf_FFR3) %>% as_tibble  %>% mutate(type="Federal Funds Rate") %>% mutate(n=c(1:25))
samligning1 <- samligning_FFR %>% gather(variable, value, -type)

irf_SHA1 <- irf_SHA$irf$dSHA
irf_SHA2 <- irf_SHA$Lower$dSHA
irf_SHA3 <- irf_SHA$Upper$dSHA

samligning_SHA <- cbind(irf_SHA1,irf_SHA2,irf_SHA3) %>% as_tibble  %>% mutate(type="Shadow Rate") %>% mutate(n=c(1:25))
samligning2 <- samligning_SHA %>% gather(variable, value, -type)


samligning <- rbind(samligning_FFR,samligning_SHA)

p7 <- samligning %>% gather(variable, value, -type, -n) %>% 
  ggplot(aes(n-1, value, linetype=variable))+ 
  geom_hline(aes(yintercept=0), linetype="solid", color="grey") + 
  geom_line(size=0.3, show.legend = FALSE) + 
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_x_continuous("Lags (months)", limits = c(0,24), breaks = seq(0, 24, 2)) +
  facet_wrap(~type) + 
  th  + theme(axis.title = element_blank())

ggsave(plot = p7, filename = "GENERATE/COMP7.pdf", width = 24, height = 6, units = "cm", dpi = 320)





# IRF total ---------------------------------------------------------------

data <- irf(V4, n.ahead = 49, cumulative = F, ci = 0.66)
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

ir$Variable <- fct_relevel(ir$Variable, "INFL", "PROD")
ir$impulse <- fct_relevel(ir$impulse, "Shock to INFL", "Shock to PROD")

p8 <- ggplot(ir, aes(x = t, y = Value, group = Variable))  +
  geom_hline(aes(yintercept=0),linetype="solid", color="grey") +
  geom_line(size = 0.3) +
  geom_line(aes(x = t, y = Upper), linetype = "dotted", size = 0.3) +
  geom_line(aes(x = t, y = Lower), linetype = "dotted", size = 0.3) +
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 6)) +
  scale_y_continuous("Percent\n ", position = "right", limits = c(-0.4,1), breaks = c(-0.06,0,0.06,0.12)) +
  facet_grid(Variable ~ impulse, switch = "y") +
  coord_cartesian(ylim = c(-0.08, 0.14)) +
  th

ggsave(plot = p8, filename = "GENERATE/COMP8.pdf", width = 24, height = 14, units = "cm", dpi = 320)







