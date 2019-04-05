


library(tidyverse)
library(vars)


nsbalpanel <- read_table2("C:/Users/larsn/Desktop/BBE/BBE_Ddisk/2step/nsbalpanel.txt", 
                          col_names = FALSE)

ts <- nsbalpanel %>% mutate_all(.funs = scale) 


name <-   c('IP','CPI', "FFR",'3m TREASURY BILLS','5y TREASURYBONDS','MONETARY BASE','M2','EXCHANGE RATE YEN',
            'COMMODITY PRICE INDEX','CAPACITY UTIL RATE','PERSONAL CONSUMPTION','DURABLE CONS','NONDURABLE CONS',
            'UNEMPLOYMENT','EMPLOYMENT','AVG HOURLY EARNINGS','HOUSING STARTS','NEW ORDERS','DIVIDENDS','CONSUMER EXPECTATIONS')

special <- c(16,107,77,78,81,96,93,75,102,18,50,51,52,27,49,118,55,63,72,120)

colnames(ts)[special] <- name

Fak <- prcomp(ts[-77], rank=3)$x



irf(VAR(cbind(ts[77], Fak), p = 13), impulse = "FFR", response = "FFR",n.ahead = 48)





obj <- irf(VAR(cbind(ts[77], Fak), p = 13), impulse = "FFR",n.ahead = 48)
eigen <- prcomp(ts[-77], rank=3)$rotation

val <- obj[1]$irf$FFR[,-1] %*% t(eigen) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$FFR[,-1] %*% t(eigen) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$FFR[,-1] %*% t(eigen) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)





val %>% full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  gather(type, value, -variable,-N) %>% 
  ggplot(aes(N, value, linetype=type)) + 
    scale_linetype_manual(values = c("dotted", "solid", "dotted")) + 
    geom_line() + 
    facet_wrap(~variable)



