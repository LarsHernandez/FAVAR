

# -------------------------------------------------------------------------
# Replication ofBernanke, Boivin and Eliasz (2003) ------------------------
# -------------------------------------------------------------------------
#
#
# 2 step approach
# 05-04-2019 Lars Nielsen
#
#
# -------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

library(tidyverse)
library(vars)


# Load data ---------------------------------------------------------------

nsbalpanel <- read_table2("Bernanke data/2step/nsbalpanel.txt", col_names = FALSE)

# Scale variables
ts <- nsbalpanel %>% mutate_all(.funs = scale) 

# Naming and order from Bernanke codes
name <- c('IP','CPI', "FFR",'3m TREASURY BILLS','5y TREASURYBONDS','MONETARY BASE','M2','EXCHANGE RATE YEN',
          'COMMODITY PRICE INDEX','CAPACITY UTIL RATE','PERSONAL CONSUMPTION','DURABLE CONS','NONDURABLE CONS',
          'UNEMPLOYMENT','EMPLOYMENT','AVG HOURLY EARNINGS','HOUSING STARTS','NEW ORDERS','DIVIDENDS','CONSUMER EXPECTATIONS')

special <- c(16,107,77,78,81,96,93,75,102,18,50,51,52,27,49,118,55,63,72,120)



colnames(ts)[special] <- name



# Factors -----------------------------------------------------------------

Fak <- prcomp(ts[-77], rank=5)$x

#irf(VAR(cbind(ts[77], Fak), p = 13), impulse = "FFR", response = "FFR",n.ahead = 48)


obj <- irf(VAR(cbind(ts[77], Fak), p = 13), impulse = "FFR", n.ahead = 48)
eigen <- prcomp(ts[-77], rank=5)$rotation

val <- obj[1]$irf$FFR[,-1] %*% t(eigen) %>% 
  cbind(.,obj[1]$irf$FFR[,1]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$FFR[,-1] %*% t(eigen) %>% 
  cbind(.,obj[2]$Lower$FFR[,1]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$FFR[,-1] %*% t(eigen) %>% 
  cbind(.,obj[3]$Upper$FFR[,1]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)



trans_1 <- c('3m TREASURY BILLS','5y TREASURYBONDS','COMMODITY PRICE INDEX','CAPACITY UTIL RATE','UNEMPLOYMENT','EMPLOYMENT','NEW ORDERS','DIVIDENDS','CONSUMER EXPECTATIONS')
trans_4 <- c('HOUSING STARTS')
trans_5 <- c('IP','CPI','MONETARY BASE','M2','EXCHANGE RATE YEN','PERSONAL CONSUMPTION','DURABLE CONS','NONDURABLE CONS','AVG HOURLY EARNINGS')


FFR <- val %>% full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  gather(type, value, -variable,-N) %>% 
  filter(variable == "V120")

pl_1 <- val %>% full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  gather(type, value, -variable,-N) %>% 
  filter(variable %in% trans_1)


pl_4 <- val %>% full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  gather(type, value, -variable,-N) %>% 
  filter(variable %in% trans_4) %>% 
  group_by(variable, type) %>% 
  mutate_at(.vars="value", .funs= function(x) exp(x)-1) %>% 
  ungroup()


pl_5 <- val %>% full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  gather(type, value, -variable,-N) %>% 
  filter(variable %in% trans_5) %>% 
  group_by(variable, type) %>% 
  mutate_at(.vars="value", .funs= function(x) exp(cumsum(x))-1) %>% 
  ungroup()



pl <- rbind(pl_1, pl_4, pl_5,FFR)


pl %>% 
  mutate(variable = factor(variable, levels = c("V120",'IP','CPI','3m TREASURY BILLS','5y TREASURYBONDS','MONETARY BASE','M2','EXCHANGE RATE YEN',
                                                'COMMODITY PRICE INDEX','CAPACITY UTIL RATE','PERSONAL CONSUMPTION','DURABLE CONS','NONDURABLE CONS',
                                                'UNEMPLOYMENT','EMPLOYMENT','AVG HOURLY EARNINGS','HOUSING STARTS','NEW ORDERS','DIVIDENDS',
                                                'CONSUMER EXPECTATIONS'))) %>% 
  mutate(variable = fct_recode(variable, "FFR" = "V120")) %>% 
  ggplot(aes(N, value, linetype=type)) + 
  scale_linetype_manual(values = c("dotted", "solid", "dotted")) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line() + 
  facet_wrap(~variable, scales = "free")













# 
# pl <- val %>% full_join(.,upp, by=c("N", "variable")) %>% 
#   full_join(., low, by=c("N", "variable")) %>%
#   gather(type, value, -variable,-N) %>% 
#   mutate(variable = factor(variable, levels = c("V120",'IP','CPI','3m TREASURY BILLS','5y TREASURYBONDS','MONETARY BASE','M2','EXCHANGE RATE YEN',
#                                                 'COMMODITY PRICE INDEX','CAPACITY UTIL RATE','PERSONAL CONSUMPTION','DURABLE CONS','NONDURABLE CONS',
#                                                 'UNEMPLOYMENT','EMPLOYMENT','AVG HOURLY EARNINGS','HOUSING STARTS','NEW ORDERS','DIVIDENDS',
#                                                 'CONSUMER EXPECTATIONS'))) %>% 
#   mutate(variable = fct_recode(variable, "FFR" = "V120"))
# 
# 
# pl %>% 
#   group_by(variable, type) %>% 
#   filter(variable == "FFR") %>% 
#   mutate_at(.vars="value", .funs = function(x) exp(cumsum(x))) %>% 
#   ggplot(aes(N, value, linetype=type)) + 
#   scale_linetype_manual(values = c("dotted", "solid", "dotted")) + 
#   geom_hline(aes(yintercept = 0), color="grey") +
#   geom_line() + 
#   facet_wrap(~variable, scales = "free")
# 


