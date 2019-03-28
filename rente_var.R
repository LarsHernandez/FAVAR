
library(Quandl)
library(tidyverse)
library(lubridate)



# Loading data ------------------------------------------------------------

ZERO <- Quandl("FED/SVENY")

ZERO_Y <- ZERO %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% 
  summarise(avg = mean(SVENY05))

CPI <- Quandl("FRED/DDOE01USA086NWDB") 

INFL_Y <- CPI %>% 
  arrange(Date) %>% 
  mutate(Value = c(NA, diff(log(.$Value))*100),
         year=year(Date))

UNEMP <- Quandl("FRED/UNRATE")

UNEMP_Y <- UNEMP %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% 
  summarise(avg = mean(Value))

# Plotting Zero cupon bond yield ------------------------------------------

ZERO[,c(1,2)] %>% 
  ggplot(aes(Date, SVENY01)) +
  geom_line()

ZERO %>% 
  gather(variable, value, -Date) %>% 
  ggplot(aes(Date, value, color=variable)) +
  geom_line()



ZERO %>% 
  gather(variable, value, -Date) %>% 
  filter(Date > "2010-01-01" & Date <"2019-02-08") %>% 
  ggplot(aes(variable, value, color=Date)) +
  geom_jitter(size=0.4)


ZERO %>% 
  select(SVENY03, SVENY05, Date) %>% 
  gather(variable, value, -Date) %>% 
  ggplot(aes(Date, value, color=variable)) +
  geom_line()



# Plotting inflation ------------------------------------------------------

ggplot(CPI, aes(Date, Value)) +
  geom_line()

ggplot(CPI, aes(Date, INF)) +
  geom_line()







# Correlation matrix -----------------------------------------------------

mat <- cor(ZERO[,-1][c(1:8000),])

new <- mat %>% 
  as.tibble %>% 
  rownames_to_column('Var1') %>% 
  gather(Var2, value, -Var1) %>% 
  mutate(Var1 = factor(Var1, levels = 1:30))

ggplot(new, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = round(value, 2)))















CPI$year <- year(CPI$Date)
bb <- left_join(ZERO_yearly, CPI, by="year")


cor(bb$avg[1:56], bb$INF[1:56])











