


# Libraries ---------------------------------------------------------------

library(Quandl)
library(tibbletime)
library(lubridate)
library(vars)
library(tseries)
library(tidyverse)
library(gridExtra)

options(warn=-1)

# Load data from Quandl ---------------------------------------------------

RATE <- Quandl("FRED/FEDFUNDS") %>% 
  filter(month(Date) %in% c(1,4,7,10)) %>% 
  filter(Date < "2018-01-01" & Date > "1970-01-01") %>% 
  mutate(quarter = paste(year(Date), month(Date)))

INFL <- Quandl("RATEINF/INFLATION_USA") %>% 
  filter(month(Date) %in% c(1,4,7,10)) %>% 
  filter(Date < "2018-01-01" & Date > "1970-02-01") %>% 
  mutate(quarter = paste(year(Date), month(Date)))

NGDP <- Quandl("FRED/GDP") %>% 
  dplyr::filter(Date < "2018-01-01" & Date > "1970-01-01") %>% 
  arrange(Date) %>% 
  mutate(dvalue = c(NA, diff(log(Value))*100)) %>% 
  arrange(desc(Date)) %>% 
  mutate(quarter = paste(year(Date), month(Date)))




# VAR model ---------------------------------------------------------------

df <- full_join(INFL, RATE, by="quarter")
df <- full_join(df, NGDP, by="quarter")
df <- df[,c(6,2,5,8)]
df <- df[-191,]
colnames(df) <- c("Date", "INFL", "RATE", "NGDP")


rbind(adf.test(df$RATE), adf.test(df$INFL), adf.test(df$NGDP))



p1 <- df1 %>% gather(variable, value, -Date) %>% ggplot(aes(Date, value, linetype=variable)) + 
   geom_line(color="#045a8d") + 
   labs(title=chart_title, x="", y="")
p1



m <- VAR(df[,-1], p = 8)

plot(irf(m, impulse = "RATE", response = "INFL"))



data <- irf(m, nsteps = 12, n.ahead=21)
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


p2 <- ggplot(ir, aes(x = t, y = Value, group = Variable))  +
   geom_line(color="#045a8d") +
   geom_ribbon(aes(x=t, ymax=Upper, ymin=Lower), alpha=0.15, fill="#045a8d") +
   scale_x_continuous(limits = c(1,21), breaks = seq(1,21,4)) +
   scale_y_continuous("", position="right") +
   labs(title="Impulse Response Functions", x="") +
   facet_grid(Variable ~ impulse, switch="y")
p2


grid.arrange(p1,p2, nrow=1)
