


Quandl("FRED/DFF") %>% mutate(year = year(Date),
                                      month = month(Date)) %>% 
  group_by(year, month) %>% 
  summarize(tot = mean(Value),
            first = first(Date)) %>%
  filter(first > "1980-01-01") %>% 
  ggplot(aes(first, tot)) +
  geom_rect(data = recessions.t, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill="grey", alpha=0.6, inherit.aes = F) +
  geom_step() +
  labs(title = "Effective Federal Funds Rate", subtitle="From january 1980 - february 2019", x="", y="Interest rate", caption = "Source: Federal Reserve")
  



data <- ZERO %>% 
  filter(Date > "1980-01-01") %>% 
  mutate("Term structure" = SVENY05 - SVENY03) %>% 
  dplyr::select("Term structure", Date) %>% 
  left_join(UNEMP, by="Date")

data$Unemployment <- na.fill(data$Value, "extend")
data$Value <- NULL

data %>% 
  gather(variable, value, -Date) %>% 
  ggplot(aes(Date, value)) +
  facet_wrap(~variable, nrow=2, scales = "free") +
  geom_rect(data = recessions.t, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill="grey", alpha=0.6, inherit.aes = F) +
  geom_line() +
  labs(title = "Spread between 1yr and 5yr zero cupon bond yield of US govenrment bonds + US unemployment",
       subtitle = "Grey zones mark official US recessions", x="", y="", caption = "Source: Federal Reserve")



df <- data.frame("2019" = as.numeric(as.vector(ZERO[1,-1])),
                 "2015" = as.numeric(as.vector(ZERO[1030,-1])),
                 "2010" = as.numeric(as.vector(ZERO[2281,-1])))
df$n <- c(1:30)

df %>% 
  gather(variable, value, -n) %>% 
  ggplot(aes(n, value, linetype = variable)) +
  geom_line() +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30)) +
  labs(title = "Yield curves for US zero cupon bonds", 
       subtitle = "X2019 is from 15th of february, the others 1st of january",
       x="Maturity in years", y="Yield in %", caption = "Source: Federal Reserve", linetype = "Yield curve")
  
