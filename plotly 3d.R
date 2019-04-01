
library(Quandl)
library(tidyverse)
library(plotly)
library(lubridate)

ZERO <- Quandl("FED/SVENY")

ZERO_plot1 <- ZERO %>% filter(Date > "2000-01-01") %>% subset(month(Date) == 1 & day(Date) == 1)

aa <- subset(ZERO_plot1, month(ZERO_plot1$Date) == 1 | day(ZERO_plot1$Date) == 1)
bb <- aa %>% select(-Date) %>% as.matrix()

ZERO_plot2 <- ZERO_plot1 %>% select(-Date) %>% as.matrix()




#plot_ly(z = ~ ZERO_plot, colors = "Greys") %>% add_surface()






par(mar=c(2, 2, 2, 2))


matur <- c(1:30)
date <- decimal_date(ZERO_plot1$Date)
date2 <- decimal_date(aa$Date)

mat <- apply(ZERO_plot2,2,rev)
mat2 <- apply(bb,2,rev)

persp(rev(date2), matur, mat2, 
      theta=40, phi=25, expand = 0.6, r=2000,
      ticktype="detailed", ylab="Maturity", xlab = "Dato", zlab = "Effektiv rente")


