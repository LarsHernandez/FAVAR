
library(Quandl)
library(tidyverse)
library(plotly)
library(lubridate)
library(gganimate)


ZERO <- Quandl("FED/SVENY")

ZERO_S <- ZERO %>% 
  filter(Date > "2000-01-01") %>% 
  subset(day(Date) == 1) 

ZERO_M <- ZERO_S %>% 
  dplyr::select(-Date) %>% 
  as.matrix()

#plot_ly(z = ~ ZERO_plot, colors = "Greys") %>% add_surface()
#par(mar=c(2, 2, 2, 2))


# Perspective plot --------------------------------------------------------

matur <- c(1:30)
date <- rev(decimal_date(ZERO_S$Date))
mat <- apply(ZERO_M,2,rev)

persp(x = date, y = matur, z = mat, 
      theta=40, phi=25, expand = 0.4,
      ticktype="detailed", 
      ylab="Løbetid", xlab = "", zlab = "Rente")



# Animation yield curve ---------------------------------------------------

ZERO_S <- colnames(ZERO_S) 

ZERO_S %>% filter(Date == "2019-04-01") %>% 
  gather(variable, value, -Date) %>% 
  ggplot(aes(variable, value)) + 
  geom_point()
















