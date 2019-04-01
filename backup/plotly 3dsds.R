
library(Quandl)
library(tidyverse)
library(plotly)

ZERO <- Quandl("FED/SVENY")

ZERO_plot <- ZERO %>% filter(Date > "2000-01-01") %>% select(-Date) %>% as.matrix()

plot_ly(z = ~ ZERO_plot) %>% add_surface()
