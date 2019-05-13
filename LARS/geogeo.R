library(scales)
library(sf)
library(tidyverse)
library(statsDK)

geo_sf <- read_sf("https://dawa.aws.dk/Sogne/?format=geojson")

KM5 <- retrieve_data("KM5", Tid = "2019", ALDER = "*", FKMED = "*", SOGN = "*")
KM5s <- KM5 %>% 
  group_by(SOGN, KØN) %>% 
  summarize(tot = sum(INDHOLD))
KM5ss <- KM5s %>% 
  group_by(SOGN) %>% 
  mutate(tott = sum(tot),
         pct = tot/tott)
KM5ss$kode <- str_extract(KM5ss$SOGN, "[:digit:]+")



KM5ss$scale <- rescale(KM5ss$tott,to=c(0,1))
KM5ss$scale <- KM5ss$scale + 0.5 

KM5ss$scale <- ifelse(KM5ss$scale > 1,1,KM5ss$scale)

KM5ss %>% 
  filter(KØN == "Women") %>% 
  left_join(geo_sf, ., by = c('kode' = 'kode')) %>% 
  ggplot() + 
  geom_sf(aes(fill = pct), color="white") +  
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       values = rescale(c(0.4,0.45,0.5,0.55,0.6)), 
                       limits = c(0.4,0.6)) +
  labs(title="Denmark - average age by parish", 
       caption = "Source: Danmarks Statistik + DAWA", 
       fill = "Age", 
       color = "Age",
       subtitle = "As of 2019 there are 2158 parishes with an average age of 43.2 years") +
  theme(title = element_text(colour = "#404040", size = 18),
        plot.background=element_rect(fill = "white", color = "white"),  
        panel.background = element_rect(fill = "white", color = NA), 
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 8, color = "#666666"),
        legend.text = element_text(size = 8, color = "#666666"),
        plot.subtitle = element_text(color = "#666666", size = 11),
        plot.caption = element_text(color = "#AAAAAA", size = 8),
        plot.margin = unit(c(0.5, 0.7, 0.5, 0.7), "cm"),
        panel.border = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(color="#AAAAAA"),
        axis.ticks = element_line(color="#AAAAAA"))
