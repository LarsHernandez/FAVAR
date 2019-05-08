library(sf)

geo_sf <- read_sf("https://dawa.aws.dk/kommuner/?format=geojson")




library(statsDK)
library(tidyverse)

FOLK <- retrieve_data("FOLK1A", CIVILSTAND="TOT", 
                      ALDER = "IALT", 
                      Tid = "2019K1", 
                      KØN="TOT")
FOLK <- FOLK %>% filter(!str_detect(OMRÅDE, "All Denmark|Region")) 
FOLK$OMRÅDE <- str_replace(FOLK$OMRÅDE, "Copenhagen", "København")
FOLK[,c(1:4)] <- NULL


FOLK %>% 
  left_join(geo_sf, ., by = c('navn' = 'OMRÅDE')) %>% 
  mutate(INDHOLD = INDHOLD/1000) %>% 
  ggplot() + 
  geom_sf(aes(fill = INDHOLD, color = INDHOLD)) +
  geom_text(aes(x = visueltcenter_x, y = visueltcenter_y, label = navn), 
            color="white", size = 2) +
  labs(title = "Inhabitants by kommune",
       subtitle = "Geo data collected through DAWA API www.dawa.aws.dk", 
       x = "", y = "", 
       caption = "Source: Danmarks Statistik",
       color = "Inhabitants\n(thousands)", 
       fill = "Inhabitants\n(thousands)")
