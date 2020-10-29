library(tidyverse)
library(leaflet)
library(patchwork)

loc <- read_csv("data/locations.csv") %>% 
  select(c(site_id, description, latitude, longitude)) %>% 
  as_tibble()

loc


readings <- read_csv("data/readings.csv")  %>% 
  as_tibble()

readings
unique(readings$units)

sensors <- readings %>% 
  left_join(loc, by = "site_id") 



sensors %>% 
  group_by(site_id, units) %>% 
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)
            ) %>% 
  arrange(units) %>% 
  View()




leaflet(loc) %>% 
  addProviderTiles("Esri") %>% 
  addMarkers(lng = loc$longitude,
             lat = loc$latitude,
             popup = loc$site_id)



# density of wind speed

sensors %>% 
  filter(units == "km/h") %>% 
  ggplot(aes(value, group = site_id, fill = site_id )) +
  geom_density(alpha = 0.4) 
  
  
