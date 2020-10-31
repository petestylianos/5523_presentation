





wind_temp <- sensors %>% 
  filter(units == "km/h" | units == "C" | units == "hPa",
         sensor_id != "5a.EPA-1h") %>% 
  janitor::clean_names()

C <- wind_temp %>% 
  filter(site_id == "arc1045")  %>%
  pivot_wider(names_from = units, values_from = value) %>% 
  select(c("C", "km/h")) %>% 
  pull("C") %>% 
  na.omit()


km <- wind_temp %>% 
  filter(site_id == "arc1045")  %>%
  pivot_wider(names_from = units, values_from = value) %>% 
  select(c("C", "km/h")) %>% 
  pull("km/h") %>% 
  na.omit()


hpa <- wind_temp %>% 
  filter(site_id == "arc1045")  %>%
  pivot_wider(names_from = units, values_from = value) %>% 
  select(c("C", "km/h", "hPa")) %>% 
  pull("hPa") %>% 
  na.omit()


dat <- tibble(
  temp = C[1:30871],
  km = km,
  pres = hpa[1:30871]
)

dat %>% 
  filter(km >= 4) %>% 
  ggplot(aes(km, pres)) +
  geom_point()

cor(dat$temp, dat$km)




sensors %>% 
  filter(units == "hPa") %>% 
  ggplot(aes(x = value)) +
  geom_histogram()



unique(sensors$units)





sensors %>% 
  filter(units == "km/h") %>% 
  summarise(max = max(value, na.rm =T))
  


