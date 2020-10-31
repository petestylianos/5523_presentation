library(tidyverse)
library(leaflet)
library(patchwork)
library(calendR)
library(lubridate)



loc <- read_csv("data/locations.csv") %>% 
  select(-location) %>% 
  as_tibble()

loc


readings <- read_csv("data/readings.csv")  %>% 
  as_tibble()

readings <- readings %>% 
  select(-id)

readings


unique(readings$units)

# 5 different site_id's
#18 sensors_id's   12 of those contain EPA


sensors <- readings %>% 
  left_join(loc, by = "site_id") 




sensors %>% 
  filter(!grepl("EPA", sensor_id),
         units == "km/h") %>% 
  group_by(site_id, sensor_id, type, units) %>% 
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)
  ) %>% 
  arrange(units) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling()
  





leaflet(loc) %>% 
  addProviderTiles("Esri") %>% 
  addMarkers(lng = loc$longitude,
             lat = loc$latitude,
             popup = loc$site_id)





monthly_sensors <-  sensors %>% 
  mutate(day = lubridate::ymd(as.Date(local_time)),
         month = lubridate::month(local_time),
         hour = hour(as_datetime(local_time))
  )








mean_wind <- sensors %>%
  filter(units == "km/h") %>%
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  pull(mean) %>% 
  as.numeric()



wind_by_day <-   monthly_sensors %>%
  group_by(day) %>% 
  filter(units == "km/h") %>%
  summarise(mean_day_wind = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(mean_day_wind >= mean_wind) %>% 
  select(day) %>% 
  mutate(cal_day =
           case_when(
             day < "2020-01-01" ~
               day - as.Date(as.character("2019-01-01"), format="%Y-%m-%d"),
             day >= "2020-01-01" ~
               day - as.Date(as.character("2020-01-01"), format="%Y-%m-%d")
             
           )
  )






calendR(year = 2020,
        start = "M",
        special.days = wind_by_day$cal_day,
        special.col = "green",            # Color of the specified days
        low.col = "white",
        weeknames.size = 3,
        day.size = 2,
        orientation ="p",
        title = "",
        subtitle = "") +
  tvthemes::theme_avatar() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Days with above average wind speed"
  )

## That's a lot of days with above average wind ? Is this a mistake ?


sensors %>% 
  filter(units == "km/h") %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ site_id) 

# missing  wind speed data

sensors %>% 
  filter(units == "km/h") %>% 
  ggplot(aes(value, group = site_id, fill = site_id )) +
  geom_histogram(alpha = 0.4) +
  theme_bw() 



## We only have observations when wind speed was 0 and then when wind speed
## was above 4, and no values in between, is this a mistake ?


# mean value for wind speed above 4 
mean_value_above4 <- sensors %>% 
  filter(units == "km/h",
         value > 4) %>%
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  pull(mean) %>% 
  as.numeric()

# How the calendar look now ?


wind_by_day_above4 <-   monthly_sensors %>%
  group_by(day) %>% 
  filter(units == "km/h") %>%
  summarise(mean_day_wind = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(mean_day_wind >= mean_value_above4) %>% 
  select(day) %>% 
  mutate(cal_day =
           case_when(
             day < "2020-01-01" ~
               day - as.Date(as.character("2019-01-01"), format="%Y-%m-%d"),
             day >= "2020-01-01" ~
               day - as.Date(as.character("2020-01-01"), format="%Y-%m-%d")
             
           )
  )



cal <-  calendR(year = 2020,
                start = "M",
                special.days = wind_by_day_above4$cal_day,
                special.col = "green",            # Color of the specified days
                low.col = "white",
                weeknames.size = 3,
                day.size = 2,
                orientation ="p",
                title = "",
                subtitle = "") +
  tvthemes::theme_avatar() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Days with above average wind speed, 
    when the average exlucdes days with zero wind speed")

ggplot2::ggsave( cal, filename = "images/cal.png")








monthly_sensors %>% 
  filter(units == "km/h") %>% 
  ggplot(aes(x = as.factor(month), y = value, fill = site_id)) +
  geom_boxplot() +
  facet_wrap(~site_id)


monthly_sensors$hour <- factor(monthly_sensors$hour, levels = 0:23)

monthly_sensors %>% 
  filter(units == "km/h") %>% 
  ggplot(aes(x = as.factor(hour), y = value, fill = site_id)) +
  geom_boxplot() +
  facet_wrap(~site_id, ncol = 2) 


##The difference in temperature causes differences in air pressure between
##n the two spots. This air pressure differential leads to the formation of w
##inds as the atmosphere tries to equalize the air pressure. Generally, the l
##arger the temperature difference, the stronger the resulting winds will be.

