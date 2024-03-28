# Importing required library
library(tidyverse)

df <- read.csv("C:/Users/johan/Downloads/Workshop/R-wrkshop.csv")

names(df)

View(df)

df <- df %>% select(pickup_datetime, TNC, trip_miles, trip_time, base_passenger_fare, Zone_d, Borough)

df %>% summary()

df %>% filter(base_passenger_fare > 0) %>% summary()

df %>% filter(base_passenger_fare > 0, trip_miles < 15) %>% summary()

df <- df %>% 
  filter(base_passenger_fare > 0, trip_miles < 15) %>% 
  rename(fare = base_passenger_fare) %>% 
  mutate(fare_per_mile = fare/trip_miles)



# our data
df %>% 
  # randomly pick 500 data points
  sample_n(500) %>% 
  # send this filtered data to ggplot
  ggplot() +
  # add a layer of scatter plot
  geom_point(aes(x = trip_miles, y = trip_time))

df %>% 
  sample_n(500) %>% 
  ggplot() +
  geom_point(aes(x = trip_miles, y = trip_time, color = fare))

df %>% 
  sample_n(500) %>% 
  ggplot() +
  geom_point(aes(x = trip_miles, y = trip_time)) +
  geom_smooth(aes(x = trip_miles, y = trip_time))

df %>% 
  sample_n(500) %>% 
  ggplot() +
  geom_point(aes(x = trip_miles, y = trip_time, color = TNC)) +
  geom_smooth(aes(x = trip_miles, y = trip_time, color = TNC))

df %>% 
  ggplot() +
  geom_density(aes(x = fare)) +
  xlim(0, 100)

df %>% 
  ggplot() +
  geom_density(aes(x = fare, fill = TNC), alpha = 0.5) +
  xlim(0, 100)

# Violin Plot
df %>% 
  ggplot() +
  geom_violin(aes(x = TNC, y = fare_per_mile)) +
  ylim(0, 50)

# Scattered plot
df %>% 
  sample_n(500) %>% 
  mutate(hod = hour(pickup_datetime)) %>% 
  ggplot() + 
  geom_point(aes(x= hod, y = trip_time))

df %>% 
  mutate(hod = hour(pickup_datetime)) %>% 
  group_by(hod) %>% 
  summarize(mean_trip_time = mean(trip_time),
            mean_trip_miles = mean(trip_miles),
            sd_trip_time = sd(trip_time),
            sd_trip_miles = sd(trip_miles)) %>% 
  summary()


# Trend Plot with different colors for points
df %>% 
  mutate(hod = hour(pickup_datetime)) %>% 
  group_by(hod, TNC) %>% 
  summarize(mean_trip_time = mean(trip_time),
            mean_trip_miles = mean(trip_miles),
            sd_trip_time = sd(trip_time),
            sd_trip_miles = sd(trip_miles)) %>% 
  ggplot() +
  geom_point(aes(x = hod, y = mean_trip_time, color = TNC))


# Line plot Trip Duration by Hour of Day
df %>% 
  mutate(hod = hour(pickup_datetime),
         date = day(pickup_datetime)) %>% 
  group_by(hod, date) %>% 
  summarise(mean_trip_time = mean(trip_time)) %>% 
  ggplot() + 
  geom_point(aes(x = hod, y = mean_trip_time, color = factor(date)), alpha = 0.5) +
  geom_line(aes(x = hod, y = mean_trip_time, color = factor(date)), alpha = 0.3) +
  theme(legend.position = "none") +
  xlab("Hour of Day") +
  ylab("Average Trip Duration") +
  ggtitle("Trip Duration by Hour of Day")

library(sf)

shpf <- read_sf("C:/Users/johan/Downloads/Workshop/taxi_zones/taxi_zones.shp")

names(shpf)

View(shpf)


# Map plotted 
shpf %>% 
  ggplot() +
  geom_sf(aes(fill = Shape_Area)) +
  scale_fill_continuous(low = 'yellow', high = 'red')

ndf <- df %>% 
  group_by(Zone_d) %>% 
  summarize(trips = n()) 


# Map in a combo of blue and dark
merge(shpf, ndf, by.x = "zone", by.y = "Zone_d") %>% 
  ggplot() +
  geom_sf(aes(fill = trips))


# Map with focused areas within the map
merge(shpf, ndf, by.x = "zone", by.y = "Zone_d") %>% 
  ggplot() +
  geom_sf(aes(fill = trips)) + 
  scale_fill_gradient2(low = 'red', mid = 'yellow', high = 'blue')


# Final plot with color categories
merge(shpf, ndf, by.x = "zone", by.y = "Zone_d") %>% 
  ggplot() +
  geom_sf(aes(fill = trips)) + 
  scale_fill_binned(guide = 'colorbar',
                    breaks = as.numeric(quantile(ndf$trips)),
                    type = 'viridis')


