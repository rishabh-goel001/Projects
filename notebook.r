library("tidyverse")

taxi <- read_csv("datasets/taxi.csv")

head(taxi,5)

taxi <- taxi %>%
  rename(lat = pickup_latitude, long = pickup_longitude) %>%
  mutate(total=log(fare_amount + tip_amount)) %>%
  filter(fare_amount > 0 | tip_amount > 0)

taxi <- taxi  %>% 
  filter((lat >= 40.70 & lat <= 40.83) & (long >=-74.025 & long <= -73.93 ))

# Loading in ggmap and viridis for nice colors
library("ggmap")
library("viridis")
manhattan <- readRDS("datasets/manhattan.rds")

ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +

  geom_bin2d(data= taxi, aes(long, lat, alpha = 0.6),bins = 60) +
  labs(x="longitude",y="latitude",fill)

library("tree")
fitted_tree <- tree()

plot(fitted_tree)
text(fitted_tree)

library("lubridate")

taxi <- taxi %>% 
  mutate(hour = hour(pickup_datetime, label = TRUE),
         wday = wday(pickup_datetime, label = TRUE),
         month = month(pickup_datetime, label = TRUE))

# Fitting a tree with total as the outcome and 
fitted_tree <- tree()

plot(fitted_tree)
text(fitted_tree)
summary(fitted_tree)

library("randomForest")

fitted_forest <- randomForest(ntree = 80, sampsize = 10000)

summary(fitted_forest)

taxi$pred_total <- fitted_forest$predicted

ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  
  stat_summary_2d(data= taxi, aes(long, lat,z = pred_total, alpha = 0.6),fun = mean, bins = 60) +
  labs(x="longitude",y="latitude",fill = color)

mean_if_enough_data <- function(x) { 
  ifelse( length(x) >= 15, mean(x), NA) 
}

ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  
  stat_summary_2d(data= taxi, aes(long, lat,z = total, alpha = 0.6),
                  fun = mean_if_enough_data, bins = 60) +
  labs(x="longitude",y="latitude",fill = color)

spends_most_on_trips <- "downtown"