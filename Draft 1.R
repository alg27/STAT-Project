df <- read.csv("smaller_taxicab_data.csv")
#Settings to make plotting faster
options(bitmapType='cairo')
x11(type='cairo')

library(lubridate)
library(dplyr)

df <- df %>%
  mutate(
    pickup_time = mdy_hms(tpep_pickup_datetime),
    dropoff_time = mdy_hms(tpep_dropoff_datetime),
    trip_time = as.numeric(difftime(dropoff_time, pickup_time, units = "mins")),
    speed = trip_distance / (trip_time/ 60), # Speed in miles per hour
    fare_per_mile = if_else(trip_distance > 0, fare_amount / trip_distance, NA_real_) # Handle division by zero
  ) %>%
  
  filter(!is.na(fare_per_mile) & fare_per_mile < 50) %>% # Filter for fare_per_mile criteria
  filter(speed <= 120) %>% # Assuming you still want to filter unrealistic speeds
  
  # Filter out rows with any negative values in the specified columns
  filter(
    fare_amount > 0,
    #trip must be longer than 10 seconds
    trip_time > (10/60),
    extra >= 0,
    mta_tax >= 0,
    tip_amount >= 0,
    tolls_amount >= 0,
    improvement_surcharge >= 0,
    congestion_surcharge >= 0,
    total_amount >= 0
  )