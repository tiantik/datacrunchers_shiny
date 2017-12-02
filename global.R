library(dplyr)
library(data.table)

auto <- fread("data/usedcar_map_v9.csv",stringsAsFactors = T)

auto.byZip <- subset(auto, !is.na(longitude) & !is.na(latitude))  %>%
  group_by(postalCode, longitude, latitude, vehicleType) %>%
  summarise(priceMean = mean(price),
            priceMedian = median(price),
            ageMean = mean(as.integer(age)),
            ageMedian = median(as.integer(age)),
            kilometerMean = mean(kilometer),
            kilometerMedian = median(kilometer),
            count = n()
  ) %>%
  arrange(longitude, latitude)
