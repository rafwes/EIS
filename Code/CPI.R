library(tidyverse)

CPIdata <- fread('https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems')

cityCodes <- c("S11A", "S12A", "S12B", "S23A", "S23B", "S24A", "S24B",
               "S35A", "S35B", "S35C", "S35D", "S35E", "S37A", "S37B",
               "S48A", "S48B", "S49A", "S49B", "S49C", "S49D", "S49E",
               "S49F", "S49G")
cities <- c(
            "Boston", "New York", "Philadelphia", "Chicago", "Detroit", "Minneapolis", "St. Louis",
            "Washington", "Miami", "Atlanta", "Tampa", "Baltimore", "Dallas", "Houston",
            "Phoenix", "Denver", "Los Angeles", "San Francisco", "Riverside", "Seattle", "San Diego",
            "Hawaii", "Alaska")

cityData <- data.frame(cityCodes, cities)

cityStrings <- paste0('CUUR', cityCodes, 'SA0')

cities_with_data <- lazy_dt(CPIdata) %>% 
  filter(series_id %in% cityStrings) %>%
  filter(year == 2005) %>%
  as_tibble()

cities <- unique(cities_with_data$series_id)
cities_codes <- str_sub(cities, 5, 8)

final_city_list <- cityData %>% filter(cityCodes %in% cities_codes)




