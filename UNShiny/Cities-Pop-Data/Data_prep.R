library(readxl) 
library(dplyr) 
library(lubridate) 
library(tidyr) 
library(readr) 

cities_population_raw <- read_excel("data/city_populations.xls", skip = 16)
cities_pop <- cities_population_raw

colnames(cities_pop) <- c("index", "country_code","country",
                         "city_code","city", "note",
                         "latitude", "longitude",
                         "1950", "1955", "1960","1965", 
                         "1970",  "1975", "1980","1985",
                         "1990", "1995", "2000", "2005",
                         "2010","2015","2020","2025",
                         "2030", "2035")

cities_pop <- cities_pop %>%
  select(-c(index, country_code, city_code, note)) %>%
  gather(date, population, `1950`:`2035`) %>% 
  mutate(population = round(population)) %>% 
  rename(year = date)

cities_pop$date <- floor_date(as.Date(strptime(as.character(cities_pop$year), "%Y")), unit = "year")

write_csv(cities_pop, "Cities-Pop-Data/cities_pop.csv")

rm(cities_population_raw)
