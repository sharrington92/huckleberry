
library(tidyverse)
library(lubridate)

files <- list.files(
  "H:/R Scripts/Other/Weather Data/Ephrata WA/", pattern = ".csv",
  full.names = T
)
  
# data.list <-  lapply(files, function(x){
#   read.csv(x, skip = 1)
# })
# 
# sapply(data.list, colnames)

data <- lapply(files, function(x){
  read.csv(x, skip = 1) %>% 
    select(-contains("WP"))
}) %>% 
  do.call(rbind, .) %>% 
  distinct() %>% 
  rename_with(~ tolower(gsub(".", "_", .x, fixed = TRUE))) %>% 
  mutate(
    date = ymd(date),
    year = year(date),
    day.year = yday(date),
    max.date = ifelse(date == max(date), 1, 0),
    month = as.factor(month(date))
  )

data %>% 
  rowwise() %>% 
  mutate(
    cdd = max((MAX_AIR_TEMP + MIN_AIR_TEMP) / 2 - 65, 0),
    day.year = yday(DATE),
    year = year(DATE)
  ) %>% 
  filter(day.year <= 202) %>% 
  group_by(year) %>% 
  mutate(cdd.agg = cumsum(cdd)) %>% 
  ggplot(aes(x = day.year, y = cdd.agg, color = as.factor(year))) +
  geom_line(size = 1) 


data %>% 
  mutate(
    year = year(DATE),
    day.year = yday(DATE)
  ) %>% 
  filter(day.year <= 202) %>% 
  ggplot(aes(x = day.year, y = MAX_AIR_TEMP, color = as.factor(year))) +
  geom_smooth()



data %>% 
  # rowwise() %>% 
  mutate(
    # year = year(DATE),
    above.90 = ifelse(max_air_temp >= 90, 1, 0),
    # day.year = yday(DATE)
  ) %>% 
  # filter(day.year <= 202) %>% 
  group_by(year) %>% 
  summarise(above.90 = sum(above.90)) %>% 
  ggplot(aes(x = year, y = above.90, fill = as.factor(year), label = above.90)) +
  geom_col() +
  geom_label(fill = "white")




