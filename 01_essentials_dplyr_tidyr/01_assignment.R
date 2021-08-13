# 1 Assignment - tidyverse essentials (dplyr & tidyr)


rm(list = ls())
graphics.off()


# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(hflights)

# Data
df <- hflights 


# Exercise 1

# Table dimensions
df %>% nrow(); df %>% ncol() 

# How many different carriers?
df %>% distinct(UniqueCarrier)

# Which and how many airports were involved?
df %>% 
  select(Origin, Dest) %>% 
  distinct() %>% 
  pivot_longer(cols = everything(), 
               names_to = "orig/dest", 
               values_to = "airport") %>% 
  distinct(airport) %>% 
  arrange(airport)

#   alternative - base R (a bit different output, but less code)
c(df$Origin, df$Dest) %>% unique() %>% sort()

# How many flights were canceled?
df %>% 
  filter(Cancelled == 1) %>% 
  nrow()



# Exercise 2

# Carrier statistics?
carrier.stats <- df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(flights = n(),
            # Total sums
            `Total distance flown (miles)` = sum(Distance),
            `Total ActualElapsedTime (h)` = round(sum(ActualElapsedTime, na.rm = T)/60,1),
            `Total AirTime (h)` = round(sum(AirTime, na.rm = T)/60,1),
            # Mean values
            `Mean distance flown (miles)` = mean(Distance),
            `Mean ActualElapsedTime (h)` = round(mean(ActualElapsedTime, na.rm = T)/60,1),
            `Mean AirTime (h)` = round(mean(AirTime, na.rm = T)/60,1))
carrier.stats

# Top 3 performing carriers % of flights over all flights.
carrier.stats %>% 
  select(UniqueCarrier, 
         distance = `Total distance flown (miles)`) %>% 
  arrange(desc(distance)) %>% 
  mutate(rank = row_number(), # add rank-position
         group = case_when(rank <= 3 ~ "top performers", # split carriers in 2 groups
                           TRUE ~ "the rest")) %>% 
  group_by(group) %>% # calculate sub-totals
  summarise(carriers = n(),   # number of carriers in a group
            distance = sum(distance)) %>% # total distance flown per group
  ungroup() %>% 
  mutate(`distance %` = round(distance / sum(distance) * 100, 2)) %>%  # calculate percentage
  arrange(desc(distance))

# resembles a  80/20 Pareto principle!
#  - 3 out of 15 (20%) carries flown 80% of all distance flown!



# Exercise 3

install.packages("stringr")   # manipulate strings
install.packages("lubridate") # manipulate date / times
install.packages("ggplot2")
library(stringr)
library(lubridate)
library(ggplot2)

#   Add date column & quarter + week column
df <- df %>% 
  # preserve date columns 
  mutate(year =  Year,
         month = Month,
         dayofmonth = DayofMonth) %>% 
  # add leading zeros to month & dayofmonth column
  mutate_at(.vars = c("Month", "DayofMonth"), 
            .funs = str_pad, 2, "left", "0") %>% 
  # create date column
  unite(., col = "date", Year, Month, DayofMonth, sep = "-") %>% 
  mutate(date = ymd(date)) %>% 
  # add quarter & week columns
  mutate(quarter = quarter(date),
         week = isoweek(date)) %>% 
  # re-arrange columns
  select(date, year, month, dayofmonth, quarter, week, DayOfWeek, everything())

# Is total number of flights increasing or decreasing quarterly?
df %>% 
  group_by(quarter) %>% 
  summarise(flights = n()) %>% 
  ungroup() %>% 
  mutate(`delta flights` = flights - lag(flights, 1),
         quarter = as.factor(quarter)) %>% 
  ggplot(aes(x = quarter, y = `delta flights`)) + # visualize optional
  geom_col()

# Is total distance increasing or decreasing monthly?
df %>% 
  group_by(month) %>% 
  summarise(distance = sum(Distance)) %>% 
  ungroup() %>% 
  mutate(`delta distance` = distance - lag(distance, 1),
         month = as.factor(month)) %>% 
  ggplot(aes(x = month, y = `delta distance`)) + # visualize optional
  geom_col()



# Exercise 4


# Create the heatmap:
#  - x     ~ carrier
#  - y     ~ month
#  - value ~ normalized value number of flights (divide with max)

df %>% 
  select(carrier = UniqueCarrier,
         month) %>% 
  group_by(carrier, month) %>% 
  summarise(flights = n()) %>% # number of flights
  ungroup() %>% 
  mutate(`max flights` = max(flights) ,# max number of flights
         `flights normalized` = flights / `max flights`) %>%  # normalized number of flights
  select(carrier, month, `flights normalized`) %>% 
  pivot_wider(names_from = month, 
              values_from = `flights normalized`, 
              values_fill = 0)

# visualize heatmap (optional)
df %>% 
  select(carrier = UniqueCarrier,
         month) %>% 
  group_by(carrier, month) %>% 
  summarise(flights = n()) %>% # number of flights
  ungroup() %>% 
  mutate(`max flights` = max(flights) ,# max number of flights
         `flights normalized` = flights / `max flights`) %>%  # normalized number of flights
  select(carrier, month, `flights normalized`) %>% 
  mutate(month = as.factor(month)) %>% 
  ggplot(aes(x = carrier, 
             y = month,
             fill = `flights normalized`)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal()
