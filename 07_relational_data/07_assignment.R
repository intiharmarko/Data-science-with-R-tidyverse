# 7 Data Wrangle: dplyr for relational data


rm(list = ls())
graphics.off()

# Libraries
library(tidyverse)
library(nycflights13)


# Exercise 1

# We will use nycflights13 database
# - and inspect if carriers use different planes
# - we would like to see the distribution
# - of number of planes per plane manufacturer
# - break down by carrier

## Add plane info & carrier name to flights table
df <- flights %>% 
  left_join(x = .,
            y = airlines %>% rename(carrier_name = name),
            by = "carrier") %>% 
  left_join(x = .,
            y = planes %>% select(-year),
            by = "tailnum")

## For each carrier count number of different planes break down by manufacturer
## - also add number of flights
carrier_plane_info <- df %>% 
  group_by(carrier_name, manufacturer) %>% 
  summarise(`nr of different planes` = n_distinct(tailnum),
            `nr of flight` = n()) %>% 
  ungroup()

## create bar plot:
## - manufacturer on x-axis
## - nr of planes on y-axis (frequencies)
## - fill color by carrier
carrier_plane_info %>% 
  ggplot(aes(x = manufacturer,
             y = `nr of different planes`,
             fill = carrier_name)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks = seq(0,2000,100)) +
  #scale_y_log10() +
  scale_fill_viridis_d() +
  xlab("Plane manufacturer") +
  ylab("Number of different planes") +
  ggtitle("Number of planes by each plane manufacturer break down by carrier") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
  


# Exercise 2

# Use weather data and flights data and try to answer the question:
# - Do weather conditions have affect on flight arrival delay time?

## Add weather data to main flights table
df <- df %>% 
  left_join(x = .,
            y = weather,
            by = c("origin", "year", "month", "day", "hour"))

## create scatter plot (you can use your own type of plot)
df %>% 
  mutate(delay = case_when(arr_delay > 30 ~ TRUE,
                           T ~ FALSE)) %>% 
  dplyr::sample_n(size = 50000) %>% 
  ggplot(aes(x = visib,
             y = precip,
             size = wind_speed,
             color = arr_delay)) +
  geom_jitter() +
  scale_size_area(max_size = 10) +
  scale_color_viridis_c(option = "magma") +
  facet_wrap(. ~ delay) 


# Exercise 3

#  In the first part of the task we will create two tables 
#  - table 1 ~ "distance_per_date":
#     - first create "date" column in flights table
#     - then calculate total distance flown for each carrier per date
#  - table 2 ~ "dates_span":
#    - generate table with one column "date"
#    - where dates are a sequence from minimum date found in flights table
#    - and up to maximum date found in the flights table

## total distances flown per date
distance_per_date <- flights %>% 
  # carrier name
  left_join(x = .,
            y = airlines %>% rename(carrier_name = name),
            by = "carrier") %>% 
  # date column
  mutate_at(.vars = c("month", "day"), .funs = str_pad, 2, "left", "0") %>% 
  unite("date", year, month, day, sep = "-") %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  # total flown distance per date
  group_by(carrier_name, date) %>% 
  summarise(`distance flown` = sum(distance)) %>% 
  ungroup()

### dates table
dates_span <- tibble(date = seq.Date(from = min(distance_per_date$date), 
                                     to =   max(distance_per_date$date),
                                     by = 1))


# Exercise 4

#  In the second part of the task we will: 
#  - join tables from previous task
#  - we would like to get a single table
#  - each date must be shown per each carrier
#  - if 365 different dates, then 365 rows per carrier
#  - probably some carriers did not fly on specific dates
#  - fill these blanks with 0 (0 miles flown)
#  - then calculate cumulative sum of miles flown per each carrier
#  - rows must be sorted by "date" per each carrier
#  - then draw a line chart:
#    - line per carrier
#    - each line represents cumulative distance flown
#    - x-axis ~ "date" | y-axis ~ "cumulative distance flown"
#    - color of line is represented by "carrier"

### first join the tables
cumulative_distance_per_date <- distance_per_date %>% 
  distinct(carrier_name) %>% # distinct carriers
  # cross join carriers with dates
  full_join(x = .,
            y = dates_span,
            by = character()) %>% # charatcer() for cross join
  # bring back distance flown info
  left_join(x = .,
            y = distance_per_date,
            by = c("date", "carrier_name")) %>% 
  # fill blanks with 0
  mutate(`distance flown` = replace_na(`distance flown`, 0)) %>% 
  # sort rows
  arrange(carrier_name, date) %>% 
  # add cumulative distance
  group_by(carrier_name) %>% 
  mutate(`distance flown cumulative` = cumsum(`distance flown`)) %>% 
  ungroup() %>% 
  rename(carrier = carrier_name)

### create the plot
cumulative_distance_per_date %>% 
  ggplot(aes(x = date,
             y = `distance flown cumulative` + 1,
             group = carrier,
             color = carrier)) +
  geom_line(size = 1.2) +
  scale_color_viridis_d(option = "inferno") +
  scale_y_log10() +
  xlab("Date") +
  ylab("Distance flown cumulative sum - log10 transformation") +
  ggtitle("Cumulative distance flown by carrier") +
  theme_minimal() 
 