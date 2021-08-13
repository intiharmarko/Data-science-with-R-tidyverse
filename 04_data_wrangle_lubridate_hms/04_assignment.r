# 4 Assignment - Data Wrangle: dates / times (lubridate & hms)


rm(list = ls())
graphics.off()


# Libraries
library(tidyverse)
library(lubridate)
library(hms)


# Exercise 1

# Use proper parsing tool from lubridate library and parse given dates / times:

ymd_hms("2021-01-15 23:05:30") 
ymd_h("2030-01-01 05")
ydm_hm("2000-28-02 10:15")
ydm_h("1990-15-03 04")
mdy_hms("05/30/1995 9:15:45")
dmy_hms("1 Nov 2040 01/02:00")
dmy_hms("30 Jun 2035 20:45:00")
ymd("20000101")
mdy("January 1st 2029")
mdy("October 2nd 2028")
mdy("July 15th 2027")
dmy("30th March 25")
yq("2015: Q2")



# Exercise 2

# Find all leap years between year 1 and 3000
# - how many leap years
# - list of leap years

# First lets generate years (date of first day in each year)

# sequence of first day in each year
years <- seq.Date(from = as.Date("01-01-1"), 
                  to = as.Date("3000-01-01"), 
                  by = "years") 

# Create a tibble
df.years <- tibble(date = years,
                   year = lubridate::year(date)) %>% 
  # check if difference between each year is 1 (test)
  mutate(diff = year - lag(year, 1))

df.years %>% summarise(d.min = min(diff, na.rm = T), d.max = max(diff, na.rm = T))

# Add flag if leap year?
df.years <- df.years %>% 
  select(-diff) %>% 
  mutate(`leap year` = leap_year(date)) 

# How many leap years all together
df.years %>% pull(`leap year`) %>% sum(.)

# Which are the leap years?
df.leap_years <- df.years %>% filter(`leap year`)

# Number of leap years per century?
df.years <- df.years %>% 
  mutate(century = ceiling(year / 100)) # add century

# count leap years per century
df.years %>% 
  group_by(century) %>% 
  summarise(`leap years` = sum(`leap year`)) %>% 
  ungroup()

df.years %>% 
  group_by(century) %>% 
  summarise(`leap years` = sum(`leap year`)) %>% 
  ungroup() %>% 
  ggplot(aes(x = century,
             y = `leap years`)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0,30,2))



# Exercise 3

# Create a table for US holidays in 2021
US.holidays <- tribble(~holiday,    ~date,
        
        "New Year's Day",                "2021-01-01",
        "Martin Luther King Jr. Day",    "2021-01-18",
        "President's Day",               "2021-02-15",
        "Memorial Day",                  "2021-05-31",
        "Independence Day",              "2021-07-04",
        "Independence Day (observed)",   "2021-07-05",
        "Labor Day",                     "2021-09-06",
        "Columbus Day",                  "2021-10-11",
        "Veterans Day",                  "2021-11-11",
        "Thanksgiving Day",              "2021-11-25",
        "Christmas Day (observed)",      "2021-12-24",
        "Christmas Day",                 "2021-12-25",
        "New Year's Day (observed)",     "2021-12-31") %>% 
  mutate(date = ymd(date)) # parse date
  
        
# Calculate how many:
# - days are between each holiday
# - weeks ...
# - hours ...
# - seconds ...

US.holidays <- US.holidays %>% 
  # write the next holiday's date in the same line as current holiday's date
  mutate(`next holiday` = lead(x = holiday, n = 1),
         `next date` = lead(x = date, n = 1)) %>% 
  # add date difference - we will use periods
  mutate(`diff days` = as.period(`next date` - date),
         `diff weeks` = `diff days` / weeks(1),
         `diff hours` = `diff days` / hours(1),
         `diff seconds` = `diff days` / seconds(1))

# Which holiday was the last one?
# Which holiday will be the next one?
US.holidays <- US.holidays %>% 
  mutate(today = Sys.Date(), # add todays date
         `diff today` = as.period(date - today)) # difference between holiday and today in days

# Is today a holiday?
US.holidays %>% filter(`diff today` == 0) %>% select(holiday, date, `diff today`)
US.holidays %>% filter(`diff today` == 0) %>% nrow() > 0

# Which holiday was the last one?
US.holidays %>% filter(`diff today` < 0) %>% arrange(`diff today`) %>% select(holiday, date, `diff today`) %>% tail(1)

# Which holiday will be the next one?
US.holidays %>% filter(`diff today` > 0) %>% arrange(`diff today`) %>% select(holiday, date, `diff today`) %>% head(1)



# Exercise 4

# Import PJM Hourly Energy Consumption Data
#  - source kaggle
browseURL("https://www.kaggle.com/robikscube/hourly-energy-consumption?select=PJME_hourly.csv")

#  - file name: pjm_hourly_est.csv
#  - keep only columns: Datetime, PJME
#  - PJME - PJM East Region: 2001-2018 (PJME) ~ estimated energy consumption in Megawatts (MW)

df.energy <- read_csv(file = "./data/pjm_hourly_est.csv", 
                      col_names = T, 
                      col_types = cols(.default = "c")) %>% 
  # keep only relevant columns
  select(datetime = Datetime,  # date time
         econs = PJME) %>%     # energy consumption in MW 
  # column conversion - parsing
  mutate(datetime = ymd_hms(datetime),
         econs = as.numeric(econs)) %>% 
  # filter only rows where you have data
  filter(!is.na(econs)) %>% 
  # sort rows based on time
  arrange(datetime)

# Do we have data for every hour - time point in observed time period?
df.energy <- df.energy %>% 
  mutate(`diff h` = as.period(datetime - lag(datetime, 1)) / hours(1)) # calculate difference between 2 points and convert difference to hours
  
df.energy %>% summarise(d.min = min(`diff h`, na.rm = T), d.max = max(`diff h`, na.rm = T))

# Difference 0 hours?
df.energy %>% filter(`diff h`== 0)
df.energy %>% filter(datetime >= "2014-11-02 00:00:00" & datetime <= "2014-11-02 05:00:00") # one example


# Difference 2 hours?
df.energy %>% filter(`diff h`== 2)
df.energy %>% filter(datetime >= "2002-04-07 00:00:00" & datetime <= "2002-04-07 08:00:00") # one example


# Add columns:
# - day ~ date
# - month
# - years
df.energy <- df.energy %>% 
  # initial columns
  mutate(date = as_date(datetime),
         month = month(date),
         year = year(date))

# Now calculate time intervals for each:
# - date
# - month
# - year

# For each interval calculate:
# - total energy consumption
# - smoothed energy consumption ~ mean / average hourly consumption

# year consumption
df.energy.y <- df.energy %>% 
  select(datetime, year, econs) %>% 
  # add start / end of each time interval & and create interval
  group_by(year) %>% 
  mutate(`int start` = min(datetime), # start of interval
         `int end`   = max(datetime), # end of interval
         `int` = `int start` %--% `int end`) %>%  # add actual interval
  ungroup() %>% 
  # calculate totals and averages
  group_by(year, `int`) %>% 
  summarise(`econs tot`   = sum(econs,  na.rm = T),
            `econs h avg` = mean(econs, na.rm = T)) %>% 
  ungroup()
  
df.energy.y %>% 
  ggplot(aes(x = year,
             y = `econs h avg`)) +
  geom_line()


# month consumption
df.energy.m <- df.energy %>% 
  select(datetime, year, month, econs) %>% 
  # create year-month column
  mutate(year_month = ym(paste(year, "-", month, sep = ""))) %>%
  # add start / end of each time interval & and create interval
  group_by(year_month) %>% 
  mutate(`int start` = min(datetime), # start of interval
         `int end`   = max(datetime), # end of interval
         `int` = `int start` %--% `int end`) %>%  # add actual interval
  ungroup() %>% 
  # calculate totals and averages
  group_by(year_month, `int`) %>% 
  summarise(`econs tot`   = sum(econs,  na.rm = T),
            `econs h avg` = mean(econs, na.rm = T)) %>% 
  ungroup()

df.energy.m %>% 
  ggplot(aes(x = year_month,
             y = `econs h avg`)) +
  geom_line()


# day consumption
df.energy.d <- df.energy %>% 
  select(datetime, date, econs) %>% 
  # add start / end of each time interval & and create interval
  group_by(date) %>% 
  mutate(`int start` = min(datetime), # start of interval
         `int end`   = max(datetime), # end of interval
         `int` = `int start` %--% `int end`) %>%  # add actual interval
  ungroup() %>% 
  # calculate totals and averages
  group_by(date, `int`) %>% 
  summarise(`econs tot`   = sum(econs,  na.rm = T),
            `econs h avg` = mean(econs, na.rm = T)) %>% 
  ungroup()

df.energy.d %>% 
  ggplot(aes(x = date,
             y = `econs h avg`)) +
  geom_line()
