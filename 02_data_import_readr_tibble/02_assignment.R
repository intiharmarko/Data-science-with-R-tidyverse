# 2 Assignment - Data Import (readr & tibble)


rm(list = ls())
graphics.off()


# Libraries
library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


# Exercise 1

# Create tibble with data about continents (source wikipedia: https://en.wikipedia.org/wiki/Continent)

continents <- tribble(
  ~`Date (data published)`, ~Continent,  ~`Area (km2)`,  ~`Percent total landmass`,  ~`Population`, ~`Percent total pop.`, # header
  #----------------------------------------------------------------------------------------------------------------------#
  "2017-11-10",              "Africa",        30370000,   20.4,                       1287920000,    16.9,
  "2017-11-10",              "Antarctica",    14000000,    9.2,                             4490,     0.1,
  "2017-11-10",              "Asia",          44579000,   29.5,                       4545133000,    59.5,
  "2017-11-10",              "Europe",        10180000,    6.8,                        742648000,     9.7,
  "2017-11-10",              "North America", 24709000,   16.5,                        587615000,     7.7,
  "2017-11-10",              "South America", 17840000,   12.0,                        428240000,     5.6,
  "2017-11-10",              "Australia",      8600000,    5.9,                         41264000,     0.5
  ) %>% 
  mutate(`Date (data published)` = lubridate::ymd(`Date (data published)`))

continents

# Calculate summaries
continents %>% 
  summarise(`Area (km2) - tot` = sum(`Area (km2)`),
            `Population - tot` = sum(`Population`),
            `Percent total landmass - check` = sum(`Percent total landmass`),
            `Percent total pop. - check` = sum(`Percent total pop.`))



# Exercise 2

# Import .csv file and parse columns
df2 <- read_csv(file = "./data/data_import/flights_02.csv", 
                col_names = T, 
                col_types = cols(
                  UniqueCarrier = col_character(),
                  FlightNum = col_integer(),
                  Year = col_integer(),
                  Month = col_integer(),
                  DayofMonth = col_integer(),
                  Origin = col_character(),
                  Dest = col_character(),
                  Distance = col_number()
                ))

str(df2)


# Exercise 3

# Import .csv file:
#  - check .csv file before importing
#  - some additional strategies not just parsing must be considered
df3 <- read_delim(file = "./data/data_import/flights_03.csv", 
                  delim = "|", 
                  col_names = F, 
                  skip = 12, 
                  comment = "#",
                  col_types = cols(.default = "c") # force all columns to be as character type
                  ) %>% 
  # add column names
  rename(UniqueCarrier = 1, 
         FlightNum = 2, 
         Date = 3, 
         Origin = 4, 
         Dest = 5, 
         Distance = 6) %>% 
  # column parsing inside mutate
  mutate(
    UniqueCarrier = as.character(UniqueCarrier),
    FlightNum = as.integer(FlightNum),
    Date = lubridate::ymd(Date),
    Origin = as.character(Origin),
    Dest = as.character(Dest),
    Distance = as.numeric(Distance))

str(df3)




# Exercise 4

# Import large csv file 
# use read_csv() & fread
system.time(
  df4.read <- read_csv2(file = "./data/data_import/big_table_04.csv", 
                        col_names = T, 
                        col_types = cols(
                          word = col_character(),
                          logical = col_logical(),
                          integer = col_integer(),
                          double = col_number(),
                          date = col_date()
                        ))
)

system.time(
  df4.fread <- data.table::fread(file = "./data/data_import/big_table_04.csv", 
                                 sep = ";", 
                                 header = T, colClasses = "character")
)

