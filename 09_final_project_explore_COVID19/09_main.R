# 9 Case study: Explore COVID-19 pandemic in the USA

# (main R script) 


rm(list =ls())
graphics.off()

# libraries
library(tidyverse)
library(lubridate)
library(cowplot)
library(rio)
library(janitor)

## string matching
#install.packages("tidystringdist")
library(tidystringdist)

## rolling average
#install.packages("zoo")
library(zoo)

# custom functions
source("./09_functions.R")


# 1) Data import

path.origin <- "./data/" # path to data directory


# 1a) COVID-19 data
data.dir.name.COVID19 <- list.files(path = path.origin) %>% str_subset("^csse_") # name of COVID-19 data directory

### import each .csv into one table
df.COVID19 <- tibble(directory = paste0(path.origin, "/", data.dir.name.COVID19),   # main directory
                     file = list.files(path = directory)) %>%                       # list of .csv file
  mutate(path = str_c(directory, file, sep = "/")) %>%                              # create path string for import
  mutate(data = map(.x = path,                                                      # import files with readr & map
                    .f = function(path_){read_csv(path_,
                                                  col_types = cols(.default = "c")) # all columns parsed as "character" for simplicity
                    })) %>%   
  mutate(date = str_remove(string = file, pattern = ".csv"),
         date = mdy(date)) %>% 
  select(date, data) %>% 
  unnest(cols = "data") %>% 
  clean_names()

## data cleaning  
df.COVID19 <- df.COVID19 %>% 
  # column conversion
  mutate(last_update = ymd_hms(last_update)) %>%        # date parsing
  mutate_at(.tbl = .,                                   # numeric columns conversion 
            .vars = setdiff(colnames(.)[5:21], "iso3"), 
            .funs = as.numeric) %>% 
  rename(state = province_state)


# 1b) GDP 
data.dir.name.GDP <- list.files(path = path.origin) %>% str_subset("GDP") # name of GDP data directory

## import data
df.GDP <- rio::import(file = paste0(path.origin, "/", data.dir.name.GDP), 
                      sheet = "clean data") %>% 
  clean_names()

## column selection and cleaning
df.GDP <- df.GDP %>% 
  # select columns
  select(state = state_or_territory,
         gdp_nominal = nominal_gdp_2020,
         gdp_per_capita = gdp_per_capita_2020) %>% 
  # column conversion
  mutate_all(.tbl = ., .funs = str_remove_all, ",") %>% 
  mutate_all(.tbl = ., .funs = str_remove_all, "\\$") %>% 
  mutate_at(.tbl = .,  .vars = colnames(.)[2:3], .funs = as.numeric)


# 1c) Population 
data.dir.name.pop <- list.files(path = path.origin) %>% str_subset("Population") # name of population data directory

## data import
df.pop <- rio::import(file = paste0(path.origin, "/", data.dir.name.pop), 
                      sheet = "data") %>% 
  clean_names()

## column selection and cleaning
df.pop <- df.pop %>% 
  select(state = name,
         pop = pop_2019)


# 1d) Covid response indicators tracker
data.dir.name.response <- list.files(path = path.origin) %>% str_subset("OxCGRT_US_latest.csv") # name of government response data directory

## data import
df.COVID19.response <- read_csv(file = paste0(path.origin, "/", data.dir.name.response), 
                                col_types = cols(.default = "c")) %>% 
  clean_names()

## column selection and cleaning
df.COVID19.response <- df.COVID19.response %>% 
  mutate(date = ymd(date)) %>% 
  select(state = region_name,
         date,
         contains("index")) %>% 
  mutate_at(.tbl = ., 
            .vars = colnames(.)[3:ncol(.)],
            .funs = as.numeric)


# 1e) Covid vaccinations
data.dir.name.vacc <- list.files(path = path.origin) %>% str_subset("vaccine") # name of vaccination data directory

## data import
df.COVID19.vacc <- read_csv(file = paste0(path.origin, "/", data.dir.name.vacc), 
                            col_types = cols(.default = "c")) %>% 
  clean_names()

## column cleaning and selection
df.COVID19.vacc <- df.COVID19.vacc %>% 
  mutate(day = ymd(day),
         daily_vaccinations = as.numeric(daily_vaccinations)) %>% 
  select(state = entity,
         date = day,
         vaccinations = daily_vaccinations)



# 2) Initial data inspection - Exploratory Data Analysis

## Check:
## - missing values
## - time spans
## - which data is relevant and which is not
## - check how data is coded


## missing values 
## (and potential problems that go with it)
count_NA(df.COVID19)
count_NA(df.GDP)
count_NA(df.pop)
count_NA(df.COVID19.response)
count_NA(df.COVID19.vacc)


## time spans
check_time_span(df.COVID19)
check_time_span(df.COVID19.response)
check_time_span(df.COVID19.vacc)

### our main time span will be dictated by main data set (data regarding infected, killed, recovered)


## check main data set
## - we are interested in main variables ("confirmed", "deaths", "recovered", "active")
## - how data is coded (important for later data usage)
## - is data consistent (reported for all dates) for all variables (variable relevancy)
## - check on USA level and on a level for selected state

### state level
df.COVID19 %>% 
  select(state,date, confirmed:active) %>% # relevant columns
  filter(state == "California") %>% # selected state
  # convert table from wide to long format
  pivot_longer(cols = c("confirmed", "deaths", "recovered", "active"), 
               names_to = "variable", 
               values_to = "value") %>% 
  # visualize values
  ggplot(aes(x = date,
             y = value,
             color = variable)) +
  geom_point() +
  facet_grid(variable ~ ., 
             scales = "free")

### national level
df.COVID19 %>% 
  select(date, confirmed:active) %>% # relevant columns
  # convert table from wide to long format
  pivot_longer(cols = c("confirmed", "deaths", "recovered", "active"), 
               names_to = "variable", 
               values_to = "value") %>% 
  # aggregate on date and variable level
  group_by(date, variable) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  # visualize values
  ggplot(aes(x = date,
             y = value,
             color = variable)) +
  geom_point() +
  facet_grid(variable ~ ., 
             scales = "free")

### what is obvious regarding main data set:
### - all data regarding COVID-19 infections
### - is reported as running total of counts 
### - if we would like to see daily counts 
### - we will need to calculate day to day differences (this is done later)
### - data for variables "confirmed" and "deaths" is reported consistently
### - while data for "active" and "recovered" is inconsistent (data is missing)
### - we will focus only on "confirmed" and "deaths"



# 3) Data Wrangling - create the main table

## main table:
## - all relevant data is stored in a single table - "main table"
## - one row is represented by date d and state i
## - so main table row count is distinct dates X different states
## - all selected variables are stored in the main table (from all source tables)
## - in order to create main table we need to
##    - create universal list of US states (compatible with all state names from all sources!)
##    - generate all relevant dates
##    - combine state names and dates (table baseline data)
##    - and bring all relevant data from source tables to main data table
## - later some additional data wrangling using main table is applied!


## USA states list
## - check state names
## - we need one universal state names table 
## - first we will create a list of 50 US states names
## - then we will create a function that
##   - will try to match state names 
##   - from each data source
##   - with the states from our list
##   - state names match is made using string similarity matching

## get list of state names using datasets library 
states.list <- tibble(state_base = datasets::state.name) 

## do the state names matching across sources with states list
states.list.COVID19 <- state_matching(data = df.COVID19, col_name = "state.COVID19")
states.list.GDP     <- state_matching(data = df.GDP,     col_name = "state.GDP")
states.list.pop     <- state_matching(data = df.pop,     col_name = "state.pop")
states.list.COVID19.response <- state_matching(data = df.COVID19.response, col_name = "state.COVID19.response")
states.list.COVID19.vaccc    <- state_matching(data = df.COVID19.vacc, col_name = "state.COVID19.vacc")

## create one universal list by joining lists
states.list <- states.list.COVID19 %>% 
  inner_join(x = .,
             y = states.list.GDP, 
             by = "state_base") %>% 
  inner_join(x = .,
             y = states.list.pop, 
             by = "state_base") %>% 
  inner_join(x = .,
             y = states.list.COVID19.response, 
             by = "state_base") %>% 
  inner_join(x = .,
             y = states.list.COVID19.vaccc, 
             by = "state_base") %>% 
  # add state id
  arrange(state_base) %>% 
  mutate(state_id = row_number()) %>% 
  select(state_id, everything())

### one state name fix is needed  
### - state_base = "New York" -> state.COVID19.vaccc = "New Yersey"
### - adequate name is "New York State"
states.list.COVID19.vaccc %>% filter(state_base %in% c("New Jersey", "New York"))
df.COVID19.vacc %>% pull(state) %>% unique() %>% str_subset("^New")

### the name fix (manual)
states.list[states.list$state_base == "New York", "state.COVID19.vacc"] <- "New York State"


## Let's add state region (will be used as a helper in later explorations)
states.region <- tibble(state_base = state.name,
                        region = state.region)


## now create states table
df.states <- states.list %>% 
  left_join(x = .,
            y = states.region,
            by = "state_base")


## Table of relevant dates
## - we will use relevant dates from our main data source
## - and will generate table with dates column
df.dates <- tibble(date = seq.Date(from = df.COVID19 %>% pull(date) %>% min(), 
                                   to   = df.COVID19 %>% pull(date) %>% max(), 
                                   by   = "day"))


## Create main table
## - cross join states and dates tables
## - we need all possible combinations regarding states and dates
df.main <- df.states %>%
  # cross join
  full_join(x = .,
            y = df.dates,
            by = character()) # charatcer() for cross join

### check if main table:
### - has 50 different states
### - each state has equal distinct date and row count
df.main %>% count(state_base) %>% as.data.frame()


## now bring the data and keep (and rename) relevant columns
df.main <- df.main %>% 
  # data from main source table - COVID19 cases
  left_join(x = .,
            y = df.COVID19 %>% select(state, date, confirmed, deaths),
            by = c("state.COVID19" = "state", 
                   "date" = "date")) %>% 
  # data regarding GDP and population
  left_join(x = .,
            y = df.GDP,
            by = c("state.GDP" = "state")) %>% 
  left_join(x = .,
            y = df.pop,
            by = c("state.pop" = "state")) %>% 
  # COVID-19 vaccination data
  left_join(x = .,
            y = df.COVID19.vacc %>% select(state, date, vaccinations),
            by = c("state.COVID19.vacc" = "state", 
                   "date" = "date")) %>% 
  # COVID-19 response data
  left_join(x = .,
            y = df.COVID19.response,
            by = c("state.COVID19.response" = "state", 
                   "date" = "date")) %>% 
  # remove redundant state name columns
  select(-c("state.COVID19", "state.GDP", "state.pop", 
            "state.COVID19.response", "state.COVID19.vacc")) %>% 
  # re-arrange and rename columns
  select(state_id, 
         state = state_base,
         region,
         date,
         `confirmed total` = confirmed,
         `deaths total` = deaths,
         `daily vaccine doses` = vaccinations,
         everything()) %>% 
  rename(population = pop) %>% 
  # sort rows state - date
  arrange(state, date)


## additional data wrangling on main table
## - add population in millions
## - calculate daily counts for "confirmed" cases, "deaths", "vaccinations" 
## - check if there are any negative daily counts (due to error in total counts)
## - some correction must be applied in order to avoid negative values for daily counts
## - add flag for first date when vaccination data is detected
## - also check if vaccination data is missing after first date
## - check is some other COVID data is missing

### non vaccination COVID-19 data missing?
df.main %>% filter(is.na(`confirmed total`)) %>% nrow()
df.main %>% filter(is.na(`deaths total`)) %>% nrow()

### vaccination started much later after COVID-19s started 
### (so we expect missing values)
df.main %>% filter(is.na(`daily vaccine doses`)) %>% nrow()

### calculate first date for each state when you detected vaccination data values
df.state.vacc.date.min <- df.main %>% 
  filter(!is.na(`daily vaccine doses`)) %>% 
  group_by(state) %>% 
  summarise(min_date = min(date)) %>% 
  ungroup()

### strange since all states have min date on the same day
### (let's say this is the first date we have data for)
### (not the actual date when vaccination started in each state)

### now we will
### - first replace NAs for vaccination data with 0
### - then we will calculate daily counts for selected variables
### - and total counts over time for vaccination doses
df.main <- df.main %>% 
  mutate(`population in millions` = round(population / 10**6, 2)) %>%       # state population in millions
  mutate(`daily vaccine doses` = replace_na(`daily vaccine doses`, 0)) %>%  # replace NAs
  # calculate daily counts : total_count(current date) -  total_count(previous date)
  group_by(state) %>% 
  mutate(`confirmed daily cases`  = `confirmed total` - lag(`confirmed total`, 1),
         `deaths daily cases`     = `deaths total`    - lag(`deaths total`, 1)) %>% 
  # calculate total counts over time: cumulative sum from first day up to observed date
  mutate(`vaccine doses total` = cumsum(`daily vaccine doses`)) %>% 
  ungroup() %>% 
  # rearrange columns
  select(state_id:date,
         `confirmed total` , `confirmed daily cases`,
         `deaths total`    , `deaths daily cases`,
         `vaccine doses total`, `daily vaccine doses`,
         population, `population in millions`,
         everything())

### check if we have negative daily counts
df.main %>% filter(`confirmed daily cases` < 0)
df.main %>% filter(`deaths daily cases` < 0)
df.main %>% filter(`daily vaccine doses` < 0) %>% select(state, date, `daily vaccine doses`)

### we will fix negative daily counts (convert negative values into 0)
df.main <- df.main %>% 
  mutate(`confirmed daily cases` = case_when(`confirmed daily cases` >= 0 ~ `confirmed daily cases`,
                                             T ~ 0),
         `deaths daily cases`    = case_when(`deaths daily cases` >= 0 ~ `deaths daily cases`,
                                             T ~ 0),
         `daily vaccine doses`   = case_when(`daily vaccine doses` >= 0 ~ `daily vaccine doses`,
                                             T ~ 0))



# 4) Exploratory Data Analysis
## - let's try to answer some selected question by data exploration
## - along the way some additional data wrangling will be applied

## check how many states per region
df.main %>% 
  group_by(region) %>% 
  summarise(states = n_distinct(state)) %>% 
  ungroup()

## show states on map (state color defined by region) - just for orientation
max.date <- df.main %>% pull(date) %>% max(.) # last date in the data

### add state in lower case for joining to get geographic data
df.main <- df.main %>% 
  mutate(state_ = tolower(state)) # convert states names to lower

### draw the map
df.main %>% 
  filter(date == max.date) %>% 
  # get longitude and latitude data
  left_join(x = .,
            y = map_data("state"),
            by = c("state_" = "region")) %>% 
  ggplot(aes(x = long, y = lat,
             group = group)) +
  geom_polygon(aes(fill = region), 
               color = "black") +
  xlab("") +
  ylab("") +
  ggtitle("") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())



## Questions?:
## What is happening with total number of infected (absolute / relative counts) over time on a state level?
## What is happening with total number of deaths   (absolute / relative counts) over time on a state level?
## (relative count - percentage calculated using state population)

## assumption (regarding infected counts): we assume each person is infected only once, 
## we do not know if some people were infected multiple times and also if they were counted more than once!

### add relative counts for infected and killed
df.main <- df.main %>% 
  mutate(`confirmed total %` = `confirmed total` / population,
         `deaths total %`    = `deaths total` / population)


## we will draw graphs per each state inside each region
## since there are quite some states in each region
## we will create two subgroups of countries for each region
## add region state groups
df.region.group <- df.main %>% 
  # get state names per region
  group_by(region) %>% 
  count(state) %>% 
  ungroup() %>% 
  arrange(region, state) %>%
  # add state count
  group_by(region) %>% 
  mutate(states = n(),
         id = row_number()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  # add group id
  mutate(group = case_when(id <= round(states / 2, 0) ~ 1,
                           T ~ 2)) %>% 
  # create new columns region-group
  mutate(`region - group` = paste0(region, " - group ", group)) %>% 
  # final column selection
  select(region, state, `region - group`)

### bring region groups to main table
df.main <- df.main %>% 
  left_join(x = .,
            y = df.region.group %>% select(-region),
            by = "state")


## plot confirmed cases & deaths total count over time (function calls)
region.groups <- df.region.group %>% distinct(`region - group`) %>% pull(`region - group`) # vector of region groups
map(.x = region.groups, .f = plot_confirmed_cases_total) # function calls



## Questions?:
## Which state paid the highest price considering confirmed cases and deaths (relative count)?
## - we will check relative counts on the last date in the data

## create plot (relative count) - bar chart
df.main %>% 
  filter(date == max.date) %>% 
  # relevant columns
  select(region, state, `confirmed total %`, `deaths total %`) %>% 
  # data columns to long format
  pivot_longer(cols = c("confirmed total %", "deaths total %"), 
               names_to = "count", 
               values_to = "value") %>% 
  # sort states
  group_by(state) %>% 
  mutate(tot_percentage = sum(value)) %>% 
  ungroup() %>% 
  arrange(tot_percentage, state) %>% 
  mutate(state = as.factor(state),
         state = fct_inorder(state)) %>% 
  # create plot
  ggplot(aes(y = state,
             x = value,
             fill = region)) +
  geom_col(color = "black") +
  facet_wrap(count ~ .,
             scales = "free") +
  xlab("Percentage (%) of state population") +
  ylab("State") +
  ggtitle(paste0("Confirmed cases and deaths relative to state population (last observed date = " , 
                 max.date, ")")) +
  scale_fill_viridis_d() +
  theme_minimal()

# export plot
ggsave(filename = paste0("./explore/02_relative_count_confirmed_cases_deaths_last_date.png"),
       plot = last_plot(), 
       width = 30, height = 25, units = "cm")


## create plot (absolute count) - bar chart
df.main %>% 
  filter(date == max.date) %>% 
  # relevant columns
  select(region, state, `confirmed total`, `deaths total`) %>% 
  # data columns to long format
  pivot_longer(cols = c("confirmed total", "deaths total"), 
               names_to = "count", 
               values_to = "value") %>% 
  # sort states
  group_by(state) %>% 
  mutate(tot_percentage = sum(value)) %>% 
  ungroup() %>% 
  arrange(tot_percentage, state) %>% 
  mutate(state = as.factor(state),
         state = fct_inorder(state)) %>% 
  # create plot
  ggplot(aes(y = state,
             x = value,
             fill = region)) +
  geom_col(color = "black") +
  facet_wrap(count ~ .,
             scales = "free") +
  xlab("Number of cases") +
  ylab("State") +
  ggtitle(paste0("Confirmed cases and deaths absolute count (last observed date = " , 
                 max.date, ")")) +
  scale_fill_viridis_d() +
  theme_minimal()

# export plot
ggsave(filename = paste0("./explore/02_absolute_count_confirmed_cases_deaths_last_date.png"),
       plot = last_plot(), 
       width = 30, height = 25, units = "cm")


## create map (relative count) 
p1 <- df.main %>% 
  filter(date == max.date) %>% 
  # relevant columns
  select(region, state, `confirmed total %`, `deaths total %`) %>% 
  mutate(state_ = tolower(state)) %>% # convert states names to lower
  # get longitude and latitude data
  left_join(x = .,
            y = map_data("state"),
            by = c("state_" = "region")) %>% 
  ggplot(aes(x = long, y = lat,
             group = group)) +
  geom_polygon(aes(fill = `deaths total %`), 
               color = "black") +
  xlab("") +
  ylab("") +
  ggtitle(paste0("Percentage of deaths compared to state population (on ", max.date, ")")) +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

p2 <- df.main %>% 
  filter(date == max.date) %>% 
  # relevant columns
  select(region, state, `confirmed total %`, `deaths total %`) %>% 
  mutate(state_ = tolower(state)) %>% # convert states names to lower
  # get longitude and latitude data
  left_join(x = .,
            y = map_data("state"),
            by = c("state_" = "region")) %>% 
  ggplot(aes(x = long, y = lat,
             group = group)) +
  geom_polygon(aes(fill = `confirmed total %`), 
               color = "black") +
  xlab("") +
  ylab("") +
  ggtitle(paste0("Percentage of infected compared to state population (on ", max.date, ")")) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

plot_grid(p1, p2, nrow = 2)

# export plot
ggsave(filename = paste0("./explore/02_map_relative_count_confirmed_cases_deaths_last_date.png"),
       plot = last_plot(), 
       width = 30, height = 25, units = "cm")



## Questions?:
## Are the following daily dynamics changing over time?:
##  - daily confirmed cases
##  - daily deaths
## We will smooth all daily dynamics time series 
## (to get a better insight, and remove within week days differences)

### Let's calculate 7d average of daily dynamics
### - we will use rollapply from zoo package
df.main <- df.main %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(`confirmed daily cases 7d avg` = rollapply(`confirmed daily cases`, FUN = mean, width = 7, align ='right', fill  = NA)) %>% 
  mutate(`deaths daily cases 7d avg`    = rollapply(`deaths daily cases`,    FUN = mean, width = 7, align ='right', fill  = NA)) %>% 
  ungroup()

## plot confirmed daily cases & deaths 7d avg function calls (function calls)
map(.x = region.groups, 
    .f = plot_confirmed_deaths_cases_7d_avg) 



## Questions?:
## Do state wealth and/or state population have effect on total percentage of confirmed cases and deaths?
## - we will draw a scatter plot
## - showing GDP per capita and state population with total percentage of confirmed cases and deaths on last date!

## draw scatter plot
df.main %>% 
  filter(date == max.date) %>% 
  ggplot(aes(x = `confirmed total %`,
             y = `deaths total %`,
             size = `population in millions`,
             color = gdp_per_capita)) +
  geom_point(alpha = 0.75,
             show.legend = T) +
  facet_wrap(. ~ region) + 
  scale_color_gradient(low = "brown1", high = "green") +
  scale_size_area(max_size = 40) +
  xlab("Total confirmed cases (% of population)") +
  ylab("Total deaths (% of population)") +
  ggtitle("Total confirmed cases % and total deaths % VS state GDP and state population") +
  theme_bw()

# export plot
ggsave(filename = paste0("./explore/04_confirmed_deaths_percentage_gdp_population_scatter.png"),
       plot = last_plot(), 
       width = 30, height = 20, units = "cm", 
       dpi = 600)



## Questions?:
## Does vaccination help decrease COVID 19 confirmed cases and death toll? 
## - we will extend graph from before
## - where we draw moving average daily cases and deaths
## - we will add total vaccine doses

## plot confirmed daily cases & deaths 7d avg + total vaccine doses (function calls)
map(.x = region.groups, 
    .f = plot_confirmed_deaths_cases_7d_avg_vaccine_doses_total) 



## Questions?:
## Show on map how number of COVID cases changed over time
## - we will create monthly snapshots (every 30 days one snapshot)
## - and draw subplot (map) for each monthly snap shot
## - showing absolute count - number of infected people

## add date id's and snapshot flag
df.main <- df.main %>% 
  arrange(state, date) %>% 
  # add date id per state
  group_by(state) %>% 
  mutate(date_id = row_number()) %>% 
  ungroup() %>% 
  # add date snapshot flag for every 30th date 
  mutate(`date snapshot flag` = case_when(date_id == 1 ~ TRUE,        # include first date
                                          date == max.date ~ TRUE, # include last date
                                          date_id %% 30 == 0 ~ TRUE,  # include every 30th date
                                          T ~ FALSE))

## create map
df.main %>% 
  filter(`date snapshot flag`) %>% 
  # relevant columns
  select(region, state, date, `confirmed total`) %>% 
  mutate(state_ = tolower(state)) %>% # convert states names to lower
  # get longitude and latitude data
  left_join(x = .,
            y = map_data("state"),
            by = c("state_" = "region")) %>% 
  ggplot(aes(x = long, y = lat,
             group = group)) +
  geom_polygon(aes(fill = `confirmed total`), 
               color = "black") +
  facet_wrap(. ~ date) +
  xlab("") +
  ylab("") +
  ggtitle("Number of total cases over time") +
  scale_fill_gradient(low = "white", high = "brown1") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

# export plot
ggsave(filename = paste0("./explore/06_map_total_count_confirmed_cases_over_time.png"),
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", 
       dpi = 600)



## Questions?:
## Show on map how total number of vaccination doses has increased over time
## - use monthly snapshots
## - and draw subplot (map) for each monthly snap shot
## - showing vaccine doses total count

df.main %>% 
  filter(`date snapshot flag`) %>% 
  # relevant columns
  select(region, state, date, `vaccine doses total`) %>% 
  mutate(state_ = tolower(state)) %>% # convert states names to lower
  # get longitude and latitude data
  left_join(x = .,
            y = map_data("state"),
            by = c("state_" = "region")) %>% 
  ggplot(aes(x = long, y = lat,
             group = group)) +
  geom_polygon(aes(fill = `vaccine doses total`), 
               color = "black") +
  facet_wrap(. ~ date) +
  xlab("") +
  ylab("") +
  ggtitle("Number of vaccine doses over time") +
  scale_fill_gradient(low = "white", high = "deepskyblue") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

# export plot
ggsave(filename = paste0("./explore/07_map_total_count_vaccine_doses_over_time.png"),
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", 
       dpi = 600)



## Questions?:
## How high was the response rate (precaution measurements) of each state towards pandemic?
## - we can use stringency index as measurement of response strength
## - also we will use monthly snapshots
## - and draw subplot (map) for each monthly snap shot
df.main %>% 
  filter(`date snapshot flag`) %>% 
  # relevant columns
  select(region, state, date, stringency_index_for_display) %>% 
  mutate(state_ = tolower(state)) %>% # convert states names to lower
  # get longitude and latitude data
  left_join(x = .,
            y = map_data("state"),
            by = c("state_" = "region")) %>% 
  ggplot(aes(x = long, y = lat,
             group = group)) +
  geom_polygon(aes(fill = stringency_index_for_display), 
               color = "black") +
  facet_wrap(. ~ date) +
  xlab("") +
  ylab("") +
  ggtitle("State response rate over time") +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

# export plot
ggsave(filename = paste0("./explore/08_map_state_response_rate_over_time.png"),
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", 
       dpi = 600)



## Questions?:
## How selected state is doing?
## - on a state level show
## - how total counts are changing over observed period
## - how 7 day average of daily cases, deaths and vaccinations is changing
## - how has been state responding to COVID 19 (show response index values over time)


## Let's calculate 7d average of daily vaccine doses
## - we will use rollapply from zoo package
df.main <- df.main %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(`daily vaccine doses 7d avg` = rollapply(`daily vaccine doses`, FUN = mean, width = 7, align ='right', fill  = NA)) %>% 
  ungroup()

## Create plots for selected states (plot selected states COVID 19 indicators)
plot_COVID19_indicators_state_level(state_ = "California")
plot_COVID19_indicators_state_level(state_ = "New York")
plot_COVID19_indicators_state_level(state_ = "Texas")
plot_COVID19_indicators_state_level(state_ = "Nebraska")

