# 7 Data Wrangle: dplyr for relational data

rm(list = ls())
graphics.off()

# Install packages and load package
library(tidyverse)
install.packages("nycflights13")
library(nycflights13)


# 7.2 Example database

# Let's check the database we will be using in this section.
?nycflights13

# Tables:
View(nycflights13::airlines)
View(nycflights13::airports)
View(nycflights13::planes)
View(nycflights13::weather)
View(nycflights13::flights)



# 7.3 Mutating joins

# Lets' create two simple tables (from slides)
table_x <- tribble(~key, ~val,
                   1,    "a1",
                   2,    "a2",
                   3,    "a3")

table_y <- tribble(~key, ~val,
                   1,    "b1",
                   2,    "b2",
                   4,    "b3")
table_x
table_y


# inner join

## simple tables
inner_join(x = table_x,
           y = table_y,
           by = "key", 
           suffix = c(".x", ".y"))

## use inner join on nycflights13 database:
#   - add name of the carrier to flights table 
#   - use carrier name from airlines table
#   - then count number of flights and show counts using carrier names
View(nycflights13::airlines)
View(nycflights13::flights)

### do the join
df <- flights %>% 
  inner_join(x = ., 
             y = airlines,
             by = c("carrier" = "carrier")) %>% 
  rename(carrier_name = name)

### show the counts
df %>% 
  count(carrier_name) %>% 
  arrange(desc(n))



# left join

## simple tables
left_join(x = table_x,
           y = table_y,
           by = "key", 
           suffix = c(".x", ".y"))

## use left join on nycflights13 database:
#   - your base table is flights
#   - now create one table that will include all the data from other tables
#   - data regarding weather, planes, carriers, etc.
#   - with left join you can add data to mai flights table
View(nycflights13::airlines)
View(nycflights13::flights)
View(nycflights13::planes)
View(nycflights13::airports)
View(nycflights13::weather)

### fix year column in planes table (we will rename it, to avoid confusion)
df.planes <- planes %>% rename(year_plane = year)

### we will add data in a pipe (multiple left joins)
df.all <- flights %>% 
  left_join(x = .,
            y = airlines,           # carrier name
            by = "carrier") %>% 
  rename(carrier_name = name) %>%   # rename carrier name column
  left_join(x = .,
            y = airports,           # destination airport data
            by = c("dest" = "faa")) %>% 
  rename(dest_name = name) %>%      # rename destination airport name column
  left_join(x = .,
            y = df.planes,             # plane data
            by = c("tailnum" = "tailnum")) %>% 
  left_join(x = .,
            y = weather,             # origin airport weather data
            by = c("origin" = "origin",
                   "year"   = "year",
                   "month"  = "month",
                   "day"    = "day",
                   "hour"   = "hour"))  



# right join

## simple tables
right_join(x = table_x,
           y = table_y,
           by = "key", 
           suffix = c(".x", ".y"))

## right join is similar to left join
#   - only x and y table position are switched
#   - first count number of flights per each tail num
#   - then bring fligt counts to planes table
#   - use right join!

## flight counts 
df.flight.counts <- flights %>% count(tailnum)

## bring flight counts to planes table
df.planes <- df.flight.counts %>% 
  right_join(x = .,
             y = planes,
             by = c("tailnum" = "tailnum")) %>% 
  rename(`number of flights` = n)



# full join

## full tables
full_join(x = table_x,
          y = table_y,
          by = "key", 
          suffix = c(".x", ".y"))

## use full join on nycflights13 database:
#   - use full join to
#   - combine all carriers (airlines table)
#   - and all destination airports (from flights table)
#   - are there any carriers that didn't fly to given destination airports?
#   - which carriers (names)? which destination airports?

### select only relevant columns from flights table
df.dest <- flights %>% 
  select(carrier, dest)

### do the full join
df.carrier_dest <- airlines %>% 
  full_join(x = .,
            y = df.dest,
            by = "carrier")

### do the check
df.carrier_dest %>% 
  filter(is.na(dest))
df.carrier_dest %>% 
  filter(is.na(carrier))

### each carrier has at least one flight to all destination airports



# 7.4 Filtering joins


# semi join

## simple tables
semi_join(x = table_x,
          y = table_y,
          by = "key")

## now 
#   - create new table airlines1
#   - by only keeping carriers ("AA", "VX", "DL") inside airlines table
#   - do a semi join between airlines1 as x 
#   - and flights as y
#   - and reverse tables
#   - what is the maning of the results
airlines1 <- airlines %>% 
  filter(carrier %in% c("AA", "VX", "DL"))

### semi joins
semi_join(x = airlines1,
          y = flights,
          by = "carrier")

semi_join(x = flights,
          y = airlines1,
          by = "carrier")


# anti join

## simple tables
anti_join(x = table_x,
          y = table_y,
          by = "key")

## similar as we did with semi join
#   - why the first anti join is empty table?
#   - what is the meaning of the second anti join?
anti_join(x = airlines1,
          y = flights,
          by = "carrier")

anti_join(x = flights,
          y = airlines1,
          by = "carrier")



# 7.5 Set operations

# First let's show how tables are binned

## We will create two simple tables using airlines table:
#   - airlines1 -> in table airlines keep rows 1,3,5,7,9 
#   - airlines2 -> in table airlines keep rows 2,4,5,8,9 
airlines1 <- airlines %>% dplyr::slice(c(1,3,5,7,9))
airlines2 <- airlines %>% dplyr::slice(c(2,4,5,8,9))


# bind_cols - bind tables column wise
bind_cols(airlines1, airlines2)


# bind_rows - bind tables row wise
bind_rows(airlines1, airlines2)


# now apply set operations using created tables

# intersect - rows that appear in both x and y 
intersect(airlines1, airlines2)


# setdiff - rows that appear in x and not in y 
setdiff(airlines1, airlines2)


# union - rows that appear in x or y 
union(airlines1, airlines2) ## we get unique rows returned without duplicates!



# 7.6 dplyr's additional functions

# for demonstration we will create a table called df
#  - using flights table
#  - filter only rows for "AA" carrier
#  - sort rows based on date time of flight

df <- flights %>% 
  filter(carrier == "AA") %>% 
  arrange(time_hour)


## Check:
#   - if 2 successive flights 
#   - flew from the same origin airport
#   - use lag() function
#   - count number of given flights

df <- df %>% 
  mutate(`origin prev flight` = lag(x = origin, n = 1)) %>%                 # extract previous flight origin airport
  mutate(`origin test` = case_when(origin == `origin prev flight` ~ TRUE,   # check the origin airports
                                   T ~ FALSE))
## number of flights
df %>% filter(`origin test`) %>% count()


## Check:
#   - if 2 successive flights 
#   - had total distance over 2000 miles
#   - use lead() function
#   - count number of given flights

df <- df %>% 
  mutate(`distance successive flights` =  distance  +  lead(x = distance, n = 1)) %>%  # total distance of two successive flights
  mutate(`distance test` =  case_when(`distance successive flights` >= 2000 ~ TRUE,    # check total distance
                                   T ~ FALSE))
  
## number of flights
df %>% filter(`distance test`) %>% count()


## check (using df):
#   - if you are able to find 
#   - which is the flight 
#   - where total distance flown
#   - first time exceeded 1.000.000 miles
#   - we will use running total - cumsum()

df <- df %>% 
  mutate(`distance runing tot` = cumsum(distance))

## which flight
df %>% mutate(`fligt id` = row_number()) %>% 
  filter(`distance runing tot` >= 1000000) %>% 
  select(`fligt id`, everything()) %>% 
  head(1)


## use df and:
#  - rank flights based on distance of the flight
#  - shorter distances have lower ranks
#  - use dplyr's dense_rank()

df <- df %>% 
  mutate(`rank flight` = dense_rank(distance))
