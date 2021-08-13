# 1 tidyverse essentials (dplyr & tidyr)

rm(list = ls())
graphics.off()

# Install packages and load package
install.packages("tidyverse") # tidyverse packages
install.packages("dplyr")     # individual package installation
install.packages("tidyr")     # individual package installation
install.packages("ggplot2")   # we need it for the data set

library(dplyr)
library(tidyr)
library(ggplot2)

# Inspect data
help("mpg")

df <- mpg
View(df) # check table
print(df) # print to console
str(df) # table structure
nrow(df); ncol(df) # rows ~ cols


# 1.3 Manipulate variables (columns) - select(), rename()

# select() - Columns selection

#   Extract columns car: manufacturer, model, year
select(df, manufacturer, model, year) # just extract columns
df.car.info <- select(df, manufacturer, model, year)
rm(df.car.info)

#   Extract columns that begin with letter "m"
select(df, starts_with("m"))

#   Extract columns that contain letter "r"
select(df, contains("r"))

#   Extract columns that end with letter "y"
select(df, ends_with("y"))

#   Select column by columns index
select(df, 1:3)      # first three columns
select(df, c(2,5,7)) # second, fifth and seventh column
select(df, (ncol(df)-2):ncol(df)) # last three columns


# rename() - Rename columns

#   Rename columns "manufacturer" and "model"
df1 <- rename(df, 
              mnfc = manufacturer,
              mod = model)


# select columns and rename in one take
select(df,
       mnfc = manufacturer,
       mod = model,
       everything()) # selects all other columns


# 1.4 mutate(), transmute()

# mutate() - Create new variable

#   Create variable: average between highway and city miles per gallon
df <- mutate(df,
             `avg miles per gallon` = (cty + hwy) / 2)

#   Create variable "car" & "cyl / trans"
df <- mutate(df,
             car = paste(manufacturer, model, sep = " "),
             `cyl / trans` = paste(cyl, " cylinders", " / ", trans, " transmission", sep = ""))


# transmute() - Create new variable and drop other variables
transmute(df,
          `avg miles per gallon` = (cty + hwy) / 2)
transmute(df,
          car = paste(manufacturer, model, sep = " "),
          `cyl / trans` = paste(cyl, " cylinders", " / ", trans, " transmission", sep = ""))

# reset data
rm(df1)
df <- mpg


# 1.5 Manipulate cases (rows) - filter(), slice()

# filter() - Extract rows by condition

#   Filter rows where manufacturer is "audi"
filter(df, manufacturer == "audi") 

#   Filter rows where manufacturer is "audi" and year is 1999
filter(df, manufacturer == "audi" & year == 1999) 

#   Filter rows where manufacturer is either "audi" or "dodge"
df1 <- filter(df, manufacturer == "audi" | manufacturer == "dodge")
df2 <- filter(df, manufacturer %in% c("audi", "dodge")) 

#   Filter rows where hwy is greater or equal than 30
filter(df, hwy >= 30)

#   Filter rows where year is not equal to 1999
filter(df, year != 1999)


# slice() - Extract rows by row position

#   Extract first 5 rows
slice(df, 1:5)

#   Extract rows from 20th row to 30th row
slice(df, 20:30)

#   Extract last 10 rows
slice(df, (nrow(df)-9):nrow(df))


# 1.6 arrange() - Sort rows

#   Sort rows by year (ascending order)
arrange(df, year)

#   Sort rows by year (descending order)
arrange(df, desc(year))

#   Sort rows by year (ascending order), cyl and displ
df.sort <- arrange(df, year, cyl, displ)


# 1.7 distinct / unique table row values

# distinct() - get only unique or distinct values

# Let's create a simple working example
df.example <- data.frame(id = 1:3,
                         name = c("John", "Max", "Julia"))
df.example <- bind_rows(df.example, slice(df.example, 2)) # create duplicate of 2nd row
df.example <- arrange(df.example, id)
df.example

# show table without duplicates
distinct(df.example)

#  Back to mpg example - lets create a table with duplicates
df.dupl <- select(df, manufacturer, model)

#   Keep only unique rows without duplicates
df.nodupl <- distinct(df.dupl)


# 1.8 Sample rows

# sample_n() - Filter n randomly selected rows 
set.seed(567)

#   10 randomly selected rows without replacement
sample_n(df, size = 10, replace = F)

#   10 randomly selected rows with replacement
sample_n(df, size = 10, replace = T)


# sample_frac() - Filter a fraction of randomly selected rows 

#   10% of table rows randomly selected
sample_frac(df, size = 0.1, replace = F)


# 1.9 Summarise (aggregate) table

# summarise() - apply summary functions on table and create summaries

#   Calculate average hwy
summarise(df, `mean hwy` = mean(hwy))

#   Count table rows, and count distinct car models
summarise(df, 
          rows = n(),
          `nr models` = n_distinct(model))

#   Calculate min / max hwy & cty
summarise(df, 
          `min hwy` = min(hwy),
          `min cty` = min(cty),
          `max hwy` = max(hwy),
          `max cty` = max(cty))


# 1.10 Group cases by certain variables

# group_by() - group cases using one or more grouping variables

#   Group cars by manufacturer
group_by(df, manufacturer)

# combine summarise() & group_by() - summary statistics for grouped data

#   Count number of cars for each manufacturer
summarise(group_by(df, manufacturer),
          cars = n())

#   Calculate mean / min / max hwy for each model
summarise(group_by(df, model),
          `mean hwy` = mean(hwy),
          `min hwy` = min(hwy),
          `max hwy` = max(hwy))


# count() - count rows for grouped variables

#   Count number of table rows
count(df)

#   Count number of cars per model
count(group_by(df, model))

rm(list = ls())
df <- mpg


# 1.11 pipe operator %>%
#     chain dplyr functions using pipe operator
#     every step executed in one pipeline!

#   Count the number of cars where manufacturer is "audi"
df %>% 
  filter(manufacturer == "audi") %>% 
  count()

#   Filter rows for manufacturer "dodge" or "chevrolet" and select
#   only columns manufacturer, model, year, class
df %>% 
  filter(manufacturer %in% c("dodge", "chevrolet")) %>% 
  select(manufacturer, model, year, class)

#   Calculate average hwy and count number of cars for each 
#   manufacturer, model, class, transmission type
#   also filter results where average hwy is greater than 30
#   and show results in descending order based on average hwy
df %>% 
  group_by(manufacturer, model, class, trans) %>% 
  summarise(`mean hwy` = mean(hwy),
            cars = n()) %>% 
  ungroup() %>%               # do not forget to ungroup - inside a pipeline!
  filter(`mean hwy` > 30) %>% 
  arrange(desc(`mean hwy`))


# 1.12 pivoting: convert table from long to wide format and vice versa

# Let's create a simple table in long format
table.long <- data.frame(id = 1:6,
                         type = c("a", "b", "a", "c", "c", "a"),
                         count = c(20, 50, 45, 15, 12, 5))
table.long

# pivot_wider() - convert long data to wide data

#   Convert table to wide format - each "type" in its own column
table.wide <- pivot_wider(table.long, 
                          names_from = type, 
                          values_from = count)
table.wide

# pivot_longer() - convert wide data to long data

#   Convert table back to long format
table.long1 <- pivot_longer(table.wide, 
                            cols = c("a", "b", "c"), 
                            names_to = "type", 
                            values_to = "count", 
                            values_drop_na = T)
table.long1

# Now let's pivot our car data set table

#   Filter only rows where manufacturer is "jeep" or "land rover" or "hyundai"
#   and select columns model, transmission type, hwy
#   calculate average hwy for each model and transmission type
#   this will be our long format table
df.long <- df %>% 
  filter(manufacturer %in% c("jeep", "land rover", "hyundai")) %>% 
  select(model, trans, hwy) %>% 
  group_by(model, trans) %>% 
  summarise(`mean hwy` = mean(hwy)) %>% 
  ungroup()
df.long

#   Now convert long table to wide format - where transmission type is transformed into columns
df.wide <- df.long %>% 
  pivot_wider(names_from = trans, 
              values_from = `mean hwy`)
df.wide

#   Convert df.wide back to long format
df.long1 <- df.wide %>% 
  pivot_longer(-model,  # exclude column "model" and use all remaining columns!!!
               names_to = "trans", 
               values_to = "mean hwy", 
               values_drop_na = T)
df.long1


# 1.13 separating and uniting columns

# Let's create a table with date column (generate date for 1 year)
dates <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2021-12-31"), by = "day") # generate dates
table <- data.frame(date = dates)
table %>% head(); table %>% tail()

# separate() - split one column into multiple columns

#   split date column into year, month, dayofmonth
#   remove leading zeros where necessary (month and dayofmonth)
#   sort columns
table.sep <- table %>% 
  separate(data = ., 
           col = date, 
           into = c("year", "month", "dayofmonth"), 
           sep = "-") %>% 
  mutate(month = as.numeric(month),
         dayofmonth = as.numeric(dayofmonth)) %>% 
  arrange(year, month, dayofmonth)
  
# more elegant way using mutate_at!
table.sep_ <- table %>% 
  separate(data = ., 
           col = date, 
           into = c("year", "month", "dayofmonth"), 
           sep = "-") %>% 
  mutate_at(.tbl = .,                         # which table? - . stands for table in the pipe line!
            .vars = c("month", "dayofmonth"), # which variables are mutated?
            .funs = as.numeric) %>%           # which functions is applied?
  arrange(year, month, dayofmonth)


# unite() - combine multiple columns into one column

# We will use library stringr for adding leading zeros for month and dayofmonth column !!!
install.packages("stringr")
library(stringr)

#   create one date column by merging columns:
#   year, month, dayofmonth
#   add leading zeros for month, dayofmonth using stringr library
#   sort columns
table.unite <- table.sep %>% 
  # add leading zeros
  mutate(month = str_pad(month, width = 2, side = "left", pad = "0"), # add leading zeros to month
         dayofmonth = str_pad(dayofmonth, width = 2, side = "left", pad = "0")) %>% # add leading zeros to dayofmonth
  unite(data = ., 
        col = "date", 
        year, month, dayofmonth, 
        sep = "-") %>% 
  arrange(date)

# more elegant way using mutate_at!
table.unite_ <- table.sep %>% 
  # add leading zeros
  mutate_at(.tbl = .,                              # which table? - . stands for table in the pipe line!
            .vars = c("month", "dayofmonth"),      # which variables are mutated?
            .funs = str_pad, 2, "left", "0") %>%   # which functions is applied? - function parameters are written down
  unite(data = ., 
        col = "date", 
        year, month, dayofmonth, 
        sep = "-") %>% 
  arrange(date)


rm(list = ls())
df <- mpg


# 1.14 dplyr and tidyr in action

# pull() - extract column as vector
df %>% pull(hwy)
df %>% pull(hwy) %>% class()
df %>% select(hwy) %>% class()


# group_by() + mutate()

#   calculate average hwy per car manufacturer & car model!
df <- df %>% 
  group_by(manufacturer, model) %>% 
  mutate(`mean hwy` = mean(hwy)) %>% 
  ungroup()


# case_when() - case when statements

#   add variable "transmission type": automatic, manual
df %>% count(trans) # check which values occur

df <- df %>% 
  mutate(trans_ = str_sub(string = trans, 
                          start = 1, 
                          end = 1)) %>%  # extract first letter from trans
  mutate(`transmission type` = case_when(trans_ == "a" ~ "automatic",
                                         trans_ == "m" ~ "manual",
                                         TRUE ~ "NA")) %>% 
  select(-trans_)

df %>% count(`transmission type`, trans)  # check car count


# row_number() - add ranks

#   add car rank / id not considering groups
df <- df %>% 
  mutate(`car id` = row_number())

#   add car rank / id consudering groups (per manufacturer)
df <- df %>% 
  group_by(manufacturer) %>% 
  mutate(`car id1` = row_number()) %>% 
  ungroup()

rm(list =ls())


# Transform table holding flights data
install.packages("hflights")
library(hflights)

df <- hflights

# count number of rows/columns, different flights
nrow(df); ncol(df)
df %>% 
  count(UniqueCarrier, FlightNum, TailNum, Year, Month, DayofMonth) %>% 
  arrange(desc(n))
#   one flight is represented with!: UniqueCarrier, FlightNum, TailNum, Year, Month, DayofMonth
  
# how many columns begin with word "Taxi"?
df %>% 
  select(starts_with("Taxi")) %>% 
  head()

# how many flights were flown less than 1000 miles / greater or equal than 1000 miles
df %>% 
  mutate(dist1000 = case_when(Distance < 1000  ~ "< 1000 miles",
                              Distance >= 1000 ~ ">= 1000 miles")) %>% 
  count(dist1000)

# flights per carrier - sort by top to bottom
df %>% 
  group_by(UniqueCarrier) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

# number of cancelled flights per carrier
df %>% count(Cancelled) # 1 for cancelled
df %>% 
  filter(Cancelled == 1) %>% 
  group_by(UniqueCarrier) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

# percentage of cancelled flights per carrier
df %>% 
  # count flights break down by cancellation
  group_by(UniqueCarrier, Cancelled) %>% 
  count() %>% 
  ungroup() %>% 
  # calculate total flights
  group_by(UniqueCarrier) %>% 
  mutate(`n tot` = sum(n)) %>% 
  ungroup() %>% 
  # calculate percentages
  mutate(`n percent %` = (n / `n tot`) * 100) %>% 
  # keep only cancelled flights - arrange top to bottom
  filter(Cancelled == 1) %>% 
  arrange(desc(`n percent %`))

# create column date by combining year + month + dayofmonth (remove this columns)
df <- df %>% 
  # add leading zeros to month and dayofmonth
  mutate_at(.vars = c("Month", "DayofMonth"), 
            .funs = str_pad, 2, "left", "0") %>% 
  unite(col = "Date", 
        Year, Month, DayofMonth, 
        sep = "-") 

# check date range
df %>% 
  summarise(min = min(Date),
            max = max(Date),
            n_distinct = n_distinct(Date))

# count flights per cancellation codes (codes in columns)
# and per carrirs (carriers in rows)
# pivoting required!!!
df %>% count(CancellationCode) # cancellation code "" must have some name other than empty string!

df %>% 
  mutate(CancellationCode = case_when(CancellationCode == "" ~ "0",  # this is not cancelled flight!!!
                                      TRUE ~ CancellationCode)) %>% 
  group_by(UniqueCarrier, CancellationCode) %>% 
  count() %>% 
  ungroup() %>% 
  pivot_wider(names_from = CancellationCode, 
              values_from = n)

  