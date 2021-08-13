# 2 Data import (tibble & readr)

rm(list = ls())
graphics.off()

# Install packages and load package
install.packages("tibble")
install.packages("readr")     

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)


# 2.1 Table called tibble

# some build in tibbles (inside ggplot2)
ggplot2::economics
class(ggplot2::economics)

ggplot2::diamonds
class(ggplot2::diamonds)

ggplot2::faithfuld
class(ggplot2::faithfuld)

# hflights data set  (not a tibble - can be converted, shown later)
hflights::hflights
class(hflights::hflights)


# 2.2 Create a tibble

# as_tibble() - From data frame (conversion)

#   Convert hflights data frame
library(hflights)
class(hflights)
dft <- as_tibble(hflights)
dft
class(dft)

#   Convert custom data frame
df <- data.frame(x = 1:10, 
                 y = seq.Date(from = as.Date("2021-01-01"), 
                              to = as.Date("2021-01-10"), 
                              by = "day"))
df
class(df)

dft <- as_tibble(df)
dft
class(dft)


# tibble() - Create custom tibble
tibble(v1 = seq(from = 1, to = 100, by = 1),
       v2 = pi,
       v3 = sqrt(v1),
       v4 = seq.Date(from = as.Date("2021-01-01"), 
                     length.out = 100, 
                     by = "day"))


# Use strange non-syntactic column names
tibble(`123` = 123,
       `.` = "period",
       `,` = "comma",
       `,*/-+?!` = "strange name")


# tribble() - Create transposed tibble
tribble(
  ~name,  ~surname,  ~male,  ~age, # header
  #--------------------------------#
  "Max",  "Smith",   T,      35,    
  "Lily", "Brown",   F,      27
  )


# 2.3  data.frame VS tibbles

# Print output
hflights %>% as_tibble()
hflights


# Subsetting

#   Extract by name - $
mpg$manufacturer

#   Extract by name - [[]]
mpg[["manufacturer"]]

#   Extract by position - [[]]
mpg[[1]]

#   Using with a pipe operator
mpg %>% .$manufacturer
mpg %>% .[["manufacturer"]]



# 2.5  Read files

library(readr)

# Read inline csv file
read_csv("c1,c2,c3
          1,a,T,
          2,b,T,
          3,c,F")

#   Inline files with a meta header lines
read_csv("First meta line
          Second meta line
          c1,c2,c3
          1,a,T,
          2,b,T,
          3,c,F", skip = 2)

#   Inline files with comments
read_csv("c1,c2,c3 # comment
          1,a,T,   # comment
          2,b,T,
          3,c,F", comment = "#")


# Read comma separated files - .csv from your disk

list.files("./data") # show files

#   Small mpg table
df <- read_csv(file = "./data/mpg_mini.csv")

#   Small mpg table (column separator = ";)
df <- read_csv2(file = "./data/mpg_mini2.csv")


# Read tab separated files - .tsv from your disk
df <- read_tsv(file = "./data/mpg.tsv")


# Read files with selected delimiter
df <- read_delim(file = "./data/mpg_delim.txt", 
                 delim = "~")


# Read text file:
#   - " " as separator
#   - decimal separator ","
#   - quotations around strings
#   - meta lines in header
#   - empty lines
#   - missing values
df <- read_delim(file = "./data/mpg.txt", 
                 col_names = T, 
                 skip = 3, 
                 skip_empty_rows = T, 
                 delim = " ", 
                 quote = "\"", 
                 na = "")


# Read log based file:
read_log(file = "./data/example.log")


# Read large .csv file
#  - read.csv   VS   read_csv
#  - execution times
system.time(
  df <- read.csv(file = "./data/mpg_maxi.csv")
)

system.time(
  df <- read_csv(file = "./data/mpg_maxi.csv")
)



# 2.6  Vector parsing

# Parse character vector
parse_character(c("one", "two", "three"))
parse_character(c("one", "two", 3))

# Other type encoding
konnichiwa <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd" # Japanese word
parse_character(konnichiwa) # UTF-8 encoding by default
parse_character(konnichiwa, locale = locale(encoding = "Shift-JIS")) # switch encoding


# Parse logical vector
parse_logical(c("TRUE", "FALSE", "T", "F"))
parse_logical(c("TRUE", "FALSE", "T", "F", "NA"))

#   check parsing problems
x <- parse_logical(c("TRUE", "FALSE", "T", "F", "NA", "string"))
problems(x) 


# Parse integer vector
parse_integer(c("10", "20", "30", "40"))
parse_integer(c("10", "20", "30", "40.5"))


# Parse factor
parse_factor(c("one", "two", "one"))
parse_factor(c("one", "two", "one"), levels = c("two", "one"))


# Parse double vector
parse_double(c("11.7", "4.13"))

#   Different decimal mark
parse_double(c("11,7", "4,13"))
parse_double(c("11,7", "4,13"), 
             locale = locale(decimal_mark = ","))


# Parse number
parse_number(c("1", "2.2", "$1000", "20%", "1,000"))

#   Grouping mark specified
parse_number(c("100,000.2"), 
             locale = locale(grouping_mark = ","))


# Parse date
parse_date("2021-12-31")

#   Specify date format
parse_date("20211231", "%Y%m%d")
parse_date("21/12/31", "%y/%m/%d")


# Parse time
parse_time("00:01")
parse_time("00:01 am")
parse_time("00:01:00")


# Parse datetime
parse_datetime("2021-12-31 00:01")



# 2.7  File parsing

# Guess parser heuristic
guess_parser(c("T", "F"))
guess_parser("2021-12-31")
guess_parser("2021-12-31 00:01")
guess_parser(c("5", "10"))


# Parse each column mpg table
read_tsv(file = "./data/mpg.tsv",
         col_types = cols(
           manufacturer = col_factor(),
           model = col_factor(),
           displ = col_double(),
           year = col_integer(),
           cyl = col_integer(),
           trans = col_character(),
           drv = col_character(),
           cty = col_number(),
           hwy = col_number(),
           fl = col_character(),
           class = col_character()
         ))


# Import table
#  - do not specify column types at import
#  - change column types inside R
read_tsv(file = "./data/mpg.tsv") %>% 
  mutate_at(.vars = c("year", "cyl"), .funs = as.integer) %>%       # integer conversion
  mutate_at(.vars = c("manufacturer", "model"), .funs = as.factor)  # factor conversion



# 2.8  Other useful import libraries

install.packages("readxl")
install.packages("rio")
install.packages("data.table")

library(readxl)
library(rio)
library(data.table)

# Import excel file

#   readxl
read_excel(path = "./data/mpg.xlsx")
read_excel(path = "./data/mpg.xlsx", sheet = "Sheet 1") # specify sheet
read_excel(path = "./data/mpg.xlsx", range = "A1:C10")  # specify range
read_excel(path = "./data/mpg.xlsx") %>% class()

#   rio
rio::import(file = "./data/mpg.xlsx")
rio::import(file = "./data/mpg.xlsx") %>% class()
rio::import(file = "./data/mpg.xlsx", sheet = "Sheet 1") # specify sheet
rio::import(file = "./data/mpg.xlsx", range = "A1:C10")  # specify range


# Import large flat file with fread

df.f <- fread(file = "./data/mpg_maxi.csv", sep = ",")

# Read large .csv file
#  - compare execution times
#  - read.csv VS read_csv VS fread

print("Execution time read.csv():")
system.time(
  df1 <- read.csv(file = "./data/mpg_maxi.csv")
)

print("Execution time read_csv():")
system.time(
  df2 <- read_csv(file = "./data/mpg_maxi.csv")
)

print("Execution time fread():")
system.time(
  df3 <- fread(file = "./data/mpg_maxi.csv")
)



# 2.9 Write files

# write a csv file

#   Comma separated
write_csv(x = mpg, 
          file = "./data/mpg_w.csv", 
          col_names = T)

#   Semicolon separated
write_csv2(x = mpg, 
          file = "./data/mpg_w2.csv", 
          col_names = T)


# write a xlsx file
rio::export(x = mpg, 
            file = "./data/mpg.xlsx")


# write/read to/from a .rds file
write_rds(x = mpg, file = "./data/mpg.rds")
read_rds(file = "./data/mpg.rds")


# write/read to/from a feather type file
install.packages("feather")
library(feather)

write_feather(x = mpg, 
              path = "./data/mpg.feather")
read_feather(path = "./data/mpg.feather")


