# 11 Appendix I

rm(list = ls())
graphics.off()

# Load package
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(stringr)


# 11.1 Column-wise operations: across()

# summarise() & across()

mpg <- ggplot2::mpg # mpg data

#  count distinct values in each column
mpg %>% 
  summarise(across(.cols = everything(), # which columns:   all columns
                   .fns = n_distinct))   # which function:  count distinct/unique values

mpg %>% 
  summarise(across(everything(),
                   n_distinct))

#  calculate mean values for selected columns (list of columns)
mpg %>% 
  summarise(across(c(displ, cty, hwy), 
                   mean))

#  calculate median value for all numeric columns
mpg %>% 
  summarise(across(where(is.numeric),   # "where" clause supports conditions for columns selection!
                   median))

#  calculate distinct values of character columns
mpg %>% 
  summarise(across(where(is.character), 
                   n_distinct))

#  apply more than one function across multiple columns
#   - calculate mean and median of all numeric columns
mpg %>% 
  summarise(across(where(is.numeric), 
                   list(avg = ~mean(.x, na.rm = T),     # multiple functions: provided as a list of functions!
                        med = ~median(.x, na.rm = T))))

avgmed <- list(avg = ~mean(.x, na.rm = T),   
               med = ~median(.x, na.rm = T))
mpg %>% 
  summarise(across(where(is.numeric), 
                   avgmed)) 
                   
#  control names of output columns
mpg %>% 
  summarise(across(where(is.numeric), 
                   avgmed, 
                   .names = "{.fn}:{.col}")) # argument used for column name control

#  use multiple conditions for column selection 
mpg %>% 
  summarise(across(where(is.numeric) & ends_with("y"),
                   median))


# summarise() ~ group_by() & across()

#  calculate sum of all numeric columns break down by car model
mpg %>% 
  group_by(model) %>% 
  summarise(across(where(is.numeric), 
                   sum)) %>% 
  ungroup()

#  calculate mean value for selected columns break down by car manufacturer & model
mpg %>% 
  group_by(manufacturer, model) %>% 
  summarise(across(c(displ, cty, hwy),
                   mean)) %>% 
  ungroup()


# mutate() & across()

#  round up (ceiling) all numeric columns
mpg %>% 
  mutate(across(where(is.numeric),
                ~ceiling(.x)))

#  convert all character columns to upper case
mpg %>% 
  mutate(across(where(is.character),
                ~str_to_upper(.x)))


# mutate() ~ group_by() & across()

#  calculate mean value for all numeric columns break down by car manufacturer
#   - aggregate mean value of numeric columns for each manufacturer
#   - keep all the rows!
mpg %>% 
  group_by(manufacturer) %>% 
  mutate(across(where(is.numeric) & -year, # column "year" is removed from calculation!
                ~mean(.x, na.rm = T), 
                .names = "{.col}_avg_manufacturer")) %>% 
  ungroup()


# if_any() / if_all() with filter()
#
# if_any() : keeps the rows where the predicate is true for at least one selected column
#
# if_all() : keeps the rows where the predicate is true for all selected columns

starwars <- dplyr::starwars # star wars data set
?starwars

#  filter rows where at least one column doesn't have NA value
starwars %>% 
  filter(if_any(.cols = everything(), 
                .fns = ~ !is.na(.x)))

#  filter rows where all columns don't have NA value
starwars %>% 
  filter(if_all(.cols = everything(), 
                .fns = ~ !is.na(.x)))

#  filter rows where column "cty" or "hwy" have values greater than 20
mpg %>% 
  filter(if_any(c(cty, hwy), 
                ~ . > 20)) # condition written as function


#  filter rows where column "cty" and "hwy" have values greater than 20
mpg %>% 
  filter(if_all(c(cty, hwy), 
                ~ . > 20)) 

