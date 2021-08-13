# 6 Functional Programming: purrr

rm(list = ls())
graphics.off()

# Install packages and load package
install.packages("purrr")
library(purrr)
library(tidyverse)
library(hflights)


# 6.2 map: apply function

# map() - basic function (return a list!)


# Example 1:
# - use hflights data set
# - and calculate: mean, min, max, sd 
# - for columns: "ActualElapsedTime", "AirTime", "Distance", "TaxiIn" ,"TaxiOut" 

df <- hflights %>% 
  select(ActualElapsedTime, AirTime, Distance, TaxiIn, TaxiOut)

df %>% map(., mean, na.rm = TRUE)
df %>% map(., min, na.rm = TRUE)
df %>% map(., max, na.rm = TRUE)
df %>% map(., sd, na.rm = TRUE)


# Example 2:
# - we would like to select only numeric columns in a cars data set
# - we can do selection by hand (first use str() and then pick each column)
# - or we can do it programmatically using map!

## pick by hand
str(mpg)
mpg %>% 
  select(displ, year, cyl, cty, hwy)

## using map
numeric.cols <- map(.x = mpg, .f = is.numeric) %>% 
  unlist() %>% 
  tibble(column = names(.),
         numeric = .) %>% 
  filter(numeric == T) %>% 
  pull(column)
mpg[, numeric.cols]

## what if we have a lot of columns - hflights data set for example
str(hflights)
numeric.cols <- hflights %>%  
  map(., is.numeric) %>% 
  unlist() %>% 
  tibble(column = names(.),
         numeric = .) %>% 
  filter(numeric == T) %>% 
  pull(column)
hflights[, numeric.cols] %>% as_tibble()



# 6.3 map: control output


# map_dbl() - return a numeric (double) vector

## our previous example with summaries
df %>% map_dbl(., mean, na.rm = TRUE)
df %>% map_dbl(., min, na.rm = TRUE)
df %>% map_dbl(., max, na.rm = TRUE)
df %>% map_dbl(., sd, na.rm = TRUE)

## create summary table
df %>% colnames() %>% 
  tibble(variable = .,
         mean = df %>% map_dbl(., mean, na.rm = TRUE),
         sd = df %>% map_dbl(., sd, na.rm = TRUE))


# map_int() - return an integer vector

## create a list of objects
list <- list(a = 1,
             b = "word",
             v = 1:10,
             df = mpg)
list

## return length of each list object
list %>% map_int(., length)
  

# map_dfc() - data frame column bind

## summaries
df %>% map_dfc(., mean, na.rm = TRUE)



# 6.4 map shortcuts

# Example: fit multiple regression line
#  - use mpg data set
#  - fit regression line: hwy = beta X displ + alpha
#  - for different number of cylinders
#  - extract fit summaries by name of the given summary

## prepare data
df.mpg <- mpg %>% 
  select(hwy, displ, cyl) %>% 
  mutate(cyl = as.factor(cyl))

## fit models with map (without using shortcut for anonymous function)
models <- df.mpg %>% 
  split(.$cyl) %>%                             # split data by different numbers of cylinders 
  map(function(df) lm(hwy ~ displ, data = df)) # map anonymous function to fit lines

models

## shorter syntax using shortcuts for anonymous functions
models <- df.mpg %>% 
  split(.$cyl) %>%                             # split data by different numbers of cylinders 
  map(~lm(hwy ~ displ, data = .)) # map anonymous function to fit lines

models


# Now extract R squared for each fitted model (line)

## longer syntax
models %>% 
  map(summary) %>%         # first we apply summary function to each model
  map_dbl(~.$"r.squared")  # then we extract only R squared value and save it to a vector

## shorter syntax using string for name of the component
models %>% 
  map(summary) %>%      # first we apply summary function to each model
  map_dbl("r.squared")  # then we extract only R squared value and save it to a vector


# Shortcut for extracting elements by position

## let's create a list of lists
list <- list(list(1:3,   4:6,   7:9),
             list(10:12, 13:15, 16:18),
             list(19:21, 22:24, 24:26))

## extract third element from each sub-list and put in a list
list %>% 
  map(3)



# 6.5 map over more than one argument


# map2() - apply function to pair of elements

## example:
##  - generate data from different normal distributions
##  - mu and sigma are different, n is fixed
##  - rnorm() is used to sample from normal distribution
##  - we will visualize distribution of sampled data
?rnorm

## distribution parameters:
mu <- c(0, -4, 5, 8) # mean
sig <- c(1, 2, 1, 3) # deviation

data <- map2(mu, sig, rnorm, n = 1000) %>%  # generate data (using vectors)
  enframe() %>%                             # convert list to data frame
  mutate(name = paste0("norm", 1:4)) %>%    # add name for each distribution
  unnest(cols = c(value))                   # extend or un-nest distribution values

data %>% count(name)

## visualize:
data %>% 
  ggplot(aes(x = value, 
             color = name, 
             fill = name)) +
  geom_density(size = 1.3, 
               alpha = .25) +
  theme_minimal()



# pmap() - apply function to groups of elements

## example:
##  - generate data from different normal distributions
##  - mu, sigma and n are different
##  - rnorm() is used to sample from normal distribution
##  - we will visualize distribution of sampled data

## distribution parameters:
n <- c(1000, 100, 1500, 10000) # number of samples
mu <- c(0, -4, 5, 8) # mean
sig <- c(1, 2, 1, 3) # deviation

## put arguments in a list 
## (different names as in rnorm, but proper argument position)
?rnorm
args <- list(n, mu, sig) # list of argument

pmap(args, .f = rnorm) %>% str() # generate data


## put arguments in a tibble 
## (the same names as in rnorm)
args <- tibble(mean = mu, sd = sig, n = n) # table of argument

## generate data
data <- args %>% 
  pmap(.l = ., .f = rnorm) %>%            # generate data (using vectors)
  enframe() %>%                           # convert list to data frame
  mutate(name = paste0("norm", 1:4)) %>%  # add name for each distribution
  unnest(cols = c(value))                 # extend or un-nest distribution values

data %>% count(name)

## visualize:
data %>% 
  ggplot(aes(x = value, 
             color = name, 
             fill = name)) +
  geom_density(size = 1.3, 
               alpha = .25) +
  theme_minimal()



# invoke_map() - apply list of functions to groups of elements

## example:
##  - generate data from different distributions (normal, uniform, exponential)
##  - each distribution has its own number and type of parameters
##  - rnorm() is used to sample from normal distribution
##  - runif() is used to sample from uniform distribution
##  - rexp() is used to sample from exponential distribution
##  - we will visualize distribution of sampled data
?rnorm
?runif
?rexp

## arguments
f <- c("rnorm", "runif", "rexp") # function names

par <- list(              # function parameters (list of lists)
  list(mean = 1, sd = 3),
  list(min = 0, max = 5),
  list(rate = 0.5)
)

## generate data
data <- invoke_map(f, par, n = 1000) %>% 
  enframe() %>%             # convert list to data frame
  mutate(name = f) %>%      # add name for each distribution
  unnest(cols = c(value))   # extend or un-nest distribution values

data %>% count(name)

## visualize:
data %>% 
  ggplot(aes(x = value, 
             color = name, 
             fill = name)) +
  geom_density(size = 1.3, 
               alpha = .25) +
  theme_minimal()



# walk() - call a function for its side effect

## simple example with print
l <- list(1:3, c(2,4,6))
l
map(l, print)
walk(l, print) # prints values from the list


## very practical example for exporting plots

## create plots with map (list of plots)
plots <- mpg %>% 
  split(.$manufacturer) %>% 
  map(~ggplot(., aes(displ, hwy)) + geom_point() + ggtitle(paste0(.$manufacturer)))

## create directory where plots will be stored
if(!dir.exists("./data/pwalk_plots")){
  dir.create("./data/pwalk_plots")
}

## export figures
list(str_c("./data/pwalk_plots/",names(plots), ".pdf"),  # list of pairs: plot path & plot
     plots) %>% 
  pwalk(., ggsave) # save figures



# 6.6 work with lists

## Let's create a list holding different types of elements

set.seed(123)

### single variables
l1 <- T
l2 <- F
s1 <- words %>% sample(1)
s2 <- words %>% sample(1)
n1 <- runif(1, 0, 1000)
n2 <- runif(1, 0, 1000)

### vectors
vec.l1 <- sample(c(T,F), size = round(runif(1, 1,100)), replace = T)
vec.l2 <- sample(c(T,F), size = round(runif(1, 1,100)), replace = T)
vec.s1 <- words %>% sample(size = round(runif(1, 1,100)), replace = T)
vec.s2 <- words %>% sample(size = round(runif(1, 1,100)), replace = T)
vec.n1 <- runif(1, 0, 1000) %>% sample(size = round(runif(1, 1,100)), replace = T)
vec.n2 <- runif(1, 0, 1000) %>% sample(size = round(runif(1, 1,100)), replace = T)

### tables
t1 <- mpg
t2 <- diamonds %>% sample_n(size = 500, replace = F)

### lists
list1 = list(a = 1, b = "b", vec = 1:10)
list2 = list(vec = seq(0,10,0.05), words = words[1:10]) 


## now put all together in a list
list <- list(l1 = l1,
             l2 = l2,
             s1 = s1,
             s2 = s2,
             n1 = n1,
             n2 = n2,
             vec.l1 = vec.l1,
             vec.l2 = vec.l2,
             vec.s1 = vec.s1,
             vec.s2 = vec.s2,
             vec.n1 = vec.n1,
             vec.n2 = vec.n2,
             t1 = t1,
             t2 = t2,
             list1 = list1,
             list2 = list2
             )

list
list %>% str()

## reshuffle list elements
list.shuffled <- list[sample(1:length(list), size = length(list), replace = F)]
list <- list.shuffled
rm(list.shuffled)



# pluck() - select element from a list

## grab 1st element
pluck(list, 1)

## grab element named "vec.l1"
pluck(list, "vec.l1") 



# keep() - select elements that pass a logical test

## check data types in a list
list %>% map(class) %>% unlist()

## keep all character types
keep(list, is.character)

## keep all logical types
keep(list, is.logical)

## keep all tibles
keep(list, is_tibble)

## keep all vectors that are logical (workaround with a pipe)
keep(list, is.atomic) %>% 
  keep(is.logical)



# discard() - select elements that do not pass a logical test

## discard all numeric type
discard(list, is.numeric)

## discard all vectors (also single vector like elements are discarded)
discard(list, is.character)



# head_while() / tail_while() - return elements until one does not pass

## return elements that are character type
head_while(list, is.character)
tail_while(list, is.character)



# flatten() - remove a level of indexes from a list

## flatten our list
list.f <- flatten(list)

## find if mpg tibble is also flattened into vectors
list.f %>% pluck("hwy")
list.f %>% pluck("manufacturer")



# transpose() - transpose the index order

## transpose our list
list.t <- transpose(list)
list.t
list.t %>% str()

## lets demonstrate transposing a list on a list of named vectors
v1 <- 1:10
v2 <- 11:20
v3 <- 21:30
names(v1) <- names(v2) <- names(v3) <- letters[1:10]

list1 <- list(v1, v2, v3)
list1

list1 %>% transpose() # transposed list



# 6.7 Summarise and join lists


# every() / some() - do all or some elements pass the test?

## are all elements atomic type?
every(list, is.atomic)

## are some elements atomic type?
some(list, is.atomic)


# has_element() - does list contain an element?
has_element(list, l1)
has_element(list, mpg)
has_element(list, diamonds)


# detect() / detect_index() - find first element or its index that pass a test
detect(list, is.numeric)
detect_index(list, is.numeric)


# vec_depth() - return depth number of indexes of given list
vec_depth(list)
vec_depth(list1)
vec_depth(list(a = 1, b = 1:2))
vec_depth(list(a = 1, b = 1:2, c = mpg))


# append() / prepend() - add elements to a list

## create two smaller lists
sublist1 <- list(pluck(list,1), pluck(list,2))
sublist2 <- list(pluck(list,3), pluck(list,4))
sublist1
sublist2

## at sublist2 to the end of sublist1
append(sublist1, sublist2)

## at sublist2 to the start of sublist1
prepend(sublist1, sublist2)


# splice() - combine object into a list

## use some elements generated before to create a list
splice(l1, vec.l1, list1)



# 6.8 Transform lists


# modify() / _at() / _if() - apply a function to list element

## apply typeof() to all elements of the list
modify(list, typeof)

## apply class() to all elements of the list
modify(list, class)

## apply nrow to table like elements
modify_if(list, is.data.frame, nrow) # only tibbles are affected

## apply length to selected numeric vector elements
modify_at(list, "vec.s2", length) # selected elements are affected


# cross2() - get all combinations of elements

combs <- cross2(letters[1:3], 1:2)
View(combs)


# reduce() - apply function recursively to each element

## lets create two lists
list.c <- list(sample(letters, 17),  # list containing 4 character vectors (each holds 17 random letters)
               sample(letters, 17),
               sample(letters, 17),
               sample(letters, 17))

list.c

list.n <- list(runif(n = 10),  # list containing 4 numeric vectors (each holds 10 random numbers sampled from uniform distribution)
               runif(n = 10),
               runif(n = 10),
               runif(n = 10))
list.n

## now find intersecting characters for all character vectors - letters present in all vectors!
reduce(list.c, intersect)

## now calculate a cumulative sum for numeric vectors 
reduce(list.n, sum)


# accumulate() - similar to reduce but keeps score track of mid-step operations
accumulate(list.c, intersect)
accumulate(list.n, sum)



# 6.9 Nested data

## Let's create a nested table from our slides
df <- mpg %>% 
  filter(manufacturer %in% c("jeep", "land rover", "lincoln")) %>% 
  select(manufacturer, model, displ, cyl, hwy)

df

df.n <- df %>% 
  dplyr::group_by(manufacturer) %>% # create grouping variable
  tidyr::nest() # execute the nesting

df.n
View(df.n)

## Now reverse operation - unnest
df1 <- df.n %>% 
  tidyr::unnest(cols = c(data)) %>%  # unnest
  ungroup() # do not forget to ungroup!

## check if identical
identical(df, df.n) # original df VS nested df
identical(df, df1)  # original df VS nested and then unnested df


# Operations that go with nesting

## count number of rows of nested tibbles
df.n$data %>% map(.x = ., .f = ~length(.$model))

## average value for hwy
df.n$data %>% map(.x = ., .f = ~mean(.$hwy))


# Nesting a larger data frame
mpg %>% 
  group_by(manufacturer) %>% 
  nest()

## can use more than one grouping variable
diamonds %>% 
  group_by(cut, color) %>% 
  nest() %>% 
  mutate(`avg price` = map(data, ~mean(.$price)),
         `nr diamonds` = map(data, ~length(.$price))) %>% 
  mutate(`avg price` = unlist(`avg price`),
         `nr diamonds` = unlist(`nr diamonds`))



# 6.10 Nested data workflow

## Modeling steps involved:
## - we will use mpg data set
## - for each manufacturer we will fir a separate linear model
## - with model we would like to predict variable hwy (y)
## - model will use two input variables: displ (x1), cyl (x2)
## - general model structure: hwy = a1 X displ + a2 x cyl + a0
## - all models mus be nested inside nested data frame
## - extract model's estimated parameters, and r squared value in a separate columns

## model fit
df.models <- mpg %>% 
  group_by(manufacturer) %>%                   # grouping variable
  nest() %>%                                   # nest data frame
  mutate(model = map(.x = data,                # fit a linear model with map
                     .f = ~lm(hwy~displ + cyl, 
                              data = .)))
View(df.models)
df.models

## extract model estimated coefficients - demonstration on a single model
model <- df.models %>% filter(manufacturer=="audi") %>% pull(model)
model
model %>% flatten()
model %>% flatten() %>% names()
model %>% flatten() %>% pluck(coefficients)
model %>% flatten() %>% pluck(coefficients) %>% enframe()
model %>% flatten() %>% pluck(coefficients) %>% .[[1]] # intercept a0
model %>% flatten() %>% pluck(coefficients) %>% .[[2]] # a1
model %>% flatten() %>% pluck(coefficients) %>% .[[3]] # a2

## extract model summary
model %>% map(summary) %>% map_dbl("r.squared")


## now do extraction of coefficients and summary-r.squared in our nested list

### we will first defined a function that will extract each coefficitent
extract_coef_ <- function(model){coefficients(model)[[1]]}
extract_coef  <- function(model, id_coef){coefficients(model)[[id_coef]]}

## now do the extraction
df.models <- df.models %>% 
  mutate(summary      = map(.x = model, .f = summary),             # extract model summary
         `r squared` = map_dbl(.x = summary, .f = "r.squared"),   # from summary extract r squared as numeric (not list)
         `coef a0`    = map_dbl(.x = model, .f = extract_coef, 1), # extract values for coefficient a0
         `coef a1`    = map_dbl(.x = model, .f = extract_coef, 2), # extract values for coefficient a1
         `coef a2`    = map_dbl(.x = model, .f = extract_coef, 3)) # extract values for coefficient a3


## we can take a closer look to models with r squared = 0
df.models %>% 
  filter(`r squared` == 0)

df.models %>% 
  filter(`r squared` == 0) %>% 
  select(manufacturer, data) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  # create a scarret plot
  ggplot(aes(x = displ, 
             y = hwy, 
             color = as.factor(cyl))) +
  geom_point() +
  facet_wrap(. ~ manufacturer)
  

## we can take a closer look to model with highest r squared = 0
df.models %>% 
  arrange(desc(`r squared`)) %>% 
  head(1)
  
df.models %>% 
  arrange(desc(`r squared`)) %>% 
  head(1) %>% 
  select(manufacturer, data) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  # create a scarret plot
  ggplot(aes(x = displ, 
             y = hwy, 
             color = as.factor(cyl))) +
  geom_point() +
  facet_wrap(. ~ manufacturer)



# 6.11 purrr practices

# some additional tips and tricks when it comes to purr


# loop over columns in a data frame:
#  - missing values count
#  - get class of the column/variable
#  - number of distinct values
#  - preserve column names
# (very useful  for initial table exploration)
mpg %>% 
  # map functions over columns and store result inside a data frame
  map_df(.x = ., 
         .f = ~(data.frame(missing_values = sum(is.na(.x)),
                           disitnct_values = n_distinct(.x),
                           class = class(.x))),
         .id = "variable") # preserve column names


# import multiple files into R with map()
#  - for example you must import many flat files
#  - files have similar structure
#  - you can first read the file names into an object
#  - then import them using map() 


## import .csv files inside a single directory
## mpg data set: ./data/mpg_single_level/[manufacturer].csv

### import multiple files
df1 <- tibble(directory = "./data/mpg_single_level",      # main directory
       files = list.files("./data/mpg_single_level")) %>% # list of .csv file
  mutate(path = str_c(directory, files, sep = "/")) %>%   # create path string for import
  mutate(data = map(.x = path,                            # import files with readr & map
                    .f = function(path_){read_csv(path_,
                                                  col_types = cols(.default = "c")) # all columns parsed as "character" for simplicity
                      })) %>%   
  pull(data) %>%              # pull data column
  bind_rows()                 # bind tibbles as list elements (we remove list here)


### import multiple files and specify column type
df2 <- tibble(directory = "./data/mpg_single_level",             # main directory
              files = list.files("./data/mpg_single_level")) %>% # list of .csv file
  mutate(path = str_c(directory, files, sep = "/")) %>%          # create path string for import
  mutate(data = map(.x = path,                                   # import files with readr & map : specify column type
                    .f = function(path_){read_csv(path_,
                                                  col_types = cols(
                                                    manufacturer = col_character(),
                                                    model = col_character(),
                                                    displ = col_double(),
                                                    year = col_integer(),
                                                    cyl = col_integer(),
                                                    trans = col_character(),
                                                    drv = col_character(),
                                                    cty = col_double(),
                                                    hwy = col_double(),
                                                    fl = col_character(),
                                                    class = col_character()))})) %>%   
  pull(data) %>%  # pull data column
  bind_rows()     # bind tibbles as list elements (we remove list here)



## import .csv files inside a two level directory
## mpg data set: ./data/mpg_double_level/[manufacturer]/[model].csv

df3 <- tibble(directory = "./data/mpg_double_level",       # main directory
              files = list.files("./data/mpg_double_level" # list files
                                 , recursive = T)) %>%     # now we use recursive mode to dig deeper 
  mutate(path = str_c(directory, files, sep = "/")) %>%    # create path string for import
  mutate(data = map(.x = path,                             # import files with readr & map
                    .f = function(path_){read_csv(path_,
                                                  col_types = cols(.default = "c")) # all columns parsed as "character" fro simplicity
                    })) %>%   
  pull(data) %>%            # pull data column
  bind_rows()               # bind tibbles as list elements (we remove list here)



# export multiple tibbles into multiple into .csv files with map()
#  - for example you must export data frame into multiple files
#  - files have similar structure
#  - first we prepare the smaller tibbles
#  - then export them using map() and nested data frame

## for example we will split mpg data set per each car level
##  - we will export each car in a .csv file named: [manufacturer]_[model]_car[id].csv
##  - create a directory for the export

directory <- "./data/mpg_export/" # path to directory

if(!dir.exists(directory)){
  dir.create(directory)
}


df.export <- mpg %>% 
  # add car id per manufacturer and model
  group_by(manufacturer, model) %>% 
  mutate(car_id = row_number()) %>% 
  ungroup() %>% 
  # add path for each file
  mutate(path = paste0(directory, 
                       manufacturer, "_", 
                       str_remove_all(model, " "), "_", 
                       car_id, ".csv")) %>% 
  # nest data rows for each car
  select(-car_id) %>% 
  group_by(path) %>% 
  nest() %>% 
  ungroup() 

## create a list of data and file path for pmap to export files
list(x = df.export$data,          # tibbles - one row in size
     file = df.export$path) %>%   # path to where write .csv files
  pmap(.l = ., .f = write_csv, ) %>%   # do the export
  quietly()  # purrr's function to suppress output to the console !!!


# draw multiple plots per one tibble with map()
# and export plots into separate .png files
#  - first draw plots and store into a data frame list
#  - then export plots on your disk

##  map ggplot function per manufacturer!
##  plots are stored as a list column!
df.plots <- mpg %>% 
  group_by(manufacturer) %>% 
  nest() %>%
  # drawing plots for each manufacturer
  mutate(plot = map(.x = data, 
                    .f = ~ggplot(., aes(x = displ, 
                                        y = hwy,
                                        color = as.factor(cyl))) + 
                      geom_jitter(size = 3) +
                      scale_color_viridis_d(option = "magma")
                    ))

## show a single plot
df.plots %>% pull(plot) %>% pluck(1)

## check if directory exists for storing the plots  
directory <- "./data/mpg_plot_export/" # path to directory

## create directory if neccessary
if(!dir.exists(directory)){
  dir.create(directory)
}

## add a path for each plot
df.plots <- df.plots %>% 
  mutate(plot_path = paste0(directory, manufacturer, ".png"))
  
## create a list of plot path and plot  for pmap to export plots
list(plot     = df.plots$plot,            # plot object
     filename = df.plots$plot_path) %>%   # path to where write .png files
  pmap(.l = ., .f = ggsave) %>%           # do the export
  quietly()  # suppress console output!!!

