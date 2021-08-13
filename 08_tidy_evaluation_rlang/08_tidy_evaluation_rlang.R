# 8 Tidy Evaluation: rlang

rm(list = ls())
graphics.off()

# Install packages and load package
install.packages("rlang")
library(tidyverse)
library(rlang)



# 8.3 Programming recipes


# Program with quoting function

## function:
data_median <- function(data, var){
  require(dplyr)
  
  var <- rlang::enquo(var)
  
  data %>% 
    summarise(median = median(!!var))
}

## function call:
data_median(mpg, hwy)
data_median(diamonds, price)



# Multiple arguments in quoting function

## function:
group_median <- function(data, var, ...){
  require(dplyr)
  
  var <- rlang::enquo(var)
  group_vars <- rlang::enquos(...)
  
  data %>% 
    group_by(!!!group_vars) %>% 
    summarise(median = median(!!var))
}

## function call:
group_median(mpg, hwy, model)
group_median(mpg, hwy, manufacturer, model)



# Argument names of a quoting function

## function:
named_median <- function(data, var, name){
  require(dplyr)
  
  var  <- rlang::enquo(var)
  name <- rlang::ensym(name)
  
  data %>% 
    summarise(!!name := median(!!var))
}

## function call:
named_median(mpg, hwy, median_hwy)
named_median(mpg, hwy, "median_hwy")
named_median(diamonds, price, "median_price")



# 8.4 Write function: dplyr


# summary - function
my_summary <- function(df, var, ...){
  require(rlang)
  require(dplyr)
  
  var <- enquo(var) # variable for which statics summary is calculated
  group_vars <- enquos(...) # grouping variables
  
  df %>% 
    group_by(!!!group_vars) %>% 
    summarise(min = min(!!var),
              max = max(!!var),
              med = median(!!var),
              mean = mean(!!var),
              sd = sd(!!var),
              range = max - min
              )
}

my_summary(mpg, hwy, model)


# count frequencies - function
count_freq <- function(df, ...){
  require(rlang)
  require(dplyr)
  
  group_vars <- enquos(...) # grouping variables
  
  df %>% 
    group_by(!!!group_vars) %>% 
    summarise(freq = n()) %>% 
    ungroup()
}

count_freq(mpg)
count_freq(mpg, manufacturer)
count_freq(diamonds, color)


# moving average - function

## create some daily data 
## (daily Covid 19 infections)
df.infections <- tibble(date = seq.Date(from = as.Date("2021-01-01"),
                                        to   = as.Date("2021-01-31"), 
                                        by = "day"),
                        inf = c(100, 120, 60, 20, 
                                180, 160, 150, 140, 180, 100, 50,
                                190, 170, 150, 180, 200, 120, 70,
                                180, 180, 200, 200, 220, 150, 100,
                                250, 300, 280, 290, 350, 200))

## we would like to have X day smoothed average number of infected people
## (we are calculating moving average of variable inf)
## our function will return a data frame:
## - where moving average for 3,5,7 days is added tot he table
moving_average_infections <- function(df = df.infections, var = inf){
  require(rlang)
  require(dplyr)
  require(tidyr)
  
  var <- enquo(var) # variable for which moving average is calculated
  
  df %>% 
    # add lag values from t-1 up to t-6
    mutate(x1 = lag(!!var, 1),
           x2 = lag(!!var, 2),
           x3 = lag(!!var, 3),
           x4 = lag(!!var, 4),
           x5 = lag(!!var, 5),
           x6 = lag(!!var, 6)) %>% 
    # replace NA with 0
    mutate_at(.tbl = ., .vars = paste0("x", 1:6), .funs = replace_na, 0) %>% 
    # calculate moving averages
    mutate(`3 day avg` = (!!var + x1 + x2) / 3,
           `7 day avg` = (!!var + x1 + x2 + x3 + x4 + x5 + x6) / 7)
   
}

df.infections1 <- moving_average_infections()

## visualize
df.infections1 %>% 
  filter(date > "2021-01-05") %>% 
  select(date, inf, `3 day avg`,  `7 day avg`) %>% 
  pivot_longer(cols = c("inf", "3 day avg", "7 day avg"), 
               names_to = "variable", 
               values_to = "value") %>% 
  ggplot(aes(x = date,
             y = value,
             color = variable)) +
  geom_line()



# 8.5 Write function: ggplot2


# generic histogram - function

## function
plot_histogram <- function(df, x){
  require(ggplot2)
  require(dplyr)
  
  x_var <- enquo(x) # variable for which histogram is drawn
  
  df %>% 
    ggplot(aes(x = !!x_var)) +
    geom_histogram()
}

## function call:
plot_histogram(mpg, hwy)
plot_histogram(mpg, hwy) + ggtitle("My histogram")


# generic scatter plot - function

## function
plot_scatter <- function(df, x, y){
  require(ggplot2)
  require(dplyr)
  
  x_var <- enquo(x) # variable x-axis
  y_var <- enquo(y) # variable y-axis
  
  df %>% 
    ggplot(aes(x = !!x_var,
               y = !!y_var)) +
    geom_point()
}

## function call:
plot_scatter(df = sample_n(diamonds, size = 10000), x = carat, y = price)


# generic scatter plot & customized theme - function

## theme
theme_fonts <- theme(
  plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
  axis.title = element_text(size = 16, face = "italic", hjust = 0.5),
  axis.text = element_text(size = 14),
)

## function
plot_scatter <- function(df, x, y, color,
                         title="", x_title="", y_title=""){
  require(ggplot2)
  require(dplyr)
  
  x_var <- enquo(x) # variable x-axis
  y_var <- enquo(y) # variable y-axis
  col_var <- enquo(color) # color variable
  
  df %>% 
    ggplot(aes(x = !!x_var,
               y = !!y_var,
               color = !!col_var)) +
    geom_point() +
    ggtitle(title) +
    xlab(x_title) +
    ylab(y_title) +
    theme_minimal() +
    theme_fonts
}

## function call:
plot_scatter(df = sample_n(diamonds, size = 10000), 
             x = carat, y = price, color = color,
             title = "Diamond price / carat / color", 
             x_title = "Diamond carat", 
             y_title = "Diamond price in USD")
