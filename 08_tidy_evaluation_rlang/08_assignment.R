# 8 Tidy Evaluation: rlang


rm(list = ls())
graphics.off()

# Libraries
library(tidyverse)
library(rlang)


# Exercise 1

# Create a function called count_freq that:
# - takes a data frame
# - creates frequency counts
# - before counting is applied do grouping (use only one grouping variable)
# - at the end rename variables in the output table
# - grouping variable should be called "x"
# - frequencies should be called "freq"

## Function:
count_freq <- function(df, x_group){
  require(rlang)
  require(dplyr)
  
  x_group_var <- enquo(x_group) # grouping variable
  
  counts <- df %>% 
    group_by(!!x_group_var) %>% 
    summarise(freq = n()) %>% 
    ungroup()
  
  colnames(counts) <- c("x", "freq")
  return(counts)
}

count_freq(df = mpg, x_group = manufacturer)



# Exercise 2

# Create a function called draw_bar_plot that:
# - takes the output of function count_freq (Exercise 1)
# - and draws bar plot
# - where variable "x" is used on x axis
# - and variable "freq" is used on y axis
# - you can use geom_col instead or geom_bar (your choice)


## Function:
draw_bar_plot <- function(df = count_freq(mpg, manufacturer),
                          x = x, y = freq){
  require(rlang)
  require(dplyr)
  require(ggplot2)
  
  x_var <- enquo(x) # variable x-axis
  y_var <- enquo(y) # variable y-axis
  
  df %>% 
    arrange(desc(!!y_var)) %>% 
    ggplot(aes(x = !!x_var,
               y = !!y_var)) +
    geom_col()
}

draw_bar_plot()


# Exercise 3

# Create a function called prepare_diamonds_data that:
# - takes diamonds data frame
# - randomly selects n diamonds
# - creates variable called "volume"
# - volume = x * y * z

## Function:
prepare_diamonds_data <- function(df = diamonds, n = 10000){
  require(rlang)
  require(dplyr)
  
  df %>% 
    sample_n(size = n) %>% 
    mutate(volume = x * y * z)
}

prepare_diamonds_data()


# Exercise 4

# Create a function called explore_diamonds that:
# - takes the output of function prepare_diamonds_data (Exercise 3)
# - and draws scatter plot
# - where variable "carat" is used on x axis
# - variable "price" is used on y axis
# - variable "volume" is used for point size
# - variable "color" is used for point color
# - add plot title and axis titles as function arguments
# - add custom plot theme inside the function

## Function:
explore_diamonds <- function(df = prepare_diamonds_data(), 
                             title = "Diamonds price, carat and volume",
                             x_title = "Diamond carat",
                             y_title = "Diamond price in USD"){
  require(ggplot2)
  require(dplyr)
  
  my_theme <- theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "italic", hjust = 0.5),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold")
  )
  
  
  df %>% 
    ggplot(aes(x = carat,
               y = price,
               size = volume,
               color = color)) +
    geom_jitter() +
    ggtitle(title) +
    xlab(x_title) +
    ylab(y_title) +
    theme_minimal() +
    my_theme
}

explore_diamonds()

