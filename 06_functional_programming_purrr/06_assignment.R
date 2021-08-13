# 6 Functional Programming: purrr


rm(list = ls())
graphics.off()

# Libraries
library(tidyverse)
library(cowplot)


# Exercise 1

# We will simulate data using R's built in random numbers generators (using different distribution types)
# - first import simulations blue print "simulations_blue_print.txt"
# - first column gives the name of the function for given distribution
# - remaining columns give arguments of each distribution (function)
# - we need to preprocess the text data to get proper inputs for simulations

## Import simulation blue print (function + arguments)
blueprint_raw <- read_csv("./data/simulations_blue_print.txt",  # import
                      col_names = F) %>% 
  rename(type = X1) # rename column

## create vector for type of distributions
type <- blueprint_raw %>% pull(type)

## split columns
blueprint_raw <- blueprint_raw %>% 
  separate(col = 1, into = c("f", "arg1", "arg2", "arg3"), sep = ";") # split into column: function & argument

## create vector of functions
f <- blueprint_raw %>% pull(f)

## create list of lists of arguments
args <- list() # empty list

for(r in 1:nrow(blueprint_raw)){
  args.row.text <- "" # arguments inside a string
  
  for(c in 2:ncol(blueprint_raw)){ # loop over each argument
    if(blueprint_raw[r,c] != ""){ # skip empty string!
      args.row.text <- str_c(args.row.text, blueprint_raw[r,c], sep = ",")
    }
  }
  
  args.row.text <- str_remove(args.row.text, pattern = "^,|,$") # remove unwanted commas "," start/end position
  args.row.text <- paste0("list(", args.row.text, ")")          # arguments as list
  eval(parse(text = paste0("args.row.list = ", args.row.text))) # convert to list and append to main argument list
  args <- c(args, list(args.row.list)) # we need to insert a list inside a list - list per function!
  
}
  
## do the simulations with invoke_map()
set.seed(123)

data.sim <- invoke_map(.f = f, .x = args) %>% # simulate
  enframe() %>%          # create compact tibble
  mutate(type = type,    # add columns
         f = f) %>% 
  unnest(cols = c(value)) %>%  # unnest
  select(f, type, value)       # rearrange columns
  


# Exercise 2

# Visualize generated data (simulations) using density plots:
# - create subplot for each function used to generate data (column f)
# - fill color indicates given function with selected argument values

## create subplots inside a nested tibble
plots <- data.sim %>% 
  group_by(f) %>% 
  nest() %>% 
  # drawing density plot for each distribution type
  mutate(plot = map(.x = data, 
                    .f = ~ggplot(., aes(x = value,
                                        fill = type)) +
                              geom_density(alpha = 0.3) +
                              scale_fill_viridis_d(option = "magma") +
                              ggtitle(paste0(f)) +
                              xlab("") +
                              theme_minimal()
                    ))
                              
## draw subplots
plot_grid(plotlist = plots %>% pull(plot), nrow = 3)



# Exercise 3

#  Import gapminder data 
#  - separate .csv file for given country
#  - create one tibble fro all countries

### import multiple files
gapminder <- tibble(directory = "./data/gapminder",             # main directory
                    files = list.files("./data/gapminder")) %>% # list of .csv file
  mutate(path = str_c(directory, files, sep = "/")) %>%         # create path string for import
  mutate(data = map(.x = path,                                  # import files with readr & map
                    .f = function(path_){read_csv(path_,
                                                  col_types = cols(.default = "c")) # all columns parsed as "character" 
                    })) %>%   
  pull(data) %>%              # pull data column
  bind_rows() %>%             # bind tibbles as list elements (we remove list here)
  # columns conversion
  mutate_at(.vars = c("country", "continent"), .funs = as.factor) %>% 
  mutate_at(.vars = c("year", "lifeExp", "pop", "gdpPercap"), .funs = as.numeric)


# Exercise 4

#  Use imported gapminder data
#  - we will draw line chart: year - x axis / lifeExp - y axis for each country
#  - create subplot for each continent 

## create subplots inside a nested tibble
plots <- gapminder %>% 
  group_by(continent) %>% 
  nest() %>% 
  # create plots
  mutate(plot = map(.x = data, 
                    .f = ~ggplot(., aes(x = year,
                                        y = lifeExp,
                                        color = country,
                                        group = country)) +
                      geom_line(show.legend = F) +
                      scale_color_viridis_d(option = "magma") +
                      scale_y_continuous(limits = c(0,100)) +
                      ggtitle(paste0("Continent - ", continent)) +
                      xlab("Year") +
                      ylab("Life expectancy") +
                      theme_minimal()
                      ))

## draw subplots
plot_grid(plotlist = plots %>% pull(plot))
