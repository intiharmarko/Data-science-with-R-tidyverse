# 5 Data Visualize: ggplot2

rm(list = ls())
graphics.off()

# Install packages and load package
library(tidyverse)


# 5.2 Visualize distribution of a continuous variable: histogram & density plot

# Histogram

#   Let's first generate some data - uniform continuous distribution (random numbers)
set.seed(11235)

df.unif <- runif(n = 100000, min = 0, max = 1) %>% 
  tibble(x = .)

#   Now visualize the distribution of random variable x
ggplot(data = df.unif, 
       mapping = aes(x = x)) +
  geom_histogram()

df.unif %>% 
  ggplot(aes(x = x)) +
  geom_histogram(bins = 45,  
                 color = "black",
                 fill = "deepskyblue3")


#   Normal continuous distribution (random numbers)
df.norm <- rnorm(n = 100000, mean = 0, sd = 1) %>% 
  tibble(x = .)

df.norm %>% 
ggplot(aes(x = x)) +
  geom_histogram(bins = 45,  
                 color = "black",
                 fill = "deepskyblue3")


#   Now use cars data set and check how values of variable hwy distribute
mpg %>% 
  ggplot(aes(x = hwy)) +
  geom_histogram(bins = 30,
                 color = "black") +
  xlab("Highway miles per gallon") +
  ylab("Number of cars (frequency)") +
  ggtitle("Distribution of variable - highway miles per gallon")



# Density plot

# Density plot of normally distributed variable
df.norm %>% 
  ggplot(aes(x = x)) +
  geom_density(adjust = .05,
               color = "black",
               fill = "deepskyblue3")

# Line parameters
df.norm %>% 
  ggplot(aes(x = x)) +
  geom_density(size = 1.2, 
               linetype = "dashed", 
               adjust = .25,
               color = "black",
               fill = "deepskyblue3")



#   Visualize normally distributed variables (different distribution parameters)
df.norm1 <- rnorm(n = 100000, mean = 0,  sd = 1) %>% tibble(x = .)
df.norm2 <- rnorm(n = 100000, mean = 1,  sd = 2) %>% tibble(x = .)
df.norm3 <- rnorm(n = 100000, mean = -2, sd = 3) %>% tibble(x = .)

ggplot() +
  # first density
  geom_density(data = df.norm1, aes(x = x), 
               color = "black", fill = "blue", 
               alpha = 0.2) +
  # second density
  geom_density(data = df.norm2, aes(x = x), 
               color = "black", fill = "red", 
               alpha = 0.2) +
  # third density
  geom_density(data = df.norm3, aes(x = x), 
               color = "black", fill = "green", 
               alpha = 0.2)


#   Now use cars data set and check how values of variables hwy and cty are distributed
ggplot() +
  geom_density(data = mpg, aes(x = hwy), 
               adjust = .75,
               color = "black", fill = "blue", 
               alpha = 0.2) +
  geom_density(data = mpg, aes(x = cty), 
               adjust = .75,
               color = "black", fill = "red", 
               alpha = 0.2) +
  xlab("Miles per gallon") +
  ylab("Density") +
  ggtitle("Distribution of variable - highway miles per gallon (blue) & city miles per gallon (red)")

# How to export a figure?
ggsave(filename = "./data/density_plot_mpg.png", 
       plot = last_plot(),
       units = "cm", width = 29, height = 21, dpi = 300)



# Area plot

#   Using "bins" - statistics
df.norm %>% 
  ggplot(aes(x = x)) +
  geom_area(stat = "bin", 
            binwidth = 0.1)

#   Using "density" - statistics
df.norm %>% 
  ggplot(aes(x = x)) +
  geom_area(stat = "density")

# Generate cumulative distribution and visualize it with area plot!
# - distribution where you show the probability that X <= x
# - X is a random variable | x is a fixed value
df.norm <- rnorm(n = 1000, mean = 0, sd = 1) %>% 
  tibble(x = .)

df.norm <- df.norm %>% 
  arrange(x) %>%    # sort x values
  mutate(count = 1, # add counts
         y = cumsum(count),  # running total or cumulative sum of counts
         y = y / sum(count)) # normalize values to get density - probability 
  
df.norm %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_area(fill = "deepskyblue3", 
            color = "black") +
  xlab("x") +
  ylab("Probability   -   P(X<=x)")

# Let's add some standard deviations
df.norm %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_area(fill = "deepskyblue3", 
            color = "black") +
  xlab("x") +
  ylab("Probability   -   P(X<=x)") %>% 
  geom_vline(xintercept = -3, linetype = "dashed", size = 1) +
  geom_vline(xintercept = -2, linetype = "dashed", size = 1) +
  geom_vline(xintercept = -1, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 2, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 3, linetype = "dashed", size = 1) 

# What are theoretical probabilities - we will use R
tibble(x = seq(-3,3,1),
       probs = pnorm(q = x, mean = 0, sd = 1, lower.tail = T))



# 5.3 Visualize distribution of a categorical variable: bar plot

# Bar plot

#   First generate some data
groups <- paste("group", 1:4, sep = " ") # group names
probs <- c(.2, .3, .4, .1) # probability that unit from  each group is randomly chosen
sum(probs)

set.seed(123)
df.data <- sample(groups, 
                  size = 1000, 
                  replace = T, 
                  prob = probs) %>% 
  tibble(group = .)

# Draw bar plot - stat = "count"
# - ggplot2 calculates the frequencies / counts
df.data %>% 
  ggplot(aes(x = group)) +
  geom_bar(stat = "count",
           color = "black")


# Draw bar plot - stat = "identity"
# - we calculates the frequencies / counts
df.data %>%
  # count frequencies
  group_by(group) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  # do the plotting
  ggplot(aes(x = group,
             y = freq)) +
  geom_bar(stat = "identity",
           color = "black")

# Add fill colors
df.data %>% 
  ggplot(aes(x = group,
             fill = group)) +
  geom_bar(stat = "count",
           color = "black")


# Alter fill colors - scaling:

# a) manual colors
df.data %>% 
  ggplot(aes(x = group,
             fill = group)) +
  geom_bar(stat = "count",
           color = "black") +
  scale_fill_manual(values = c("red", "green", "blue", "gray"))

# b) brewer - pallete
df.data %>% 
  ggplot(aes(x = group,
             fill = group)) +
  geom_bar(stat = "count",
           color = "black") +
  scale_fill_brewer(palette = 3)

# c) viridis - pallete
df.data %>% 
  ggplot(aes(x = group,
             fill = group)) +
  geom_bar(stat = "count",
           color = "black") +
  scale_fill_viridis_d(option = "inferno")


# Add labels on top of each column
df.data %>%
  group_by(group) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = group,
             y = freq,
             fill = group)) +
  geom_bar(stat = "identity",
           color = "black") +
  # add labels - with geom_text
  geom_text(aes(label = freq,   # map label variable
                y = freq + 10), # shift label position up to avoid overlap!
            size = 12)          # size of 



# Now use car dataset and:
#  - count number of cars for each manufacturer
#  - calculate counts and percentages %
#  - create a bar plot: showing number of cars per manufacturer
#  - each manufacturer must have its own fill color
#  - add column labels: [number of cars]  | [percentage] %
#  - columns must be sorted - from highest to lowest!
#  - do not show legend!
figure <- mpg %>% 
  # number of cars per manufacturer
  group_by(manufacturer) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  # add percentages and create labels
  mutate(percentage = round(n / sum(n) * 100, 1),
         label = paste(n, " | ", percentage, "%", sep = "")) %>% 
  # order manufacturer variable - as factor variable
  arrange(desc(n)) %>% 
  mutate(manufacturer = as.factor(manufacturer),
         manufacturer = fct_inorder(manufacturer)) %>% 
  # create plot
  ggplot(aes(x = manufacturer,
             y = n,
             fill = manufacturer)) +
  geom_bar(stat = "identity", 
           show.legend = F,    # legend turned off
           color = "black") +  
  geom_text(aes(label = label, # add labels
                y = n + 1),
            size = 6) +
  scale_fill_viridis_d(option = "inferno", # scale fill colors
                       direction = -1) +
  xlab("Car manufacturer") +
  ylab("Car count") +
  ggtitle("Number of cars per each manufacturer")

# export final plot
ggsave(filename = "./data/cars_per_manufacturer.png", 
       plot = figure, 
       units = "cm", 
       width = 40, height = 21, 
       dpi = 500)



# 5.4 Visualize relation of 2 continuous variables: scatter plot

# Scatter plot

#   Can we show some relation between hwy & cty in cars dataset?
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_point()


# Change color, shape & size of points
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_point(color = "red",
             size = 5,
             shape = 17)


# Add statistic layer - regression line
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = F)


# Alter axis variables scales
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 50, 2.5), 
                     limits = c(0, 50)) +
  scale_y_continuous(breaks = seq(0, 50, 5), 
                     limits = c(0, 50))


# Diamonds dataset 
#  - aprox. 50k different diamonds prices
#  - we will randomly select 10k diamonds
#  - Are diamond carat and diamond related?
set.seed(123)

df.diamonds <- diamonds %>% sample_n(size = 10000, replace = F)

df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point()


# Include point transparency 
df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point(size = 3,
             alpha = 1/5)


# Alter y-axis scale to transform nonlinear trend to linear trend
# - we can try square root or logarithmic (base 10) transformation
df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point() +
  scale_y_sqrt()

df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() 


# Adding smoothing line - auto detect 
df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point() +
  geom_smooth()



# 5.5 Visualize relation of 2 categorical variables: bar plot & scatter plot

# Bar plot

# Use car dataset and:
#  - With bar plot show the distribution of cars
#  - break down by car manufacturer and car class
#  - use "stacked" bar plot
mpg %>% 
  ggplot(aes(x = manufacturer,
             fill = class)) +
  geom_bar(position = "stack",
           color = "black") +  
  scale_fill_viridis_d() +
  xlab("Car manufacturer") +
  ylab("Car count") +
  ggtitle("Number of cars per each manufacturer break down by class")

# Bar plot with position = "dodge"
mpg %>% 
  ggplot(aes(x = manufacturer,
             fill = class)) +
  geom_bar(position = "dodge",
           color = "black") +  
  scale_fill_viridis_d() +
  xlab("Car manufacturer") +
  ylab("Car count") +
  ggtitle("Number of cars per each manufacturer break down by class")

# Bar plot with position = "fill"
mpg %>% 
  ggplot(aes(x = manufacturer,
             fill = class)) +
  geom_bar(position = "fill",
           color = "black") +  
  scale_fill_viridis_d() +
  xlab("Car manufacturer") +
  ylab("Relative car count (%)") +
  ggtitle("Cars per each manufacturer break down by class")


# Scatter plot

#   No colors altered
mpg %>% 
  ggplot(aes(x = manufacturer,
             y = class)) +
  geom_point(position = "jitter") +  
  xlab("Car manufacturer") +
  ylab("Car class") +
  ggtitle("Cars per each manufacturer and class")

#   Colors altered
mpg %>% 
  ggplot(aes(x = manufacturer,
             y = class,
             color = class)) +
  geom_point(position = "jitter",
             size = 3) + 
  scale_color_viridis_d() +
  xlab("Car manufacturer") +
  ylab("Car class") +
  ggtitle("Cars per each manufacturer and class")

#   alternative - geom_jitter
mpg %>% 
  ggplot(aes(x = manufacturer,
             y = class,
             color = class)) +
  geom_jitter(size = 3) + 
  scale_color_viridis_d() +
  xlab("Car manufacturer") +
  ylab("Car class") +
  ggtitle("Cars per each manufacturer and class")


# Diamonds data set
# - visualize distribution of diamonds
# - considering diamond cut & diamond color
# - use bar plot & scatter plot
df.diamonds %>% 
  ggplot(aes(x = color,
             fill = cut)) +
  geom_bar(position = "stack",
           color = "black") +  
  scale_fill_viridis_d() +
  xlab("Diamond color") +
  ylab("Number of diamonds") +
  ggtitle("Number of diamonds per each diamond color break down by diamond cut")
  
df.diamonds %>% 
  ggplot(aes(x = color,
             y = cut,
             color = cut)) +
  geom_jitter(size = 2) +  
  scale_color_viridis_d() +
  xlab("Diamond color") +
  ylab("Diamond cut") +
  ggtitle("Diamonds per each diamond color and diamond cut")



# 5.6 Combine continuous and categorical variable: box plot & violin plot

# Box plot

# If we would like to answer a question: are there great differences in highway consumption for different classes of cars?
# We can use box plot to show us the underlying truth in the data.
mpg %>% 
  ggplot(aes(x = class,
             y = hwy)) +
  geom_boxplot()

# Are there differences in diamond prices, if we compare different diamond colors?
df.diamonds %>% 
  ggplot(aes(x = color,
             y = price,
             fill = color)) +
  geom_boxplot() +
  scale_fill_viridis_d()
  scale_y_log10()
  
  
# Tweaking parameters of box plot
  mpg %>% 
    ggplot(aes(x = class,
               y = hwy)) +
    geom_boxplot(fill = "brown1", 
                 outlier.colour = "blue", 
                 outlier.size = 5,
                 size = 1.3)

  
# Violin plot
  
# Cars data set  
mpg %>%
  ggplot(aes(x = class,
             y = hwy)) +
  geom_violin()

# Diamonds data set
df.diamonds %>% 
  ggplot(aes(x = color,
             y = price,
             fill = color)) +
  geom_violin() +
  scale_fill_viridis_d()


# Tweaking parameters of violin plot
mpg %>%
  ggplot(aes(x = class,
             y = hwy)) +
  geom_violin(fill = "deepskyblue2", 
              #scale = "area"        # all violins with the same area
              scale = "count"        # areas proportional to number of observations
              )


# Modify theme layer

## Built in themes
mpg %>% 
  ggplot(aes(x = class,
             y = hwy)) +
  geom_boxplot() +
  #theme_bw()
  #theme_classic()
  #theme_dark()
  #theme_minimal()
  theme_void()


## Custom theme modifications
mpg %>% 
  ggplot(aes(x = class,
             y = hwy,
             fill = class)) +
  geom_boxplot() +
  xlab("Car class") +
  ylab("Highway miles per gallon (MPG)") +
  ggtitle("Car fuel consumption by class") +
  # theme layer:
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 25, 
                                  face = "bold", 
                                  hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)), 
        legend.background = element_rect(fill = "grey", 
                                         colour = "black", 
                                         linetype = "dashed"),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 14, 
                                   face = "italic"),
        legend.title = element_text(size = 20, 
                                    face = "bold")
        )

ggsave(filename = "./data/cars_box_plot.png", 
       plot = last_plot(), 
       width = 29, height = 21, units = "cm", 
       dpi = 300)



# 5.7 Visualize multiple variables using only one plot


# Splitting plots into smaller subplots ~ facetting

# facet_wrap - wrap 1D ribbon of panels into 2D (sequence of panels) 

## Split hwy VS cty by car class
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_jitter() +
  facet_wrap(vars(class),
             scales = "free")
  #facet_wrap(. ~ class)

## Split hwy VS cty by car class and drive type
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_jitter() +
  facet_wrap(class ~ drv)
  #facet_wrap(drv ~ class)


# facet_grid - layout panels in a grid (matrix of panels)

## Split hwy VS cty by car class - rows
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_jitter() +
  facet_grid(rows = vars(class))
  #facet_grid(class ~ .)

## Split hwy VS cty by car class - columns
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_jitter() +
  facet_grid(cols = vars(class))
  #facet_grid(. ~ class)
  
## Split hwy VS cty by car class and drive type
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_jitter() +
  facet_grid(class ~ drv)
  #facet_grid(drv ~ class)
  

# Adding multiple features

## 2 continuous vars + point size = continuous
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy,
             size = cyl)) +
  geom_jitter() +
  scale_size(range = c(1,10)) # scale point size

## 2 continuous vars + point color = continuous
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy,
             color = cyl)) +
  geom_jitter() +
  scale_color_viridis_c()

## 2 continuous vars + point color = categorical
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy,
             color = class)) +
  geom_jitter() +
  scale_color_viridis_d()

## 2 continuous vars + point shape = categorical
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy,
             shape = drv,
             color = drv)) +
  geom_jitter(size = 3) +
  scale_color_viridis_d()


# Final plot - multiple features ~ diamonds
df.diamonds %>% 
  ggplot(aes(x = carat, 
             y = price, 
             color = cut)) +
  geom_point(position = "jitter") +
  facet_grid(color ~ clarity, 
             scales = "free", 
             labeller = "label_both") +
  scale_y_continuous(breaks = seq(0,20000,5000)) +
  scale_color_viridis_d(option = "viridis") +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ carat ~ cut ~ clarity ~ color - scatter plot") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./data/scatterplot_diamonds_multiple_elements.png", 
       plot = last_plot(),
       units = "cm", width = 40, height = 25, dpi = 600) 



# 5.8 Visualize time series

# Line chart

#   We will use economics data set from ggplo2 package

# Visualize yearly unemployment
economics %>% 
  ggplot(aes(x = date,
             y = unemploy)) +
  geom_line()

# Visualize multiple time series - wide format of table
economics %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = unemploy), color = "brown1") +
  geom_line(aes(y = pce),      color = "deepskyblue2") +
  geom_line(aes(y = psavert),  color = "green")

# Changing parameters
economics %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = unemploy), color = "brown1", size = 1.2, linetype = "dashed") +
  geom_line(aes(y = pce),      color = "deepskyblue2", size = 1.2, linetype = "longdash") +
  geom_line(aes(y = psavert),  color = "green", size = 1.2, linetype = "dotdash") +
  scale_y_log10()

# Visualize multiple time series - long format of table
economics_long %>% 
  ggplot(aes(x = date,
             y = value,
             group = variable,
             color = variable)) +
  geom_line()


# x - labels (date) changes
economics_long %>% 
  filter(variable != "pop") %>% 
  ggplot(aes(x = date,
             y = value,
             group = variable,
             color = variable)) +
  geom_line() +
  # scale_x_date(date_breaks = "year")
  # scale_x_date(date_breaks = "5 years")
  # scale_x_date(date_breaks = "10 years")
  # scale_x_date(date_breaks = "month")
  # scale_x_date(date_breaks = "10 years", date_labels = "%Y") # show only year
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") # show only year



# 5.9 Create heatmaps maps word clouds

# Heat map

#   Let's create heat map for cars data set
#   - manufacturer VS car class and number of cars
mpg %>% 
  group_by(manufacturer, class) %>% 
  summarise(cars = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = class,
             y = manufacturer,
             fill = cars)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma")

#   - manufacturer VS car class and max hwy
mpg %>% 
  group_by(manufacturer, class) %>% 
  summarise(`hwy mean` = mean(hwy)) %>% 
  ungroup() %>% 
  ggplot(aes(x = class,
             y = manufacturer,
             fill = `hwy mean`)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma")



# Word clouds

# We need to install an additional package
install.packages("ggwordcloud")
library(ggwordcloud)


# create word cloud for car model from the cars dataset
df.cars <- mpg %>% 
  group_by(model, manufacturer) %>% 
  count()

# create word cloud
#   add text size - number of cars
#   determine max text size
set.seed(135) #randomness in positioning labels in the cloud
df.cars %>% 
  ggplot(aes(label = model, 
             size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()

#   include word rotation 
df.cars <- df.cars %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1)))

#   final word cloud
df.cars %>% 
  ggplot(aes(label = model, 
             size = n, 
             angle = angle1,
             color = manufacturer)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  scale_color_viridis_d(option = "magma") +
  theme_minimal()

ggsave(filename = "./data/word_cloud.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Map

#   We will use R's built in data set to show crime rate for US states

#   Create crime map data
df.crime <- USArrests %>% 
  mutate(region = tolower(rownames(.))) %>% 
  # left and other join will be covered in the following lessons!!!
  left_join(x = .,  # merge crime data set with map information
            y = map_data("state"),
            by = "region")

#   Draw the map
df.crime %>% 
ggplot(aes(x = long, 
           y= lat, 
           group = group))+
  geom_polygon(aes(fill = Assault), 
               color = "white")+
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal()



# 5.10 Create subplots

# We need to install an additional package
install.packages("cowplot")
library(cowplot)

# First create some smaller plots - cars data set
p1 <- ggplot(mpg, aes(x = cty, y = hwy))   + geom_jitter()
p2 <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_jitter()
p3 <- ggplot(mpg, aes(x = cyl, y = hwy))   + geom_jitter()
p4 <- ggplot(mpg, aes(x = drv, y = cty))   + geom_jitter()
p5 <- ggplot(mpg, aes(x = trans, y = hwy)) + geom_jitter()
p6 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_jitter()

# Create subplots
plot_grid(p1, p2, p3, p4)

#   Add labels 
plot_grid(p1, p2, p3, p4, labels = "AUTO") # automatic labels
plot_grid(p1, p2, p3, p4, labels = c("p1", "p2", "p3", "p4")) # custom labels

#   Change subplot configuration
plot_grid(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)
plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)
plot_grid(p1, p2, p3, p4, p5, p6, nrow = 6, ncol = 1)
plot_grid(p1, p2, p3, p4, p5, p6, nrow = 1, ncol = 6)
