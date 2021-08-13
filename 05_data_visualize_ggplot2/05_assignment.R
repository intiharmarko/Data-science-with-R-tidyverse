# 5 Assignment - Data Visualize: ggplot2


rm(list = ls())
graphics.off()


# Libraries
library(tidyverse)
library(lubridate)
library(cowplot)
library(hflights)
library(ggwordcloud)


# Exercise 1

# Find top 4 carriers (total number of flights)
# - now draw density plot for variable "distance"
# - each carrier has its own density curve
# - fill density area with different colors
# - scale fill colors with "Viridis" color palette
# - all densities are drawn in the same plot
# - use transparency for fill colors
# - remove the outliers: distance < 2000 miles
# - use theme_minimal
# - additional you can tweak some theme parameters (seen from the plot)
# - export your final plot


# Lets prepare the data
df <- hflights %>% 
  # calculate total number of flights per carrier
  group_by(UniqueCarrier) %>% 
  mutate(`nr flights` = n()) %>% 
  ungroup() %>% 
  # rank carriers & pick top 4
  mutate(rank = dense_rank(1/`nr flights`)) %>%  # smaller rank is given to the lowest value: that why 1/`nr flights`
  filter(rank <= 4) %>%  # select top 4
  select(carrier = UniqueCarrier, rank, distance = Distance) %>%  # relevant column selection
  mutate(carrier = as.factor(carrier),  # create factor 
         carrier = fct_infreq(carrier)) # sort factor levels based on frequency of flights

# Create figure
df %>% 
  ggplot(aes(x = distance,
             fill = carrier)) +
  geom_density(alpha = 0.5,
               color = "black") +
  scale_x_continuous(breaks = seq(0,2000,250),
                     limits = c(0, 2000)) +
  scale_y_continuous(breaks = seq(0,0.005,0.0001)) +
  scale_fill_viridis_d() +
  xlab("Distance of the flight (in miles)") +
  ylab("Denstity") +
  ggtitle("Distribution of flight distances - top 4 performing carriers") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Export plot
ggsave(filename = "./data/top_4_carriers_distance_density.png", 
       plot = last_plot(), 
       units = "cm", 
       width = 29, 
       height = 21, 
       dpi = 600)



# Exercise 2

# We will use diamonds data set from ggplot2
# - randomly select 10000 diamonds
# - create new variable volume = x * y * z
# - now keep only diamonds with:
#   - carat < 2.5   and
#   - price < 15000 and
#   - volume < 600
# - use your data to create scatter plot
# - on x axis put carat
# - on y axis put price 
# - size of the dots is represented with volume
# - for color of the dots use diamond cut 
# - create facets using facet_wrap and diamond color
# - additional you can tweak some theme parameters (seen from the plot)
# - export your final plot

# Lets prepare the data
set.seed(123)

df <- diamonds %>% 
  sample_n(size = 10000, replace = F) %>% # sample diamonds
  mutate(volume = x * y * z) %>% # diamond volume
  filter(carat < 2.5 & price < 15000 & volume < 600) # filter conditions 
  

# Create figure
df %>% 
  ggplot(aes(x = carat,
             y = price,
             size = volume,
             color = cut)) +
  geom_jitter() +
  scale_color_viridis_d() +
  facet_wrap(vars(color), 
             scales = "free") +
  xlab("Diamond weight (in carats)") +
  ylab("Diamond price (in USD)") +
  ggtitle("Diamond price by weight, volume, cut and diamond color") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14))

# Export plot
ggsave(filename = "./data/diamonds_scatterplot.png", 
       plot = last_plot(), 
       units = "cm", 
       width = 29, 
       height = 21, 
       dpi = 600)



# Exercise 3

# Use corpus.txt - data set from the assignment 3
# - clean the corpus (as we did before)
# - prepare a table called "corpus.words" (as we did before)
# - keep only 200 most frequent words
# - now use table "corpus.words" to draw word cloud:
# - each word is shown on the word cloud
# - "count" ~ frequency represents the size of the word on word cloud
# - words are put into different groups based on the first letter in the word:
#   - group a first letters: "a" "b" "c" "d" "e"
#   - group b first letters: "f" "g" "h" "i" "j"
#   - group c first letters: "k" "l" "m" "n" "o"
#   - group d first letters: "p" "q" "r" "s" "t"
#   - group e first letters: "u" "v" "w" "x" "y" "z"
#   - group f for all other characters!
# - use groups for word colors
# - export your final plot

# Lets prepare the data
corpus <- readLines("./data/corpus.txt")

corpus.clean <- corpus %>% 
  str_to_lower(.) %>%     # to lower-case
  str_remove_all(pattern = "[:punct:]") %>% # remove punctuation(s)
  str_remove_all(pattern = "[:digit:]") %>% # remove digits
  str_replace_all(pattern = "\\t|\\n", replacement = " ") %>% # replace tabs & new lines
  str_trim(side = "both") %>%  # trim white spaces from both sides
  str_replace_all(pattern = "\\s{2,}", replacement = " ") # replace multiple white spaces (more than one) with single white space

set.seed(123)

corpus.words <- corpus.clean %>% 
  str_c(sep = " ", collapse = " ") %>%  # collapse line into a single line
  str_split(pattern = " ") %>% # split line by white spaces into list of words
  unlist() %>%  # convert list to vector
  tibble(word = .) %>%  # create tibble
  # create word counts
  group_by(word) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  # top 1000 words
  mutate(rank = row_number()) %>% 
  filter(rank <= 200) %>% 
  # include word rotation 
  mutate(angle = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  # create groups
  mutate(`first letter` = str_sub(word,1,1),
         group = case_when(`first letter` %in% letters[1:5]   ~ "a",
                           `first letter` %in% letters[6:10]  ~ "b",
                           `first letter` %in% letters[11:15] ~ "c",
                           `first letter` %in% letters[16:20] ~ "d",
                           `first letter` %in% letters[21:26] ~ "e",
                           TRUE ~ "f"))

# Create figure
corpus.words %>% 
  ggplot(aes(label = word, 
             size = count, 
             angle = angle,
             color = group)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  scale_color_viridis_d(option = "magma") +
  theme_minimal()

# Export plot
ggsave(filename = "./data/corpus_top_200_words.png", 
       plot = last_plot(), 
       units = "cm", 
       width = 29/2, 
       height = 21/2, 
       dpi = 600)



# Exercise 4

# Use pjm_hourly_est.csv - data set from the assignment 4
# - we will draw time series for
#   - hourly energy consumption
#   - daily averages energy consumption 
#   - weekly averages energy consumption
# - create three time series subplots with cowplot
# - additional you can tweak some theme parameters (seen from the plot)
# - export your final plot

# Prepare the data
df.energy <- read_csv(file = "./data/pjm_hourly_est.csv", 
                      col_names = T, 
                      col_types = cols(.default = "c")) %>% 
  # keep only relevant columns
  select(datetime = Datetime,  # date time
         econs = PJME) %>%     # energy consumption in MW 
  # column conversion - parsing
  mutate(datetime = ymd_hms(datetime),
         econs = as.numeric(econs)) %>% 
  # filter only rows where you have data
  filter(!is.na(econs)) %>% 
  # sort rows based on time
  arrange(datetime) %>% 
  # Add columns:
  mutate(date = as_date(datetime),
         year = year(date),
         week = isoweek(date), # week of year
         week = str_pad(week, width = 2, side = "left", pad = "0"), # pad week to get two digits
         year_week = paste0(year, week), # merge year and week
         week_count = dense_rank(year_week)) # add absolute week count with dense rank

## Daily average consumption
df.energy.d <- df.energy %>% 
  group_by(date) %>% 
  summarise(`econs d avg` = mean(econs, na.rm = T)) %>% 
  ungroup()

## Weekly average consumption
df.energy.w <- df.energy %>% 
  group_by(week_count) %>% 
  summarise(`econs w avg` = mean(econs, na.rm = T)) %>% 
  ungroup()


# Create figure
p1 <- df.energy %>% 
  ggplot(aes(x = datetime,
             y = econs)) +
  geom_line(color = "gray50") +
  xlab("Time stamp - hour") +
  ylab("Megawatts (MW)") +
  ggtitle("Hourly energy consumption") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))

p2 <- df.energy.d %>% 
  ggplot(aes(x = date,
             y = `econs d avg`)) +
  geom_line(color = "gray30") +
  xlab("Date") +
  ylab("Megawatts (MW)") +
  ggtitle("Average daily energy consumption (smoothed)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))

p3 <- df.energy.w %>% 
  ggplot(aes(x = week_count,
             y = `econs w avg`)) +
  geom_line(color = "gray10") +
  xlab("Week") +
  ylab("Megawatts (MW)") +
  ggtitle("Average weekly energy consumption (smoothed)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))

plot_grid(p1, p2, p3, nrow = 3)

# Export plot
ggsave(filename = "./data/energy_consumption_time_series.png", 
       plot = last_plot(), 
       units = "cm", 
       width = 29, 
       height = 21, 
       dpi = 600)

