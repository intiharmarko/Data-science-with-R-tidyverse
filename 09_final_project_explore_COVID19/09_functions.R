# 9 Case study: Explore COVID-19 pandemic in the USA

# (R custom functions) 



# 2) Initial data inspection - Exploratory Data Analysis

## a) function for NAs detection
count_NA <- function(df){
  require(tidyverse)
  require(cowplot)
  
  # count Nas 
  df.Na.count <- map(df, ~sum(is.na(.))) %>% 
    simplify() %>% 
    tibble(col = names(.),
           NAs = .) %>% 
    # add NA as percentage of cases
    mutate(`NA %` = round(NAs / nrow(df) * 100, 2))
  
  print(df.Na.count %>% as.data.frame())
  
  # absolute NAs count
  p1 <- ggplot(df.Na.count, aes(x = col, y = NAs)) + 
    geom_col() +
    theme(axis.text.x = element_text(angle = 90))
  
  # relative NAs count
  p2 <- ggplot(df.Na.count, aes(x = col, y = `NA %`)) + 
    geom_col() +
    scale_y_continuous(limits = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 90))
  
  plot_grid(p1, p2, nrow = 2)
}

## b) function for time span check
check_time_span <- function(df){
  require(tidyverse)
  
  # summarise time spans - table level 
  table.count <- df %>% 
    summarise(distinct_dates = n_distinct(date),
              min_date = min(date),
              max_date = max(date),
              `days between max - min` = max_date - min_date + 1) 
  
  print(table.count)
  
  # show distinct dates count for each state
  df %>% 
    # count distinct dates per each state 
    group_by(state) %>% 
    summarise(distinct_dates = n_distinct(date)) %>% 
    ungroup() %>% 
    # show counts
    ggplot(aes(x = state, 
               y = distinct_dates)) + 
    geom_col() +
    theme(axis.text.x = element_text(angle = 90))
}



# 3) Data Wrangling - create the main table

## a) state name matching across all sources
state_matching <- function(states_base = states.list,
                           data,
                           col_name ){
  require(tidystringdist)
  
  # name of final state column
  col_name <- rlang::ensym(col_name)
  
  ## extract unique state names from given data source 
  states.data <- data %>% distinct(state)
  
  ## create table of all combinations states pairs
  states.combs <- expand.grid(state_base = states.list %>% pull(state_base),
                              state = states.data %>% pull(state)) %>% 
    as_tibble()
  
  ## Compute string distance for state names
  states.distance <- tidy_stringdist(df = states.combs, 
                                     v1 = state_base, 
                                     v2 = state, method= "osa") %>% 
    # sort best name match per state_base & add matching rank
    arrange(state_base, osa) %>% 
    group_by(state_base) %>% 
    mutate(rank = row_number()) %>% 
    ungroup() %>% 
    # filter only top ranks
    filter(rank == 1) %>% 
    # select & rename only relevant columns
    select(state_base,
           !!col_name := state)
  
  return(states.distance)
}



# 4) Exploratory Data Analysis

## a) plot function: confirmed cases & deaths total count over time
plot_confirmed_cases_total <- function(region.group){
  
  # data for the plot
  plot.data <- df.main %>% 
    filter(`region - group` == region.group)
  
  # confirmed cases absolute count 
  p11 <- plot.data %>% 
    ggplot(aes(x = date, 
               y = `confirmed total`,
               group = state,
               color = state)) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Number of confirmed cases total") +
    ggtitle(paste0("Infected cases / ", region.group)) +
    theme_minimal()
  
  # confirmed cases relative count
  p21 <- plot.data %>% 
    ggplot(aes(x = date, 
               y = `confirmed total %`,
               group = state,
               color = state)) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("% of confirmed cases total (relative to state population)") +
    theme_minimal()
  
  # deaths absolute count 
  p12 <- plot.data %>% 
    ggplot(aes(x = date, 
               y = `deaths total`,
               group = state,
               color = state)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Number of deaths total") +
    ggtitle(paste0("Deaths / ", region.group)) +
    theme_minimal()
  
  # deaths relative count
  p22 <- plot.data %>% 
    ggplot(aes(x = date, 
               y = `deaths total %`,
               group = state,
               color = state)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("% of deaths total (relative to state population)") +
    theme_minimal()
  
  # draw subplots
  plot <- plot_grid(p11, p12, p21, p22, nrow = 2, ncol = 2)  
  
  # export plot
  ggsave(filename = paste0("./explore/01_confirmed_cases_deaths_", region.group, ".png"),
         plot = plot, 
         width = 30, height = 20, units = "cm")
}


## b) plot function: confirmed daily cases & deaths 7d avg
plot_confirmed_deaths_cases_7d_avg <- function(region.group){
  
  # data for the plot
  plot.data <- df.main %>% 
    filter(`region - group` == region.group)
  
  # confirmed cases absolute count 
  p1 <- plot.data %>% 
    ggplot(aes(x = date, 
               y = `confirmed daily cases 7d avg`,
               group = state,
               color = state)) +
    geom_line(show.legend = T, 
              size = 0.9) +
    geom_point(show.legend = F, 
               size = 1) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Number of cases (7 day avg)") +
    ggtitle(paste0("Infected daily cases / ", region.group)) +
    theme_minimal()
  
  # deaths absolute count 
  p2 <- plot.data %>% 
    ggplot(aes(x = date, 
               y = `deaths daily cases 7d avg`,
               group = state,
               color = state)) +
    geom_line(show.legend = T, 
              size = 0.9) +
    geom_point(show.legend = F, 
               size = 0.8) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Number of deaths (7 day avg)") +
    ggtitle(paste0("Deaths / ", region.group)) +
    theme_minimal()
  
  # draw subplots
  plot <- plot_grid(p1, p2, nrow = 2, ncol = 1)  
  
  # export plot
  ggsave(filename = paste0("./explore/03_confirmed_daily_cases_deaths_7d_avg_", region.group, ".png"),
         plot = plot, 
         width = 30, height = 20, units = "cm", 
         dpi = 600)
}


## c) plot function: confirmed daily cases & deaths 7d avg + total vaccine doses
plot_confirmed_deaths_cases_7d_avg_vaccine_doses_total <- function(region.group){
  
  # data for the plot
  plot.data <- df.main %>% 
    filter(`region - group` == region.group)
  
  # confirmed cases absolute count 
  p1 <- plot.data %>% 
    ggplot(aes(x = date, 
               y = `confirmed daily cases 7d avg`,
               group = state,
               color = state)) +
    geom_line(show.legend = T, 
              size = 0.9) +
    geom_point(show.legend = F, 
               size = 1) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Number of cases (7 day avg)") +
    ggtitle(paste0("Infected daily cases / ", region.group)) +
    theme_minimal()
  
  # deaths absolute count 
  p2 <- plot.data %>% 
    ggplot(aes(x = date, 
               y = `deaths daily cases 7d avg`,
               group = state,
               color = state)) +
    geom_line(show.legend = T, 
              size = 0.9) +
    geom_point(show.legend = F, 
               size = 0.8) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Number of deaths (7 day avg)") +
    ggtitle(paste0("Deaths / ", region.group)) +
    theme_minimal()
  
  # total vaccine doses 
  p3 <- plot.data %>% 
    ggplot(aes(x = date, 
               y = `vaccine doses total`,
               group = state,
               color = state)) +
    geom_line(show.legend = T, 
              size = 0.9) +
    geom_point(show.legend = F, 
               size = 0.8) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Total vaccine doses") +
    ggtitle(paste0("Total vaccine doses / ", region.group)) +
    theme_minimal()
  
  # draw subplots
  plot <- plot_grid(p1, p2, p3, nrow = 3, ncol = 1)  
  
  # export plot
  ggsave(filename = paste0("./explore/05_confirmed_daily_cases_deaths_7d_avg_total_vaccine_doses", region.group, ".png"),
         plot = plot, 
         width = 30, height = 20, units = "cm", 
         dpi = 600)
}


## d) plot function: selected state COVID 19 indicators
plot_COVID19_indicators_state_level <- function(state_selection){
  
  # extract selected state data
  df.main.state <- df.main %>% filter(state == state_selection)
  
  # Total counts stacked area plot (infected, deaths, vaccine doses)
  p1 <- df.main.state %>% 
    select(date, state, `confirmed total`, `deaths total`, `vaccine doses total`) %>% 
    pivot_longer(cols = c("confirmed total", "deaths total", "vaccine doses total"), 
                 names_to = "indicator", values_to = "value") %>% 
    mutate(indicator = factor(indicator, 
                              levels = c("vaccine doses total", 
                                         "confirmed total", 
                                         "deaths total"))) %>% 
    ggplot(aes(x = date, 
               y = value,
               fill = indicator)) +
    geom_area(color = "black") +
    scale_fill_manual(values = c("forestgreen", "brown1", "black")) +
    xlab("Date") +
    ylab("Total count") +
    ggtitle(paste0("State: ", state_selection, " - COVID 19 indicators over observed period")) +
    theme_minimal()
  
  # Confirmed cases / deaths  - 7 day moving average
  p2 <- df.main.state %>% 
    select(date, state, `confirmed daily cases 7d avg`, `deaths daily cases 7d avg`, `daily vaccine doses 7d avg`) %>% 
    pivot_longer(cols = c("confirmed daily cases 7d avg", "deaths daily cases 7d avg", "daily vaccine doses 7d avg"), 
                 names_to = "indicator", values_to = "value") %>%
    mutate(value = na_if(value, 0)) %>% 
    ggplot(aes(x = date, 
               y = value,
               group = indicator,
               color = indicator)) +
    geom_line(size = 0.9) +
    geom_point(size = 1.2) +
    scale_y_log10() +         # replace 0 with NA (later we use log10 transformation)
    scale_color_manual(values = c("brown1", "forestgreen", "black")) +
    xlab("Date") +
    ylab("Number of cases & deaths (7 day avg) - log10 scale") +
    theme_minimal()
  
  # State (Government) response - selected indexes
  p3 <- df.main.state %>% 
    select(date, 
           government_response_index_for_display, 
           containment_health_index_for_display, 
           economic_support_index_for_display,
           stringency_index_for_display) %>% 
    pivot_longer(cols = c("government_response_index_for_display",
                          "containment_health_index_for_display",
                          "economic_support_index_for_display",
                          "stringency_index_for_display"), 
                 names_to = "index", values_to = "value") %>% 
    ggplot(aes(x = date, y = value, fill = index)) +
    geom_area(color = "black") +
    scale_y_continuous(limits = c(0, 300)) +
    scale_fill_viridis_d() +
    xlab("Date") +
    ylab("State response - indexes") +
    theme_minimal()
  
  # Create subplots
  row1 <- cowplot::plot_grid(p1, NULL, rel_widths = c(0.945,0.055))
  row2 <- cowplot::plot_grid(p2, NULL, rel_widths = c(0.97,0.03))
  row3 <- cowplot::plot_grid(p3, NULL, rel_widths = c(1,0))
  plot_ <- cowplot::plot_grid(row1, row2, row3, nrow = 3)
  
  # export plot
  ggsave(filename = paste0("./explore/09_COVID19_indicators_state", state_selection, ".png"),
         plot = plot_, 
         width = 35, height = 25, units = "cm", 
         dpi = 600)
}

