library(tidyverse)
library(here)
library(stringr)

source(here("R", "cows_trend.R"))

milk_us_sum <- milk_trend %>%
  group_by(year) %>%
  summarize(milk_l_e6 = sum(milk_l_e6)) %>%
  mutate(state = "Total U.S.") %>%
  select(year, state, milk_l_e6)

milk_state_clean <- milk_trend %>%
  select(year, state, milk_l_e6)

milk_us_trend <- rbind(milk_us_sum, milk_state_clean)