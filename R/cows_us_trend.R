library(tidyverse)
library(here)
library(stringr)

source(here("R", "cows_trend.R"))

cows_us_sum <- cows_trend %>%
  group_by(year) %>%
  summarize(cows = sum(cows)) %>%
  mutate(state = "Total U.S.") %>%
  select(state, year, cows)

cows_state_clean <- cows_trend %>%
  select(state, year, cows)

cows_us_trend <- rbind(cows_us_sum, cows_state_clean)
