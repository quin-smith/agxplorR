library(tidyverse)
library(here)
library(stringr)

cows_trend <- read.csv(here("data","cows.csv"))

colnames(cows_trend) <- as.character(cows_trend[1, ])

cows_trend <- cows_trend[-c(1,2),]

colnames(cows_trend)[1] <- "region"
colnames(cows_trend)[2] <- "state"

cows_trend <- cows_trend %>%
  mutate_all(na_if,"") %>%
  fill(region) %>%
  drop_na(region:state) %>%
  pivot_longer(cols = 3:52,
               names_to = "year",
               values_to = "cows") %>%
  mutate(cows = str_replace(cows, pattern = ",", replacement = "")) %>%
  mutate(cows = as.numeric(cows)) %>%
  arrange(state) %>%
  mutate(year = as.numeric(year))
