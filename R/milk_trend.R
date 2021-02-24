library(tidyverse)
library(here)
library(stringr)
library(janitor)
library(tidytext)


milk_trend <- read.csv(here("data","milk.csv")) %>% 
  select(Year, State, Value) %>% 
  clean_names() %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>%
  mutate(milk_kg = as.numeric(value) * 0.4535924) %>% 
  rename(milk_lb = value)

milk_trend <- milk_trend %>% 
  mutate(state = str_to_title(milk_trend$state))
