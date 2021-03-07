library(tidyverse)
library(here)
library(stringr)
library(janitor)
library(tidytext)

py_state_chicken <- read.csv(here("data","data_py_state","chicken_py_state.csv")) %>% 
  clean_names() %>% 
  select(state, commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>%
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 0.45359237 / 1000000) %>% # conversion from lbs to millions of kg
  mutate(commodity = "Chicken")

py_state_eggs <- read.csv(here("data","data_py_state","eggs_py_state.csv")) %>% 
  clean_names() %>% 
  select(state, commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 0.45359237 / 1000000) %>% # conversion from lbs to millions of kg
  mutate(commodity = "Eggs")

py_state_milk <- read.csv(here("data","data_py_state","milk_py_state.csv")) %>% 
  clean_names() %>% 
  select(state, commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>%
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 0.45359237 / 1000000) %>%  # conversion from lbs to kg to millions of kg
  mutate(commodity = "Milk")

py_state_peas <- read.csv(here("data","data_py_state","peas_py_state.csv")) %>% 
  clean_names() %>% 
  select(state, commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>%
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 50.80234544 / 1000000) %>%  # conversion from cwt to kg to millions of kg
  mutate(commodity = "Peas")

py_state_pork <- read.csv(here("data","data_py_state","pork_py_state.csv")) %>% 
  clean_names() %>% 
  select(state, commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>%
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 0.45359237 / 1000000) %>%  # conversion from lbs to kg to millions of kg
  mutate(commodity = "Pork") %>% 
  add_row(state = "OTHER STATES", commodity = "Pork")

py_state_potatoes <- read.csv(here("data","data_py_state","potatoes_py_state.csv")) %>% 
  clean_names() %>% 
  select(state, commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>%
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 50.80234544 / 1000000) %>% # conversion from cwt to kg to millions of kg
  mutate(commodity = "Potatoes")

py_state_rice <- read.csv(here("data","data_py_state","rice_py_state.csv")) %>% 
  clean_names() %>% 
  select(state, commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>%
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 50.80234544 / 1000000) %>%  # conversion from cwt to kg to millions of kg
  mutate(commodity = "Rice")

py_state <- py_state_pork %>% 
  full_join(py_state_chicken, by = "state") %>% 
  full_join(py_state_eggs, by = "state") %>% 
  full_join(py_state_milk, by = "state") %>% 
  full_join(py_state_peas, by = "state") %>% 
  full_join(py_state_potatoes, by = "state") %>% 
  full_join(py_state_rice, by = "state") %>% 
  fill(commodity.y) %>% 
  fill(commodity.x.x) %>% 
  fill(commodity.y.y) %>% 
  fill(commodity.x.x.x, .direction = "updown") %>% 
  fill(commodity.y.y.y, .direction = "updown") %>% 
  fill(commodity, .direction = "updown") %>% 
  pivot_wider(names_from = "commodity.x", values_from = "value.x") %>% 
  pivot_wider(names_from = "commodity.y", values_from = "value.y") %>% 
  pivot_wider(names_from = "commodity.x.x", values_from = "value.x.x") %>% 
  pivot_wider(names_from = "commodity.y.y", values_from = "value.y.y") %>% 
  pivot_wider(names_from = "commodity.x.x.x", values_from = "value.x.x.x") %>% 
  pivot_wider(names_from = "commodity.y.y.y", values_from = "value.y.y.y") %>% 
  pivot_wider(names_from = "commodity", values_from = "value") %>% 
  select(state, Chicken, Eggs, Milk, Peas, Pork, Potatoes, Rice) %>% 
  pivot_longer(cols = c(Chicken,
                        Eggs,
                        Milk,
                        Peas,
                        Pork,
                        Potatoes,
                        Rice),
               names_to = "commodity",
               values_to = "production")




















# values from USDA data

# beef: year - 2019, original data - BEEF, SLAUGHTER - PRODUCTION, MEASURED IN LB
# chicken: year - 2019, original data - CHICKENS, BROILERS - PRODUCTION, MEASURED IN LB
# eggs: year - 2020, original data - EGGS - PRODUCTION, MEASURED IN EGGS
# average egg weight = 50 g (from: https://weightofstuff.com/how-much-does-an-egg-weigh/)
# milk: year - 2020, original data - MILK - PRODUCTION, MEASURED IN LB
# nuts: year - 2019, original data - TREE NUT TOTALS, UTILIZED - PRODUCTION, MEASURED IN TONS
# peas: year - 2020, original data - PEAS, DRY EDIBLE - PRODUCTION, MEASURED IN CWT
# pork: year - 2019, original data - HOGS - PRODUCTION, MEASURED IN LB
# potatoes: year - 2020, original data - POTATOES - PRODUCTION, MEASURED IN CWT
# rice: year - 2020, original data - RICE - PRODUCTION, MEASURED IN CWT