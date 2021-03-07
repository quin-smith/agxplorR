library(tidyverse)
library(here)
library(stringr)
library(janitor)
library(tidytext)


source(here("R", "food_impact.R"))

beef_py <- read.csv(here("data","data_past_year","beef_py.csv")) %>% 
  clean_names() %>% 
  select(commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 0.45359237 / 1000000) %>%  # conversion from lbs to millions of kg
  mutate(commodity = "Beef")


chicken_py <- read.csv(here("data","data_past_year","chicken_py.csv")) %>% 
  clean_names() %>% 
  select(commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 0.45359237 / 1000000) %>%  # conversion from lbs to millions of kg
  mutate(commodity = "Chicken")


eggs_py <- read.csv(here("data","data_past_year","eggs_py.csv")) %>% 
  clean_names() %>% 
  select(commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(commodity) %>% 
  summarize(value = sum(value)) %>% 
  mutate(value = value * 50 / 1000 / 1000000) %>% # conversion from eggs to kg to millions of kg
  mutate(commodity = "Eggs")
  


milk_py <- read.csv(here("data","data_past_year","milk_py.csv")) %>% 
  clean_names() %>% 
  select(commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 0.45359237 / 1000000) %>%  # conversion from lbs to kg to millions of kg
  mutate(commodity = "Milk")


nuts_py <- read.csv(here("data","data_past_year","nuts_py.csv")) %>% 
  clean_names() %>% 
  select(commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value))  %>% 
  mutate(value = value * 2000 * 0.45359237 / 1000000) %>%  # conversion from tons to kg to millions of kg
  mutate(commodity = "Tree Nuts")
  

peas_py <- read.csv(here("data","data_past_year","peas_py.csv")) %>% 
  clean_names() %>% 
  select(commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 50.80234544 / 1000000) %>%  # conversion from cwt to kg to millions of kg
  mutate(commodity = "Peas")

  
pork_py <- read.csv(here("data","data_past_year","pork_py.csv")) %>% 
  clean_names() %>% 
  select(commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 0.45359237 / 1000000) %>%  # conversion from lbs to kg to millions of kg
  mutate(commodity = "Pork")


potatoes_py <- read.csv(here("data","data_past_year","potatoes_py.csv")) %>% 
  clean_names() %>% 
  select(commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 50.80234544 / 1000000) %>% # conversion from cwt to kg to millions of kg
  mutate(commodity = "Potatoes")


rice_py <- read.csv(here("data","data_past_year","rice_py.csv")) %>% 
  clean_names() %>% 
  select(commodity, value) %>% 
  mutate(value = str_replace_all(value, pattern = ",", replacement = "")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = value * 50.80234544 / 1000000) %>%  # conversion from cwt to kg to millions of kg
  mutate(commodity = "Rice")


commodity_py <- rbind(beef_py, chicken_py, eggs_py, milk_py, nuts_py, peas_py, pork_py, potatoes_py, rice_py)

food_impact_fct <- food_impact %>% 
  filter(product == "Rice" |
           product == "Potatoes" |
           product == "Peas" |
           product == "Nuts" |
           product == "Bovine Meat (beef herd)" |
           product == "Pig Meat" |
           product == "Poultry Meat" |
           product == "Milk" |
           product == "Eggs") %>% 
  pivot_wider(names_from = effect_type, values_from = mean) %>% 
  mutate(product = str_replace_all(product, pattern = "Nuts", replacement = "Tree Nuts")) %>%
  mutate(product = str_replace_all(product, pattern = "Bovine Meat \\(beef herd\\)", replacement = "Beef")) %>% 
  mutate(product = str_replace_all(product, pattern = "Pig Meat", replacement = "Pork")) %>% 
  mutate(product = str_replace_all(product, pattern = "Poultry Meat", replacement = "Chicken")) %>% 
  rename("commodity" = product)
  
  
total_impact <- commodity_py %>% 
  inner_join(food_impact_fct, by = "commodity") %>% 
  mutate(land_use_total = as.numeric(value) * as.numeric(land_use_mu)) %>% 
  mutate(ghg_2013_total = as.numeric(value) * as.numeric(ghg_2013_mu)) %>% 
  mutate(ghg_2007_total = as.numeric(value) * as.numeric(ghg_2007_mu)) %>% 
  mutate(acid_total = as.numeric(value) * as.numeric(acid_mu)) %>% 
  mutate(eutroph_total = as.numeric(value) * as.numeric(eutroph_mu)) %>% 
  mutate(freshwater_total = as.numeric(value) * as.numeric(freshwater_mu)) %>% 
  mutate(stresswater_total = as.numeric(value) * as.numeric(stresswater_mu)) %>% 
  select(commodity, value, land_use_total, ghg_2013_total, ghg_2007_total, acid_total, eutroph_total, freshwater_total, stresswater_total)





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



# values from food impact data

# land_use_mu - land use in m^2 / FU
# ghg_2013_mu - GHG emissions - kg of CO2 equiv./FU - IPCC 2013 incl. cc feedbacks
# ghg_2007_mu - GHG emissions - kg of CO2 equiv./FU - IPCC 2007
# acid_mu - g of SO2 equiv / FU - CML2 baseline
# eutroph_mu - g PO4^3- equiv / FU - CML2 baseline
# freshwater_mu - freshwater withdrawls (L/FU)
# stresswater_mu - stress-weighted water use (L/FU)








