library(tidyverse)
library(here)
library(stringr)
library(janitor)
library(lubridate)

# us_dairy_emissions contains total US dairy emissions

emissions_trend <- read.csv(here("data","us_dairy_emissions.csv")) %>%
  clean_names() %>%
  select(element, value, date_value) %>%
  rename(emissions_type = element) %>%
  rename(total_emissions = value) %>%
  rename(year = date_value) %>%
  mutate(year = lubridate::year(year)) %>%
  filter(emissions_type == "EMISSIONS (CO2EQ) FROM CH4 (MANURE MANAGEMENT)" |
           emissions_type == "EMISSIONS (CO2EQ) FROM N2O (MANURE MANAGEMENT)" |
           emissions_type == "EMISSIONS (CO2EQ) (MANURE MANAGEMENT)" |
           emissions_type == "EMISSIONS (N2O) (MANURE MANAGEMENT)" |
           emissions_type == "EMISSIONS (CH4) (MANURE MANAGEMENT)") %>%
  mutate(emissions_type = case_when(
    emissions_type == "EMISSIONS (CO2EQ) FROM CH4 (MANURE MANAGEMENT)" ~ "ch4_co2_eq",
    emissions_type == "EMISSIONS (CO2EQ) FROM N2O (MANURE MANAGEMENT)" ~ "n2o_co2_eq",
    emissions_type == "EMISSIONS (CO2EQ) (MANURE MANAGEMENT)" ~ "total_co2_eq",
    emissions_type == "EMISSIONS (N2O) (MANURE MANAGEMENT)" ~ "n20",
    emissions_type == "EMISSIONS (CH4) (MANURE MANAGEMENT)" ~ "ch4"
  ))




