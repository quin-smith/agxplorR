library(tidyverse)
library(here)
library(janitor)
library(readxl)

serv_impact <- read_excel(here("data", "pnas.1906908116.sd01.xlsx")) %>% 
  clean_names() %>% 
  select(food_group, ghg_emissions_rank, land_use_rank, 
         eutrophication_potential_ep_rank, acidification_potential_ap_rank, 
         scarcity_weighted_water_use_rank) %>% 
  column_to_rownames(var = "food_group")

serv_impact <- rbind(rep(15,6), rep(1,6), serv_impact)