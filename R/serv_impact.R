library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(RColorBrewer)

serv_impact <- read_excel(here("data", "pnas.1906908116.sd01.xlsx")) %>% 
  clean_names() %>% 
  select(food_group, ghg_emissions_rank, land_use_rank, 
         eutrophication_potential_ep_rank, acidification_potential_ap_rank, 
         scarcity_weighted_water_use_rank) %>% 
  rename("GHG" = ghg_emissions_rank, 
         "Land Use" = land_use_rank, 
         "EP" = eutrophication_potential_ep_rank, 
         "AP" = acidification_potential_ap_rank, 
         "Water Use" = scarcity_weighted_water_use_rank)

serv_impact <- rbind(rep(15,6), rep(1,6), serv_impact)


serv_pal <- c("#F0C418",
              "#E67E25",
              "#E74C3B",
              "#23B99A",
              "#2998FF",
              "#f57089",
              "#A24CC2",
              "#58F380",
              "#D9E716",
              "#40570f",
              "#BF6F76",
              "#8C8070",
              "#195f0d",
              "#2C1DFF",
              "#284907")

  
