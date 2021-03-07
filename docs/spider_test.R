# Test Spider Chart


library(tidyverse)
library(shiny)
library(bslib)
library(here)
library(sf)
library(tsibble)
library(feasts)
library(fable)
library(broom)
library(fmsb)


source(here("R", "serv_impact.R"))

radarchart(serv_impact)

serv_impact <- read_excel(here("data", "pnas.1906908116.sd01.xlsx")) %>% 
  clean_names() %>% 
  select(food_group, ghg_emissions_rank, land_use_rank, 
         eutrophication_potential_ep_rank, acidification_potential_ap_rank, 
         scarcity_weighted_water_use_rank) %>% 
  column_to_rownames(var = "food_group")

serv_impact <- rbind(rep(15,6), rep(1,6), serv_impact)

 






