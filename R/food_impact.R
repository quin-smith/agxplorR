library(tidyverse)
library(here)
library(stringr)
library(janitor)
library(tidytext)
library(readxl)



food_impact <- read_excel(here("data","aaq0216_DataS2.xls"), skip = 1) %>% 
  rename(product = "...1") %>% 
  rename(land_use_mu = "...4") %>% 
  rename(ghg_2013_mu = "...10") %>% 
  rename(ghg_2007_mu = "...16") %>% 
  rename(acid_mu = "...22") %>% 
  rename(eutroph_mu = "...28") %>% 
  rename(freshwater_mu = "...34") %>% 
  rename(stresswater_mu = "...40") %>% 
  select(product,
         land_use_mu,
         ghg_2013_mu,
         ghg_2007_mu,
         acid_mu,
         eutroph_mu,
         freshwater_mu,
         stresswater_mu) %>% 
  filter(land_use_mu != "Mean" & land_use_mu != "NA")
  






# land_use_mu - land use in m^2 / FU
# ghg_2013_mu - GHG emissions - kg of CO2 equiv./FU - IPCC 2013 incl. cc feedbacks
# ghg_2007_mu - GHG emissions - kg of CO2 equiv./FU - IPCC 2007
# acid_mu - g of SO2 equiv / FU - CML2 baseline
# eutroph_mu - g PO4^3- equiv / FU - CML2 baseline
# freshwater_mu - freshwater withdrawls (L/FU)
# stresswater_mu - stress-weighted water use (L/FU)

