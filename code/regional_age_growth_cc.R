#K.Palof 
# Regional groupings / Group 1 and 2 / Sea Otter - presence or absence
# Group Sitka - Biorka/Legma Islands and 
# Group 1 - 
# Group 2 - 
# Geoduck age exploration - addressing issues from late 2016/early 2017
# 

rm(list = ls()) # clear workspace since data frames have same names

## Load packages ---------------------------------------------------
library(tidyverse)
library(FSA)
library(gridExtra)

## Load data -------------------------------------------------------
dat <- read.csv("./data/12_14_geoduck_all.csv")
weight_pop <- read.csv("./data/GeoduckAgeStudyWeighting.csv")

weight_pop %>% mutate(ADFG_Fishery.Area = area, wt_each = popsize_wshow/(sum(popsize_wshow))) %>% 
  select (-area) %>% select(-AgeStudyWeighting)-> weight_pop

# match area name, make sure both have year
levels(dat$ADFG_Fishery.Area)
levels(weight_pop$area)
glimpse(dat)

dat %>% left_join(weight_pop) -> dat_wt

################# Age structure visual------------------------------------------------------


################# Mean Age -------------------------------------------
################# Age Composition Differences -------------------------------------------
################# Growth -------------------------------------------
# L-A, W-L, W-A traditional methods

################# Catch curve traditional -------------------------------------------
