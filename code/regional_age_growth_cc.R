#K.Palof 
# Regional groupings / Group 1 and 2 / Sea Otter - presence or absence
# Group Sitka - Biorka/Legma Islands and 
# Group 1 - sea otter absent - Nakat, Vallenar, Vegas/Hotspur
# Group 2 - sea otter present - Cone is, east san, Tlevak, Warren, and 
      # Sitka (Biorka/ Taigud)   - do this with and without Sitka
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

# full data set with weigtings
dat %>% left_join(weight_pop) -> dat_wt # raw data
# summarized by area, age with weigthing
dat %>% group_by(ADFG_Fishery.Area, Age_2012) %>% 
  summarise(n = n()) -> dat_hist
dat_hist %>% left_join(weight_pop) %>% mutate(n_wt = n*wt_each) -> dat_hist1 # counts by age/area with weighting

## weighting counts 
dat %>% group_by(ADFG_Fishery.Area) %>% summarise(N_samp = n()) ->total.n.area
dat_hist1 %>% left_join(total.n.area) %>% 
  mutate(prop = n/N_samp, n_corrected = prop*popsize_wshow) -> dat_hist2
# n_corrected is counts using population size weighting.  n_wt is proportion using same weighting
# this is just a scale peference

# melt so that each age counted has it's own row


### Sea otter (w/ sitka) -------------------
################# age structure visual------------------------------------------------------
head(dat_wt)
head(dat_hist2) 

ggplot(dat_wt, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 

ggplot(dat_wt, aes(x=Age_2012, fill=otter.status)) + 
  geom_density(alpha = 0.3) 

ggplot(dat_hist1, aes(x=Age_2012, y = n_wt, fill = otter.status))  + 
  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?

ggplot(dat_hist2, aes(x=Age_2012, y = n_corrected, fill = otter.status))  + 
  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?


################# mean Age -------------------------------------------
################# age Composition Differences -------------------------------------------
################# growth -------------------------------------------
# L-A, W-L, W-A traditional methods

################# Catch curve traditional -------------------------------------------
