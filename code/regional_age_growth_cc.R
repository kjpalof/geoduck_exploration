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
library(weights)

## Load data -------------------------------------------------------
dat <- read.csv("./data/12_14_geoduck_all.csv")
weight_pop <- read.csv("./data/GeoduckAgeStudyWeighting.csv")

weight_pop %>% mutate(ADFG_Fishery.Area = area, wt_each = popsize_wshow/(sum(popsize_wshow))) %>% 
  select (-area) %>% select(-AgeStudyWeighting)-> weight_pop

# full data set with weigtings
dat %>% left_join(weight_pop) -> dat2 # raw data
# summarized by area, age with weigthing
dat2 %>% group_by(ADFG_Fishery.Area, Age_2012) %>% 
  summarise(n = n()) -> dat2_by.area
dat2_by.area %>% left_join(weight_pop) %>% mutate(n_wt = n*wt_each) -> dat_wt_by.area # counts by age/area with weighting

## weighting counts 
dat %>% group_by(ADFG_Fishery.Area) %>% summarise(N_samp = n()) ->total.n.area
dat_wt_by.area %>% left_join(total.n.area) %>% 
  mutate(prop = n/N_samp, n_corrected = prop*popsize_wshow) -> dat_wt_by.area2
# n_corrected is counts using population size weighting.  n_wt is proportion using same weighting
# this is just a scale peference

# melt so that each age counted has it's own row


### Sea otter (w/ sitka) -------------------
################# age structure visual------------------------------------------------------
head(dat2)
head(dat_hist2) 

ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 
ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, position = "dodge") 

ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_density(alpha = 0.3) 

ggplot(dat_wt_by.area2, aes(x=Age_2012, y = n_wt, fill = otter.status))  + 
  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?

ggplot(dat_wt_by.area2, aes(x=Age_2012, y = n_corrected, fill = otter.status))  + 
  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?

################ data sets by sea otter ---------------
dat2 %>% filter(otter.status == "present") -> present_raw
dat2 %>% filter(otter.status == "absent") -> absent_raw
dat_wt_by.area2 %>% filter(otter.status == "present") -> present_wt
dat_wt_by.area2 %>% filter(otter.status == "absent") -> absent_wt

################# mean Age -------------------------------------------
# use dat_wt_by.area2  weight mean age by n_corrected or n_wt(weight)
dat_wt_by.area2 %>% group_by(otter.status) %>% summarise(mean = weighted.mean(Age_2012, n_wt))
dat_wt_by.area2 %>% group_by(otter.status) %>% summarise(mean = weighted.mean(Age_2012, n_corrected))

wtd.t.test(x = present_wt$Age_2012, y = absent_wt$Age_2012, weight= present_wt$n_wt, 
           weighty = absent_wt$n_wt)

t.test(x = present_raw$Age_2012, y = absent_raw$Age_2012)

################# age Composition Differences -------------------------------------------
#do this on raw unweighted data

ks.test(present_raw$Age_2012, absent_raw$Age_2012) #reject null that the two are equal
  
  
################# growth -------------------------------------------
# L-A, W-L, W-A traditional methods

################# Catch curve traditional -------------------------------------------
