#K.Palof 
# Geoduck age exploration - addressing issues from late 2016/early 2017
# see read me for more

#Testing for samples from the same distribution 
# K-S test?

rm(list = ls()) # clear workspace since data frames have same names

## Load packages ------
library(tidyverse)
library(kSamples)

## Load data -------
dat <- read.csv("./data/12_14_geoduck_all.csv")

dat %>%
  filter(Fishery.Area == "1_Cone Is.") -> cone_is
dat %>%
  filter(Fishery.Area == "2_East San Fernando") ->sanfran



# K-S test
ks.test(cone_is$Age_2012, sanfran$Age_2012)


#A-D test, k sample tests
ad.test(Age_2012 ~ Fishery.Area, data = dat)
# reject null hypothosis that all samples come from the same distribution.