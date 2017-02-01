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
dat %>%
  filter(Fishery.Area == "3_Nakat Inlet") ->nakat
dat %>%
  filter(Fishery.Area == "4_Vallenar Bay") ->val
dat %>%
  filter(Fishery.Area == "5_Vegas/Hotspur Is.") ->vegh
dat %>%
  filter(Fishery.Area == "6_Tlevak Strait") -> tlev
dat %>%
  filter(Fishery.Area == "7_Warren/Kosciusko Is.") -> warren
dat %>%
  filter(Fishery.Area == "8_Taigud/Kolosh Is.") ->tkis
dat %>%
  filter(Fishery.Area == "9_Biorka/Legma Is.") ->biork


# K-S test
ks.test(cone_is$Age_2012, sanfran$Age_2012) # reject null
ks.test(cone_is$Age_2012, nakat$Age_2012) # reject null
ks.test(cone_is$Age_2012, val$Age_2012) # reject null
ks.test(cone_is$Age_2012, vegh$Age_2012) # reject null
ks.test(cone_is$Age_2012, tlev$Age_2012) # reject null
ks.test(cone_is$Age_2012, warren$Age_2012) # reject null
ks.test(cone_is$Age_2012, tkis$Age_2012) # reject null
ks.test(cone_is$Age_2012, biork$Age_2012) # reject null




#A-D test, k sample tests
ad.test(Age_2012 ~ Fishery.Area, data = dat)
# reject null hypothosis that all samples come from the same distribution.