#K.Palof 
# Geoduck age exploration - addressing issues from late 2016/early 2017
# see read me for more

rm(list = ls()) # clear workspace since data frames have same names

## Load packages ------
library(tidyverse)
library(FSA)

## Load data -------
dat <- read.csv("./data/12_14_geoduck_all.csv")
weight_pop <- read.csv("./data/GeoduckAgeStudyWeighting.csv")

# match area name, make sure both have year
levels(dat$ADFG_Fishery.Area)
levels(weight_pop$area)
glimpse(dat)


# match area names to merge files.
# Calculate weighting - compare to Mike's calc - see variable in file

# apply weighting to age frequencies
# perform catch curve analysis on data total and each area