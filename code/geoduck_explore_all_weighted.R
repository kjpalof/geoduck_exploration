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

weight_pop %>% mutate(wt_each = popsize_wshow/(sum(popsize_wshow))) -> weight_pop

# match area name, make sure both have year
levels(dat$ADFG_Fishery.Area)
levels(weight_pop$area)
glimpse(dat)

# this doesn't give 0's for missing ages...
dat %>% group_by(ADFG_Fishery.Area, Age_2012) %>% 
  summarise(n = n()) -> dat_hist
  
#histograms - no weightings by population size -------------
ggplot(dat, aes(x = Age_2012)) +geom_histogram(binwidth =1.5) #all together, no weighting
ggplot(dat, aes(x=Age_2012)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1.0,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

ggplot(dat, aes(x=Age_2012, fill=ADFG_Fishery.Area)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1.5) +
  geom_density()  # Overlay with transparent density plot

ggplot(dat, aes(x=Age_2012, fill=ADFG_Fishery.Area)) +
  geom_histogram(binwidth=1.5, alpha=.5, position="identity")
ggplot(dat, aes(x=Age_2012, fill=ADFG_Fishery.Area)) + geom_density(alpha=.3)
  
ggplot(dat, aes(x=Age_2012)) + geom_histogram(binwidth=1.5, colour="black", fill="white") + 
  facet_grid( ADFG_Fishery.Area~ .)

### histograms from summarized data -------------------
dat_hist
ggplot(dat_hist, aes(x=Age_2012, y = n))  + 
  geom_bar(stat = "identity", width =0.5) +geom_density()


ggplot(dat_hist, aes(x=Age_2012, y = n, fill = ADFG_Fishery.Area))  + 
  geom_bar(stat = "identity", width =0.5)
ggplot(dat_hist, aes(x=Age_2012, y = n, fill = ADFG_Fishery.Area))  + 
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()



# match area names to merge files.
# Calculate weighting - compare to Mike's calc - see variable in file

# apply weighting to age frequencies
# perform catch curve analysis on data total and each area