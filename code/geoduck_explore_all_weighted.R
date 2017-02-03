#K.Palof 
# Geoduck age exploration - addressing issues from late 2016/early 2017
# see read me for more

rm(list = ls()) # clear workspace since data frames have same names

## Load packages ------
library(tidyverse)
library(FSA)
library(gridExtra)

## Load data -------
dat <- read.csv("./data/12_14_geoduck_all.csv")
weight_pop <- read.csv("./data/GeoduckAgeStudyWeighting.csv")

weight_pop %>% mutate(ADFG_Fishery.Area = area, wt_each = popsize_wshow/(sum(popsize_wshow))) %>% 
  select (-area) %>% select(-AgeStudyWeighting)-> weight_pop

# match area name, make sure both have year
levels(dat$ADFG_Fishery.Area)
levels(weight_pop$ADFG_Fishery.Area)
glimpse(dat)

dat %>% left_join(weight_pop) -> dat_wt

### summarized by area / age-------------
# this doesn't give 0's for missing ages...
dat_wt %>% group_by(ADFG_Fishery.Area, Age_2012) %>% 
  summarise(n = n()) -> dat_wt_by.area

## summary statistics raw ------------
dat %>% group_by(ADFG_Fishery.Area) %>% 
  summarise(min = min(Age_2012), mean = mean(Age_2012), median = median(Age_2012), max =max(Age_2012)) -> sumstats

### save tables and figures if needed ------------------------------------
write_csv(sumstats, 'output/raw_summary.csv')


############################################  
### histograms - no weightings by population size -------------
ggplot(dat, aes(x = Age_2012)) +geom_histogram(binwidth =1.0,colour="black", fill="white") #all together, no weighting
ggplot(dat, aes(x=Age_2012)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1.0,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

## all on same graph with density - weird ---------
ggplot(dat, aes(x=Age_2012, fill=ADFG_Fishery.Area)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1.5) +
  geom_density(alpha=.2)  # Overlay with transparent density plot

# areas by color ---------------
ggplot(dat, aes(x=Age_2012, fill=ADFG_Fishery.Area)) +
  geom_histogram(binwidth=1.0, alpha=.75, position="identity")
#with lines for means --------------
ggplot(dat, aes(x=Age_2012, fill=ADFG_Fishery.Area)) +
  geom_histogram(binwidth=1.0, alpha=.5, position="identity")+
  geom_vline(data = sumstats, aes(xintercept = mean, colour = ADFG_Fishery.Area), linetype = "dashed", size =1)

# just density plots by area --------------
ggplot(dat, aes(x=Age_2012, fill=ADFG_Fishery.Area)) + geom_density(alpha=.3)
# facet wrap by area  ------------
ggplot(dat, aes(x=Age_2012)) + geom_histogram(binwidth=1.5, colour="black", fill="white") + 
  facet_grid( ADFG_Fishery.Area~ .)

## weighting in histogram???---------
ggplot(dat_wt, aes(x = Age_2012, weight = wt_each)) +geom_histogram(binwidth =1.0,colour="black", fill="white") #all together - using weighting

ggplot(dat_wt, aes(x=Age_2012, weight= wt_each, fill=ADFG_Fishery.Area)) +
  geom_histogram(binwidth=1.0, alpha=.75, position="identity")

### bar graphs from summarized data -------------------
dat_wt_by.area
all <- ggplot(dat_wt_by.area, aes(x=Age_2012, y = n))  + 
  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?


ggplot(dat_wt_by.area, aes(x=Age_2012, y = n, fill = ADFG_Fishery.Area))  + 
  geom_bar(stat = "identity", width =0.5)
ggplot(dat_wt_by.area, aes(x=Age_2012, y = n, fill = ADFG_Fishery.Area))  + 
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()


#########################################
#### weighing counts by area population ---------------------------
# match area names to merge files.
# Calculate weighting - compare to Mike's calc - see variable in file
weight_pop
# apply weighting to age frequencies
#dat_wt_by.area # n is counts of observations for each age in each area.
dat_wt_by.area %>% left_join(weight_pop) -> dat_wt_by.area
dat_wt_by.area %>% mutate(n_wt = n*wt_each) -> dat_wt_by.area
wt_all <- ggplot(dat_wt_by.area, aes(x=Age_2012, y = n_wt))  + 
  geom_bar(stat = "identity", width =0.5)

grid.arrange(all, wt_all, nrow=2)


############################
### sea otter -------------------
head(dat_wt)

ggplot(dat_wt, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 

ggplot(dat_wt, aes(x=Age_2012, fill=otter.status)) + 
  geom_density(alpha = 0.3) 
  
head(dat_wt_by.area)
ggplot(dat_wt_by.area, aes(x=Age_2012, y = n_wt, fill = otter.status))  + 
  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?
# bar graph using otter status and wt counts
##################################
### log scale ---------
dat_hist1 %>% 
  mutate()

# perform catch curve analysis on data total and each area