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
levels(weight_pop$area)
glimpse(dat)

dat %>% left_join(weight_pop) -> dat_wt

### summarized by area / age-------------
# this doesn't give 0's for missing ages...
dat %>% group_by(ADFG_Fishery.Area, Age_2012) %>% 
  summarise(n = n()) -> dat_hist

## summary statistics raw ------------
dat %>% group_by(ADFG_Fishery.Area) %>% 
  summarise(min = min(Age_2012), mean = mean(Age_2012), median = median(Age_2012), max =max(Age_2012)) -> sumstats

### save tables and figures if needed ------------------------------------
write_csv(sumstats, 'output/raw_summary.csv')


############################################  
### histograms - no weightings by population size -------------
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

#with lines for means
ggplot(dat, aes(x=Age_2012, fill=ADFG_Fishery.Area)) +
  geom_histogram(binwidth=1.0, alpha=.5, position="identity")+
  geom_vline(data = sumstats, aes(xintercept = mean, colour = ADFG_Fishery.Area), linetype = "dashed", size =1)

ggplot(dat, aes(x=Age_2012, fill=ADFG_Fishery.Area)) + geom_density(alpha=.3)
  
ggplot(dat, aes(x=Age_2012)) + geom_histogram(binwidth=1.5, colour="black", fill="white") + 
  facet_grid( ADFG_Fishery.Area~ .)

### bar graphs from summarized data -------------------
dat_hist
all <- ggplot(dat_hist, aes(x=Age_2012, y = n))  + 
  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?


ggplot(dat_hist, aes(x=Age_2012, y = n, fill = ADFG_Fishery.Area))  + 
  geom_bar(stat = "identity", width =0.5)
ggplot(dat_hist, aes(x=Age_2012, y = n, fill = ADFG_Fishery.Area))  + 
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()


#########################################
#### weighing counts by area population ---------------------------
# match area names to merge files.
# Calculate weighting - compare to Mike's calc - see variable in file
weight_pop
# apply weighting to age frequencies
dat_hist # n is counts of observations for each age in each area.
dat_hist %>% left_join(weight_pop) -> dat_hist1
dat_hist1 %>% mutate(n_wt = n*wt_each) -> dat_hist1
wt_all <- ggplot(dat_hist1, aes(x=Age_2012, y = n_wt))  + 
  geom_bar(stat = "identity", width =0.5)

grid.arrange(all, wt_all, nrow=2)


############################
### sea otter -------------------
head(dat_wt)

ggplot(dat_wt, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 

ggplot(dat_wt, aes(x=Age_2012, fill=otter.status)) + 
  geom_density(alpha = 0.3) 
  



# perform catch curve analysis on data total and each area