#K.Palof 
# Catch curve anlysis for these two groups.
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
library(nlstools)
library(AICcmodavg)
library(FSAdata)
options(scipen=9999) # remove scientific notation
theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
## Load data -------------------------------------------------------
dat <- read.csv("./data/12_14_geoduck_all.csv")
weight_pop <- read.csv("./data/GeoduckAgeStudyWeighting.csv")

weight_pop %>% mutate(ADFG_Fishery.Area = area, wt_each = popsize_wshow/(sum(popsize_wshow))) %>% 
  select (-area) %>% select(-AgeStudyWeighting)-> weight_pop

# full data set with weigtings
dat %>% left_join(weight_pop) -> dat2 # raw data
# summarized by area, age with weigthing
dat2 %>% group_by(ADFG_Fishery.Area, Age_2012) %>% 
  summarise(n = n())  -> dat2_by.area
dat2_by.area %>% left_join(weight_pop) %>% mutate(n_wt = n*wt_each) -> dat_wt_by.area # counts by age/area with weighting

## weighting counts 
dat %>% group_by(ADFG_Fishery.Area) %>% summarise(N_samp = n()) ->total.n.area
dat_wt_by.area %>% left_join(total.n.area) %>% 
  mutate(prop = n/N_samp, n_corrected = prop*popsize_wshow) -> dat_wt_by.area2
# n_corrected is counts using population size weighting.  n_wt is proportion using same weighting
# this is just a scale peference

################ data sets by sea otter ---------------
dat2 %>% filter(otter.status == "present") -> present_raw
dat2 %>% filter(otter.status == "absent") -> absent_raw
dat_wt_by.area2 %>% filter(otter.status == "present") -> present_wt
dat_wt_by.area2 %>% filter(otter.status == "absent") -> absent_wt

# one data set with ages as individual entries
ee <- data.frame(x = rep(present_wt$Age_2012, present_wt$n_corrected), z = rep(present_wt$otter.status, present_wt$n_corrected))
ff <- data.frame(x = rep(absent_wt$Age_2012, absent_wt$n_corrected), z = rep(absent_wt$otter.status, absent_wt$n_corrected))
ee %>% bind_rows(ff) -> all_n_corrected # expanded counts combined into one data set. x is ages and z is otter.status
# weighted histograms
ggplot(all_n_corrected, aes(x=x, fill = z))+
  geom_histogram(binwidth = 1.0, alpha =0.5, position = "dodge")+ylab("expanded count (x10000s)")+
  xlab("Age_2012")+
  scale_y_continuous(breaks = c(50000, 100000), labels = c(5,10))

## Need to use the new method to report catch curve analysis and estimates.

#### data prep PRESENT ------------------- 
# want count by age for each group 
head(present_wt) # Age_2012 and n (unweighted age counts) or n_corrected (weighted age counts)

present_wt %>% group_by(otter.status, Age_2012) %>% summarise( count = sum(n), Wcount = sum(n_corrected)) -> present_freq
present_freq %>% mutate(logcount = log(count), logWcount = log(Wcount)) -> present_freq
write_csv(present_freq, 'output/present_freq.csv')

present_freq %>% select(otter.status,Age_2012, logcount, logWcount ) %>% 
  gather( key, value, -otter.status, -Age_2012) ->attempt1
  
ggplot(attempt1, aes(Age_2012, value, colour = key)) +geom_point()

ggplot(present_freq, aes(Age_2012, logWcount)) +geom_point()
ggplot(present_freq, aes(Age_2012, Wcount)) +geom_point()

# Are there any zero observations, if so remove them
present_freq$Wcount == 0

# plot just the right hand tail
str(agefreq2)
plot(logWcount[60:92]~Age_2012[60:92],data=present_freq,main ="Present group,  just 'peak' to right tail, samples > 0", xlab = "Age", ylab="ln(frequency)",pch=19)

## Catch curve models - present group ----------------------------------
#catch-curve models here using catchCurve function - can tell the function
max(present_freq$Wcount)
fitCC1 <- catchCurve(logWcount~Age_2012, data=present_freq, ages2use=63:85) 
#max count age (63) to the first age with no obsrevations - 86

summary(fitCC1)
confint(fitCC1)
plot(fitCC1)

fitCC2 <- catchCurve(logWcount~Age_2012, data=present_freq, ages2use=63:89) 
#need to use raw data for this.  it transforms the frequencies, 
#89 is last observation >1
summary(fitCC2)
confint(fitCC2)
plot(fitCC2, main = "present")

# use first "peak"
ggplot(present_freq, aes(Age_2012, Wcount)) +geom_point()
fitCC3 <- catchCurve(logWcount~Age_2012, data=present_freq, ages2use=26:85)
#need to use raw data for this.  it transforms the frequencies
#26 is the first peak in count data
summary(fitCC3)
confint(fitCC3)
plot(fitCC3)

fitCC4 <- catchCurve(logWcount~Age_2012, data=present_freq, ages2use=26:89) 
#need to use raw data for this.  it transforms the frequencies
# no truncation at the end.
summary(fitCC4)
confint(fitCC4)
plot(fitCC4)

# No truncation
fitCC2a <- catchCurve(logWcount~Age_2012, data=present_freq, ages2use=63:112) 
#need to use raw data for this.  it transforms the frequencies, 
summary(fitCC2a)
confint(fitCC2a)
plot(fitCC2a)
# No truncation
fitCC4a <- catchCurve(logWcount~Age_2012, data=present_freq, ages2use=26:112) 
#need to use raw data for this.  it transforms the frequencies, 
summary(fitCC4a)
confint(fitCC4a)
plot(fitCC4a)

#### data prep ABSENT ------------------- 
# want count by age for each group 
head(absent_wt) # Age_2012 and n (unweighted age counts) or n_corrected (weighted age counts)

absent_wt %>% group_by(otter.status, Age_2012) %>% summarise( count = sum(n), Wcount = sum(n_corrected)) -> absent_freq
absent_freq %>% mutate(logcount = log(count), logWcount = log(Wcount)) -> absent_freq
write_csv(absent_freq, 'output/absent_freq.csv')
absent_freq %>% select(otter.status,Age_2012, logcount, logWcount ) %>% 
  gather( key, value, -otter.status, -Age_2012) ->attempt2

ggplot(attempt2, aes(Age_2012, value, colour = key)) +geom_point()

ggplot(absent_freq, aes(Age_2012, logWcount)) +geom_point()
ggplot(absent_freq, aes(Age_2012, Wcount)) +geom_point()

# Are there any zero observations, if so remove them
absent_freq$Wcount == 0

# plot just the right hand tail
absent_freq$logWcount
plot(logWcount[38:83]~Age_2012[38:83],data=absent_freq,main ="absent group,  just 'peak' to right tail, samples > 0", xlab = "Age", ylab="ln(frequency)",pch=19)

## Catch curve models - absent group ----------------------------------
#catch-curve models here using catchCurve function - can tell the function
max(absent_freq$Wcount)
fitCC1 <- catchCurve(logWcount~Age_2012, data=absent_freq, ages2use=50:76) 
#max count age (50) to the first age with no obsrevations - 77

summary(fitCC1)
confint(fitCC1)
plot(fitCC1)

fitCC2 <- catchCurve(logWcount~Age_2012, data=absent_freq, ages2use=50:96) 
#need to use raw data for this.  it transforms the frequencies, 
#96 is last observation >1
summary(fitCC2)
confint(fitCC2)
plot(fitCC2, main = "absent")

# use first "peak"
ggplot(absent_freq, aes(Age_2012, Wcount)) +geom_point()
fitCC3 <- catchCurve(logWcount~Age_2012, data=absent_freq, ages2use=26:76)
#need to use raw data for this.  it transforms the frequencies
#26 is the first peak in count data
summary(fitCC3)
confint(fitCC3)
plot(fitCC3)

fitCC4 <- catchCurve(logWcount~Age_2012, data=absent_freq, ages2use=26:96) 
#need to use raw data for this.  it transforms the frequencies
# no truncation at the end.
summary(fitCC4)
confint(fitCC4)
plot(fitCC4)

# No truncation
fitCC2a <- catchCurve(logWcount~Age_2012, data=absent_freq, ages2use=50:102) 
#need to use raw data for this.  it transforms the frequencies, 
summary(fitCC2a)
confint(fitCC2a)
plot(fitCC2a)
# No truncation
fitCC4a <- catchCurve(logWcount~Age_2012, data=absent_freq, ages2use=26:102) 
#need to use raw data for this.  it transforms the frequencies, 
summary(fitCC4a)
confint(fitCC4a)
plot(fitCC4a)
