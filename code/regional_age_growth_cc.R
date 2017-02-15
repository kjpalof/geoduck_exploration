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
library(nlstools)
library(AICcmodavg)
library(ggplot2)

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

# melt so that each age counted has it's own row


### Sea otter (w/ sitka) -------------------
################# age structure visual------------------------------------------------------
head(dat2)
head(dat_hist2) 

# unweighted 
ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 
one <- ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, position = "dodge") 

ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_density(alpha = 0.3) 

#weighted 
two <- ggplot(dat_wt_by.area2, aes(x=Age_2012, y = n_wt, fill = otter.status))  + 
  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?

ggplot(dat_wt_by.area2, aes(x=Age_2012, y = n_corrected, fill = otter.status))  + 
  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?

grid.arrange(one, two, nrow=2)
################ data sets by sea otter ---------------
dat2 %>% filter(otter.status == "present") -> present_raw
dat2 %>% filter(otter.status == "absent") -> absent_raw
dat_wt_by.area2 %>% filter(otter.status == "present") -> present_wt
dat_wt_by.area2 %>% filter(otter.status == "absent") -> absent_wt

present_wt %>% group_by(otter.status, Age_2012) %>% summarise (count = n())

################# mean Age -------------------------------------------
# use dat_wt_by.area2  weight mean age by n_corrected or n_wt(weight)
dat_wt_by.area2 %>% group_by(otter.status) %>% summarise(mean = weighted.mean(Age_2012, n_wt))
dat_wt_by.area2 %>% group_by(otter.status) %>% summarise(mean = weighted.mean(Age_2012, n_corrected))

wtd.t.test(x = present_wt$Age_2012, y = absent_wt$Age_2012, weight= present_wt$n_wt, 
           weighty = absent_wt$n_wt) # not significantly different
 
t.test(x = present_raw$Age_2012, y = absent_raw$Age_2012) # are significantly different
wilcox.test(x = present_raw$Age_2012, y = absent_raw$Age_2012)
wilcox.test(dat2$Age_2012 ~ dat2$otter.status)

kruskal.test(dat2$Age_2012 ~ dat2$otter.status)
# which to use?????? ##
# Mann - Whitney U test


################# age Composition Differences -------------------------------------------
#do this on raw unweighted data
ks.test(present_raw$Age_2012, absent_raw$Age_2012) #reject null that the two are equal

# bin data for ks-test

# need to do this with weighted data  


# Fisher's Exact test

  
################# growth -------------------------------------------
# L-A, W-L, W-A traditional methods
# use FSA model package

######################################
#Do these seperately for each group - present and absent designates areas
#######################AREA 1#####################
dat2 %>% filter(otter.status == "present") -> present_raw # also done above but can be subsetted here if needed
dat2 %>% filter(otter.status == "absent") -> absent_raw

ggplot(present_raw, aes(Age_2012, Valve.Length.mm)) +geom_point()
ggplot(present_raw, aes(Age_2012, Valve.Weight.g)) +geom_point()
ggplot(present_raw, aes(Valve.Length.mm, WholeClamWeight_g)) +geom_point()

l1 <- logisticFuns()
r1 <- RichardsFuns()
g1 <- GompertzFuns()
#http://derekogle.com/IFAR/supplements/growth/OtherGrowthFuns.html

#LVB standard growth relationship with valve length
plot(Valve.Length.mm ~ Age_2012, data = present_raw)
svR1 <- list(Linf=142, k=0.05, a=1.1, b=0.4)
curve(r1(x, unlist(svR1)), from = 3, to = 120, add=TRUE, lwd=2)

svTypical.p <- vbStarts(Valve.Length.mm~Age_2012, data=present_raw)
svTypical.p
unlist(svTypical.p)
#svTypical was created above with starting values
#using nls() to fit the model
#
vbTypical <- Valve.Length.mm~(Linf*(1-exp(-K*(Age_2012-t0)))) # the equation to fit

present_raw %>% filter(!is.na (Valve.Length.mm)) -> present_rawL # needed to remove rows without length measurements

fit1 <- nls(vbTypical, data=present_rawL, start=svTypical.p) # fitting using nls and generated start values
#changed to new start values because the ones generated by svTypical caused an error

#fit model setting t0=0 
svTypical.p # tried to change back to these start values
vbTypical.t0 <- Valve.Length.mm~(Linf*(1-exp(-K*(Age_2012)))) # the equation to fit
fit2 <- nls(vbTypical.t0, data=present_rawL, start= list(Linf=142, K=0.58))
fitPlot(fit2, xlab="Age", ylab="Valve Length (mm)", main="", col.mdl="red") 
#### ----save this age-length plot ------------------
fit2
summary(fit2)
overview(fit2)

############## Checking for model assumptions
# and starting points above
residPlot(fit2)
par(mfrow=c(1,1))
#"funneling" from left to right suggests that the variability about the model
##  is not constant - heteroscedasticity
hist(residuals(fit2), main = "")
# assumption of normality is met if this histogram is symmetric without overly long "tails"
##    Not really skewed...appears ok...skip changing to multiplicative.

ggplot(present_rawL, aes(Age_2012, Valve.Length.mm)) +geom_point() 


plot(resid(fit2)~Age_2012, data=present_rawL)#look for normality assumptions
#appears that residuals do not have a pattern with age

###########Bootstrap###############
boot2 <-nlsBoot(fit2, niter=1000)
confint(boot2, plot=TRUE)

ests <-boot2$coefboot
### plots of fit and bootstrap CIs
ages2plot <- 0:114
lengths2plot <- 0:204
par(mfrow=c(1,1))

png('./figures/present_VBL1.png')
fitPlot(fit2, xlab="Age (2012)", ylab="Valve Length (mm)", xlim=range(ages2plot), main="Regional Group 2 - sea otter present")
LCI <- UCI <- LPI <- UPI <-numeric(length(ages2plot))
for(i in 1:length(ages2plot)){
  pv <- ests[,"Linf"]*(1-exp(-ests[,"K"]*(ages2plot[i])))
  LCI[i] <-quantile(pv, 0.025)
  UCI[i] <-quantile(pv,0.975)
  LPI[i] <- quantile(pv - boot2$rse, 0.025)
  UPI[i] <- quantile(pv + boot2$rse, 0.975)
}
#lines(UCI~ages2plot, type="l", col="blue", lwd=2, lty=2)
#lines(LCI~ages2plot, type="l", col="blue", lwd=2, lty=2)
lines(UPI ~ ages2plot, type ="l", col = "red", lwd=2, lty = 2)
lines(LPI ~ ages2plot, type ="l", col = "red", lwd=2, lty = 2)
dev.off()
#ggsave("./figures/present_VBL.png", dpi=300, height=4.5, width=6.5, units="in")

#prediction bounds - add and subtract the RSE - redidual standard error from each bootstrap model
### save this graph for RMD ---------------

## other parametrization ----------
# Galucci and Quinn 1979
# Schnute (Quinn II and Deriso 1999)

# p.18
sv.schnute <- vbStarts(Valve.Length.mm~Age_2012, data=present_raw, type = "Schnute")
sv.galqu <- vbStarts(Valve.Length.mm~Age_2012, data=present_raw, type = "GallucciQuinn")
sv.francis <- vbStarts(Valve.Length.mm~Age_2012, data=present_raw, type = "Francis", tFrancis = c(9,95))
sv.galqu
sv.schnute
sv.francis

# Francis parameterization
ages <- c(9,95)
vbFrancis <-vbFuns("Francis")
fitFrancis <- nls()

#################################################################
## weight - length --------------------------------------
GD14growth_WL <- completeFun (GD14growth_L, "Valve.Weight.g")
str(GD14growth_WL)
summary(GD14growth_WL)
# made new input file that removes any missing values from either length 
#   or weight. 

# using Length - Weight relationship to estimate Beta
#weight-length models 
### used to get an estimate of beta parameter for weight-age relationship
############
################### Area 1 ####################
GD14growth_WL1 <- subset(GD14growth_WL, Discrete.Sample.ID=="14DS~1")

plot(Valve.Weight.g~Valve.Length.mm, data=GD14growth_WL1)
length(GD14growth_WL1$Valve.Weight.g)
# additive error structure, non-linear fit
wlallo <- Valve.Weight.g ~ (A)*(Valve.Length.mm^B)
fit <- nls(wlallo, data=GD14growth_WL1, start=list(A=0.01, B=1.8))

fitPlot(fit, xlab="Valve Length", ylab="Valve Weight (g)", main="", col.mdl="red")
fit
summary(fit)
overview(fit)

## plotting ##############################################
plot(Valve.Weight.g~Valve.Length.mm,data=GD14growth_WL1,ylab="Valve Weight, g",xlab=" Valve Length, mm", pch=19, ylim=c(0,300), xlim=c(0,220), main="2014 Area 1")

length2plot <- 0:220
pred <- numeric(length(length2plot))
for(i in 1:length(length2plot)){
  pr<- 0.02452*((length2plot[i])^1.71643)
  pred[i] <- pr
}
#adds fitted line
lines(pred~length2plot, type="l", col="red", lwd=3, lty=2)


############## Checking for model assumptions
# using additive error structure
residPlot(fit)
par(mfrow=c(1,1))
#"funneling" from left to right suggests that the variability about the model
##  fairly constant
hist(residuals(fit), main = "")
# assumption of normality is met if this histogram is symmetric without overly long "tails"
##    Not right skewed do not need to change to multiplicative.

plot(resid(fit)~Valve.Length.mm, data=GD14growth_WL1)#look for normality assumptions
#appears that residuals do not have a pattern with age
# additive error structure appears appropriate
################# Catch curve traditional -------------------------------------------
