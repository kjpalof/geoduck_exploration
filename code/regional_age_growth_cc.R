#K.Palof 
# Regional groupings / Group 1 and 2 / Sea Otter - presence or absence
# Group Sitka - Biorka/Legma Islands and Taigud/Kolosh Islands
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
library(scales)
library(SDMTools)
options(scipen=9999) # remove scientific notation
theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

## Load data -------------------------------------------------------
dat <- read.csv("./data/12_14_geoduck_all.csv")
weight_pop <- read.csv("./data/GeoduckAgeStudyWeighting.csv")

weight_pop %>%  filter(area != "Biorka/Legma Islands") %>% 
  filter(area != "Taigud/Kolosh Islands") -> weight_pop #remove sitka area since they are grouped under "sitka" now
weight_pop %>% mutate(wt_each = popsize_wshow/(sum(popsize_wshow))) %>% 
  select(-AgeStudyWeighting)-> weight_pop

# need to edit data to make sitka one sample - so group Biorka/Legma Islands and Taigud/Kolosh Islands together
unique(dat$ADFG_Fishery.Area)
dat %>% mutate(area1 = as.character(ADFG_Fishery.Area))  %>% 
        mutate(area = ifelse(area1 == "Biorka/Legma Islands", "Sitka", 
                             ifelse(area1 == "Taigud/Kolosh Islands", 
                                    "Sitka", area1))) %>% 
        select( -area1) -> dat1
  
# full data set with weigtings
dat1 %>% left_join(weight_pop) -> dat2 # raw data
# summarized by area, age with weigthing
dat2 %>% group_by(area, Age_2012) %>% 
  summarise(n = n())  -> dat2_by.area # counts by age for each area 
dat2_by.area %>% left_join(weight_pop) %>% mutate(n_wt = n*wt_each) -> dat_wt_by.area # counts by age/area with weighting

## weighting counts ------------------
dat1 %>% group_by(area) %>% summarise(N_samp = n()) ->total.n.area  # number sampled in each area
dat_wt_by.area %>% left_join(total.n.area) %>% 
  mutate(prop = n/N_samp, n_corrected = prop*popsize_wshow) -> dat_wt_by.area2
# n_corrected is counts using population size weighting.  n_wt is proportion using same weighting
# this is just a scale peference

# melt so that each age counted has it's own row
### weighted histograms--------------
#  need to remember how to do this - sara or ben

### Sea otter (w/ sitka) -------------------
######## age structure visual------------------------------------------------------
head(dat2)
#head(dat_hist2) why is this here?

# unweighted 
ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 
one <- ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "dodge")

ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_density(alpha = 0.3) 

#weighted 
#two <- ggplot(dat_wt_by.area2, aes(x=Age_2012, y = n_wt, fill = otter.status))  + ylab("weighted counts")+
#  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?

#ggplot(dat_wt_by.area2, aes(x=Age_2012, y = n_corrected, fill = otter.status))  + 
#  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?

## histograms --------
head(dat2)
# all areas unweighted
ggplot(dat2, aes(Age_2012, fill = otter.status)) + geom_histogram(binwidth = 1.0, alpha = 0.5, position = "dodge") +
  facet_grid( area ~.)

#regional unweighted
ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "dodge")

################ data sets by sea otter ---------------
dat2 %>% filter(otter.status == "present") -> present_raw
dat2 %>% filter(otter.status == "absent") -> absent_raw
dat_wt_by.area2 %>% filter(otter.status == "present") -> present_wt
dat_wt_by.area2 %>% filter(otter.status == "absent") -> absent_wt

present_wt %>% group_by(otter.status, Age_2012) %>% summarise (count = n())

#### mean age by area -------------
# use dat_wt_by.area2  weight mean age by n_corrected or n_wt(weight)
dat_wt_by.area2 %>% group_by(area) %>% summarise(mean = weighted.mean(Age_2012, n_wt), 
               SE = (wt.sd(Age_2012, n_wt)/(sqrt(sum(!is.na(Age_2012))))), 
               MIN = min(Age_2012, na.rm = TRUE), 
               MAX = max(Age_2012, na.rm = TRUE), 
               n = sum(n))

# mean weighted by the weighted count (n_wt = n*wt_each)
dat_wt_by.area2 %>% group_by(area) %>% summarise(mean = weighted.mean(Age_2012, n_corrected))
# mean weighted by the corrected n (proportion of each age applied to population size)

################# mean Age by group-------------------------------------------
# use dat_wt_by.area2  weight mean age by n_corrected or n_wt(weight)
dat_wt_by.area2 %>% group_by(otter.status) %>% summarise(mean = weighted.mean(Age_2012, n_wt),
                       SE = (wt.sd(Age_2012, n_wt)/(sqrt(sum(!is.na(Age_2012))))), 
                       MIN = min(Age_2012, na.rm = TRUE), 
                       MAX = max(Age_2012, na.rm = TRUE), 
                       n = sum(n))
# mean weighted by the weighted count (n_wt = n*wt_each)
dat_wt_by.area2 %>% group_by(otter.status) %>% summarise(mean = weighted.mean(Age_2012, n_corrected))
# mean weighted by the corrected n (proportion of each age applied to population size)
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
head(present_wt %>% as.data.frame()) # need to take n_corrected here and expand it
# corrected counts based on weight for sea otter present (ee) and absent (ff)
ee <- data.frame(x = rep(present_wt$Age_2012, present_wt$n_corrected), z = rep(present_wt$otter.status, present_wt$n_corrected))

ff <- data.frame(x = rep(absent_wt$Age_2012, absent_wt$n_corrected), z = rep(absent_wt$otter.status, absent_wt$n_corrected))

ks.test(ee$x, ff$x) # reject null that two are equal

ee %>% bind_rows(ff) -> all_n_corrected # expanded counts combined into one data set. x is ages and z is otter.status
# weighted histograms for each group
two <- ggplot(all_n_corrected, aes(x=x, fill = z))+
  geom_histogram(binwidth = 1.0, alpha =0.5, position = "dodge")+ylab("expanded count (x10000s)")+
  xlab("Age_2012")+
  scale_y_continuous(breaks = c(50000, 100000), labels = c(5,10))

#one <- ggplot(dat2, aes(x=Age_2012, fill=otter.status)) + 
#  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "dodge")
### test and reason why I don't want to use the bar graphs
#all_n_corrected %>% group_by(z, x) %>% summarise (n = n()) -> all.n.cort.sum
#ggplot(all.n.cort.sum, aes(x=x, y = n, fill = z))  + ylab("weighted counts")+
#  geom_bar(stat = "identity", width =0.5) # can you add density to this as a bar graph?

# density of ages in each group
png('./figures/hist_density_corrected.png')
ggplot(all_n_corrected, aes(x=x, fill=z)) + 
  geom_density(alpha = 0.3) 
dev.off()

png('./figures/histogram_count_both.png')
grid.arrange(one, two, nrow=2)
dev.off()

# Fisher's Exact test

  
################# growth -------------------------------------------
# L-A, W-L, W-A traditional methods
# use FSA model package

######################################
#Do these seperately for each group - present and absent designates areas

dat2 %>% filter(otter.status == "present") -> present_raw # also done above but can be subsetted here if needed
dat2 %>% filter(otter.status == "absent") -> absent_raw

ggplot(present_raw, aes(Age_2012, Valve.Length.mm)) +geom_point()
ggplot(present_raw, aes(Age_2012, Valve.Weight.g)) +geom_point()
ggplot(present_raw, aes(Valve.Length.mm, WholeClamWeight_g)) +geom_point()

l1 <- logisticFuns()
r1 <- RichardsFuns()
g1 <- GompertzFuns()
#http://derekogle.com/IFAR/supplements/growth/OtherGrowthFuns.html

### Present group growth -------------------------------
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
### plots of fit and bootstrap CIs ----------------
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
fitFrancis <- nls(Valve.Length.mm ~vbFrancis(Age_2012, L1, L2, L3, t1 = ages[1], t3=ages[2]), 
                  data = present_raw, start = sv.francis)
overview(fitFrancis)


#################################################################
## weight - length --------------------------------------
present_rawL
# input file for whole clam weight 
present_rawL %>% filter(!is.na (WholeClamWeight_g)) -> present_rawWL_whole
# input file for valve weight - removes blanks in these for analysis 
present_rawL %>% filter(!is.na (Valve.Weight.g)) -> present_rawWL_valve

# using Length - Weight relationship to estimate Beta
### used to get an estimate of beta parameter for weight-age relationship
############
################### Weight - Length (present) ####################
# valve length -------------
plot(Valve.Weight.g~Valve.Length.mm, data=present_rawWL_valve)

# additive error structure, non-linear fit
wlallo <- Valve.Weight.g ~ (A)*(Valve.Length.mm^B)
fit <- nls(wlallo, data=present_rawWL_valve, start=list(A=0.01, B=1.8))#A=0.01, B=1.8

fitPlot(fit, xlab="Valve Length", ylab="Valve Weight (g)", main="", col.mdl="red")
fit
summary(fit)
overview(fit)

## plotting ##############################################
png('./figures/present_valveweight_length.png')
plot(Valve.Weight.g~Valve.Length.mm,data=present_rawWL_valve,ylab="Valve Weight, g",xlab=" Valve Length, mm", pch=19, ylim=c(0,300), xlim=c(0,220), main="Group 2 sea otter present")
coef1 <- coef(fit)
length2plot <- 0:220
pred <- numeric(length(length2plot))
for(i in 1:length(length2plot)){
  pr<- coef1[1]*((length2plot[i])^coef1[2])
  pred[i] <- pr
}
#adds fitted line
lines(pred~length2plot, type="l", col="red", lwd=3, lty=2)
dev.off()

############## Checking for model assumptions
# using additive error structure
residPlot(fit)
par(mfrow=c(1,1))
#"funneling" from left to right suggests that the variability about the model
##  fairly constant
hist(residuals(fit), main = "")
# assumption of normality is met if this histogram is symmetric without overly long "tails"
##    Not right skewed do not need to change to multiplicative.

plot(resid(fit)~Valve.Length.mm, data=present_rawWL_valve)#look for normality assumptions
#appears that residuals do not have a pattern with age
# additive error structure appears appropriate

##################################################
# whole clam weight -------------
plot(WholeClamWeight_g~Valve.Length.mm, data=present_rawWL_whole)

# additive error structure, non-linear fit
wlallo2 <- WholeClamWeight_g ~ (A)*(Valve.Length.mm^B)
fit_WC <- nls(wlallo2, data=present_rawWL_whole, start=list(A=0.01, B=1.8))#A=0.01, B=1.8

fitPlot(fit_WC, xlab="Valve Length", ylab="Whole Clam Weight (g)", main="", col.mdl="red")
fit_WC
summary(fit_WC)
overview(fit_WC)

## plotting ##############################################
png('./figures/present_wholeclamweight_length.png')
plot(WholeClamWeight_g~Valve.Length.mm,data=present_rawWL_whole,ylab="Whole Clam Weight, g",xlab=" Valve Length, mm", pch=19, ylim=c(0,3700), xlim=c(0,220), main="Group 2 sea otter present")

coef2 <- coef(fit_WC)
length2plot <- 0:220
pred2 <- numeric(length(length2plot))
for(i in 1:length(length2plot)){
  pr<- coef2[1]*((length2plot[i])^coef2[2])
  pred2[i] <- pr
}
#adds fitted line
lines(pred2~length2plot, type="l", col="red", lwd=3, lty=2)
dev.off()

############## Checking for model assumptions
# using additive error structure
residPlot(fit_WC)
par(mfrow=c(1,1))
#"funneling" from left to right suggests that the variability about the model
##  fairly constant
hist(residuals(fit), main = "") # may be right skewed - look into this more 
# assumption of normality is met if this histogram is symmetric without overly long "tails"
##    Not right skewed do not need to change to multiplicative.

plot(resid(fit_WC)~Valve.Length.mm, data=present_rawWL_whole)#look for normality assumptions
#appears that residuals do not have a pattern with age
# additive error structure appears appropriate

#####WEIGHT - Age ------------------------------
# using weight - age relationship
#### valve weight - age ------------------------------
present_rawWL_valve %>% filter(is.na(Age_2012)) # check for ones without ages.

plot(Valve.Weight.g~Age_2012,data=present_rawWL_valve,ylab="Valve Weight (g)",  
     pch=19, xlim=c(0,120), ylim=c(0,300), main="Group 2 present (valve weight)")

#LVB standard growth relationship with Valve weight

svTypical_W <- vbStarts(Valve.Weight.g~Age_2012, data=present_rawWL_valve)
svTypical_W 
unlist(svTypical_W)
# consider using max observed length or close to it as starting value, Linf here is low
#svTypical was created above with starting values
#using nls() to fit the model

#changed to new start values because the ones generated by svTypical caused an error
vbTypical_WV <- Valve.Weight.g~(Winf*(1-exp(-K*(Age_2012-t0)))^b) # the equation to fit
fit_Wval <- nls(vbTypical_WV, data=present_rawWL_valve, start= list(Winf=125, K=0.20, t0=1.62, b=2.7))
# Not working getting error that there's a 
######### "missing value of an infinity produced when evaluating the model"

#fit with t0=0, and B from W-L relationship B=2.71
vbTypical_WVB <- Valve.Weight.g~(Winf*(1-exp(-K*(Age_2012)))^2.71)
fit_WvalB <- nls(vbTypical_WVB, data=present_rawWL_valve, start= list(Winf=150, K=0.05))
fitPlot(fit_WvalB, xlab="Age", ylab="Valve Weight (g)", main="", col.mdl="red")

fit_WvalB
summary(fit_WvalB) ## need to save this for .RMD
overview(fit_WvalB)# output from the fitted object

###########Bootstrap ----------------
boot_WvalB <-nlsBoot(fit_WvalB, niter=1000)
confint(boot_WvalB, plot=TRUE)

estsWVp <-boot_WvalB$coefboot
### plots of fit and bootstrap CIs
ages2plot <- 0:120
#lengths2plot <- 0:204

par(mfrow=c(1,1))

png('./figures/present_valveWlength.png')
fitPlot(fit_WvalB, xlab="Age (2012)", ylab="Valve Weight (g)",xlim=range(ages2plot),main=" Group 2 present")
LCIwv <- UCIwv <- LPIwv <- UPIwv <-numeric(length(ages2plot))
for(i in 1:length(ages2plot)){
  pv <- estsWVp[,"Winf"]*(1-exp(-estsWVp[,"K"]*(ages2plot[i])))^2.71
  LCIwv[i] <-quantile(pv, 0.025)
  UCIwv[i] <-quantile(pv,0.975)
  LPIwv[i] <- quantile(pv - boot_WvalB$rse, 0.025)
  UPIwv[i] <- quantile(pv + boot_WvalB$rse, 0.975)
}
#lines(UCI~ages2plot, type="l", col="blue", lwd=2, lty=2)
#lines(LCI~ages2plot, type="l", col="blue", lwd=2, lty=2)
lines(UPIwv ~ ages2plot, type ="l", col = "red", lwd=2, lty = 2)
lines(LPIwv ~ ages2plot, type ="l", col = "red", lwd=2, lty = 2)
dev.off()


### Absent  group growth -------------------------------
#LVB standard growth relationship with valve length
plot(Valve.Length.mm ~ Age_2012, data = absent_raw)
svR1 <- list(Linf=142, k=0.05, a=1.1, b=0.4)
curve(r1(x, unlist(svR1)), from = 3, to = 120, add=TRUE, lwd=2)

svTypical.a <- vbStarts(Valve.Length.mm~Age_2012, data=absent_raw)
svTypical.a
unlist(svTypical.a)
#svTypical was created above with starting values
#using nls() to fit the model
#
#vbTypical <- Valve.Length.mm~(Linf*(1-exp(-K*(Age_2012-t0)))) # the equation to fit

absent_raw %>% filter(!is.na (Valve.Length.mm)) -> absent_rawL # needed to remove rows without length measurements

#fit1 <- nls(vbTypical, data=present_rawL, start=svTypical.p) # fitting using nls and generated start values
#changed to new start values because the ones generated by svTypical caused an error

#fit model setting t0=0 
svTypical.a # tried to change back to these start values
vbTypical.t0 <- Valve.Length.mm~(Linf*(1-exp(-K*(Age_2012)))) # the equation to fit
fit2a <- nls(vbTypical.t0, data=absent_rawL, start= list(Linf=131, K=0.44))
fitPlot(fit2a, xlab="Age", ylab="Valve Length (mm)", main="", col.mdl="red") 
#### ----save this age-length plot ------------------
fit2a
summary(fit2a)
overview(fit2a)

############## Checking for model assumptions
# and starting points above
residPlot(fit2a)
par(mfrow=c(1,1))
#"funneling" from left to right suggests that the variability about the model
##  is not constant - heteroscedasticity
hist(residuals(fit2a), main = "")
# assumption of normality is met if this histogram is symmetric without overly long "tails"
##    Not really skewed...appears ok...skip changing to multiplicative.

ggplot(absent_rawL, aes(Age_2012, Valve.Length.mm)) +geom_point() 


plot(resid(fit2a)~Age_2012, data=absent_rawL)#look for normality assumptions
#appears that residuals do not have a pattern with age

###########Bootstrap###############
boot2a <-nlsBoot(fit2a, niter=1000)
confint(boot2a, plot=TRUE)

ests.a <-boot2a$coefboot
### plots of fit and bootstrap CIs ----------------
ages2plot <- 0:114
lengths2plot <- 0:204
par(mfrow=c(1,1))

png('./figures/absent_VBL1.png')
fitPlot(fit2a, xlab="Age (2012)", ylab="Valve Length (mm)", xlim=range(ages2plot), main="Regional Group 1 - sea otter absent")
LCI <- UCI <- LPI <- UPI <-numeric(length(ages2plot))
for(i in 1:length(ages2plot)){
  pv <- ests.a[,"Linf"]*(1-exp(-ests.a[,"K"]*(ages2plot[i])))
  LCI[i] <-quantile(pv, 0.025)
  UCI[i] <-quantile(pv,0.975)
  LPI[i] <- quantile(pv - boot2a$rse, 0.025)
  UPI[i] <- quantile(pv + boot2a$rse, 0.975)
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
fitFrancis <- nls(Valve.Length.mm ~vbFrancis(Age_2012, L1, L2, L3, t1 = ages[1], t3=ages[2]), 
                  data = present_raw, start = sv.francis)
overview(fitFrancis)


#################################################################
## weight - length --------------------------------------
absent_rawL
# input file for whole clam weight 
absent_rawL %>% filter(!is.na (WholeClamWeight_g)) -> absent_rawWL_whole
# input file for valve weight - removes blanks in these for analysis 
absent_rawL %>% filter(!is.na (Valve.Weight.g)) -> absent_rawWL_valve

# using Length - Weight relationship to estimate Beta
### used to get an estimate of beta parameter for weight-age relationship
############
################### Weight - Length (present) ####################
# valve length -------------
plot(Valve.Weight.g~Valve.Length.mm, data=absent_rawWL_valve)

# additive error structure, non-linear fit
wlallo <- Valve.Weight.g ~ (A)*(Valve.Length.mm^B)
fit.a <- nls(wlallo, data=absent_rawWL_valve, start=list(A=0.05, B=2.8))#A=0.01, B=1.8

fitPlot(fit.a, xlab="Valve Length", ylab="Valve Weight (g)", main="", col.mdl="red")
fit.a
summary(fit.a)
overview(fit.a)

## plotting ##############################################
png('./figures/absent_valveweight_length.png')
plot(Valve.Weight.g~Valve.Length.mm,data=absent_rawWL_valve,ylab="Valve Weight, g",xlab=" Valve Length, mm", pch=19, ylim=c(0,300), xlim=c(0,220), main="Group 1 sea otter absent")
coef1 <- coef(fit.a)
length2plot <- 0:220
pred <- numeric(length(length2plot))
for(i in 1:length(length2plot)){
  pr<- coef1[1]*((length2plot[i])^coef1[2])
  pred[i] <- pr
}
#adds fitted line
lines(pred~length2plot, type="l", col="red", lwd=3, lty=2)
dev.off()

############## Checking for model assumptions
# using additive error structure
residPlot(fit.a)
par(mfrow=c(1,1))
#"funneling" from left to right suggests that the variability about the model
##  fairly constant
hist(residuals(fit.a), main = "")
# assumption of normality is met if this histogram is symmetric without overly long "tails"
##    Not right skewed do not need to change to multiplicative.

plot(resid(fit.a)~Valve.Length.mm, data=absent_rawWL_valve)#look for normality assumptions
#appears that residuals do not have a pattern with age
# additive error structure appears appropriate

##################################################
# whole clam weight -------------
plot(WholeClamWeight_g~Valve.Length.mm, data=absent_rawWL_whole)
# remove outlier of 5925
absent_rawWL_whole %>% filter(WholeClamWeight_g < 4000) ->absent_rawWL_whole
# additive error structure, non-linear fit
wlallo2 <- WholeClamWeight_g ~ (A)*(Valve.Length.mm^B)
fit_WC.a <- nls(wlallo2, data=absent_rawWL_whole, start=list(A=0.01, B=1.8))#A=0.01, B=1.8

fitPlot(fit_WC.a, xlab="Valve Length", ylab="Whole Clam Weight (g)", main="", col.mdl="red")
fit_WC.a
summary(fit_WC.a)
overview(fit_WC.a)

## plotting ##############################################
png('./figures/absent_wholeclamweight_length.png')
plot(WholeClamWeight_g~Valve.Length.mm,data=absent_rawWL_whole,ylab="Whole Clam Weight, g",xlab=" Valve Length, mm", pch=19, ylim=c(0,3700), xlim=c(0,220), main="Group 1 sea otter absent")

coef2 <- coef(fit_WC.a)
length2plot <- 0:220
pred2 <- numeric(length(length2plot))
for(i in 1:length(length2plot)){
  pr<- coef2[1]*((length2plot[i])^coef2[2])
  pred2[i] <- pr
}
#adds fitted line
lines(pred2~length2plot, type="l", col="red", lwd=3, lty=2)
dev.off()

############## Checking for model assumptions
# using additive error structure
residPlot(fit_WC.a)
par(mfrow=c(1,1))
#"funneling" from left to right suggests that the variability about the model
##  fairly constant
hist(residuals(fit_WC.a), main = "") # may be right skewed - look into this more 
# assumption of normality is met if this histogram is symmetric without overly long "tails"
##    Not right skewed do not need to change to multiplicative.

plot(resid(fit_WC.a)~Valve.Length.mm, data=absent_rawWL_whole)#look for normality assumptions
#appears that residuals do not have a pattern with age
# additive error structure appears appropriate

#####WEIGHT - Age ------------------------------
# using weight - age relationship
#### valve weight - age ------------------------------
absent_rawWL_valve %>% filter(is.na(Age_2012)) # check for ones without ages.

plot(Valve.Weight.g~Age_2012,data=absent_rawWL_valve,ylab="Valve Weight (g)",  
     pch=19, xlim=c(0,120), ylim=c(0,300), main="Group 1 present (valve weight)")

#LVB standard growth relationship with Valve weight

svTypical_W <- vbStarts(Valve.Weight.g~Age_2012, data=absent_rawWL_valve)
svTypical_W 
unlist(svTypical_W)
# consider using max observed length or close to it as starting value, Linf here is low
#svTypical was created above with starting values
#using nls() to fit the model

#changed to new start values because the ones generated by svTypical caused an error
#vbTypical_WV <- Valve.Weight.g~(Winf*(1-exp(-K*(Age_2012-t0)))^b) # the equation to fit
#fit_Wval <- nls(vbTypical_WV, data=present_rawWL_valve, start= list(Winf=125, K=0.20, t0=1.62, b=2.7))
# Not working getting error that there's a 
######### "missing value of an infinity produced when evaluating the model"

#fit with t0=0, and B from W-L relationship B=1.84
vbTypical_WVB.a <- Valve.Weight.g~(Winf*(1-exp(-K*(Age_2012)))^1.84)
fit_WvalB.a <- nls(vbTypical_WVB.a, data=absent_rawWL_valve, start= list(Winf=150, K=0.05))
fitPlot(fit_WvalB.a, xlab="Age", ylab="Valve Weight (g)", main="", col.mdl="red")

fit_WvalB.a
summary(fit_WvalB.a) ## need to save this for .RMD
overview(fit_WvalB.a)# output from the fitted object

###########Bootstrap ----------------
boot_WvalB.a <-nlsBoot(fit_WvalB.a, niter=1000)
confint(boot_WvalB.a, plot=TRUE)

estsWVa <-boot_WvalB.a$coefboot
### plots of fit and bootstrap CIs
ages2plot <- 0:120
#lengths2plot <- 0:204

par(mfrow=c(1,1))

png('./figures/absent_valveWlength.png')
fitPlot(fit_WvalB.a, xlab="Age (2012)", ylab="Valve Weight (g)",xlim=range(ages2plot),main=" Group 1 absent")
LCIwv <- UCIwv <- LPIwv <- UPIwv <-numeric(length(ages2plot))
for(i in 1:length(ages2plot)){
  pv <- estsWVa[,"Winf"]*(1-exp(-estsWVa[,"K"]*(ages2plot[i])))^1.84
  LCIwv[i] <-quantile(pv, 0.025)
  UCIwv[i] <-quantile(pv,0.975)
  LPIwv[i] <- quantile(pv - boot_WvalB.a$rse, 0.025)
  UPIwv[i] <- quantile(pv + boot_WvalB.a$rse, 0.975)
}
#lines(UCI~ages2plot, type="l", col="blue", lwd=2, lty=2)
#lines(LCI~ages2plot, type="l", col="blue", lwd=2, lty=2)
lines(UPIwv ~ ages2plot, type ="l", col = "red", lwd=2, lty = 2)
lines(LPIwv ~ ages2plot, type ="l", col = "red", lwd=2, lty = 2)
dev.off()
