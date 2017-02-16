#K.Palof
# Regional catch curve Schute 2007 analysis

#A look at recruitment year choice sensitivity to estimates of Z (mortality)
# schunute and haight 2007
# try using PBStools.https://code.google.com/p/pbs-tools/

# only have to install packages once

######
install.packages("C:/Users/kjpalof/Documents/R/win-library/3.3/PBStools_1.24.20.zip", repos = NULL)
install.packages("C:/Users/kjpalof/Documents/R/win-library/3.3/PBSdata_1.22.10.zip", repos = NULL)
install.packages("C:/Users/kjpalof/Documents/R/win-library/3.3/PBSfishery_1.20.105.zip", repos = NULL)
#install.packages("H:/R/win-library/3.0/PBStools_1.24.20.zip")
#install.packages("C:/Users/kjpalof/Documents/R/win-library/3.2/PBStools_1.24.20.zip")
install.packages("RODBC")
install.packages("BRugs")
install.packages("PBSfishery") # Need to run this using an old version of R.  
#####
library(PBStools)
library(PBSdata)
library(PBSfishery)
#need to have OpenBugs 3.2.2 installed
library(BRugs)

# Example
#####
data(nage394)
?runCCA
runCCA(fnam="nage394", hnam=NULL, ioenv=.GlobalEnv)
#####
# opens a GUI - have to figure out what all of the options here mean.
######################################################################

###############
# regional groups - using Age_2012 - age corrected to 2012 year
present <- read.csv("./output/present_freq.csv")
present %>% select( -otter.status) -> present_cc

present_cc1 <- as.matrix(present_cc[,2:3])
#colnames(GD_woSIT_1214age) <- c("1214")
c <- c(2, 5:85, 87:90, 94,95,101,102,107,112)
c2 <-dput(as.character(c))
rownames(present_cc1) <- c2

runCCA(fnam="present_cc1", hnam=NULL, ioenv=.GlobalEnv)
# opens a GUI - have to figure out what all of the options here mean.
#load data - type in year '1214' here and press get.  then plot.
# change input values for parameters - see excel file ' Schnute2007_catch curve analysis.xls'
# change any parameter bounds and which parameters to estimate (check box)
# press "set" then "NLM"  I'm using the dirichlet and logistic normal distributions.


# regional groups - using Age_2012 - age corrected to 2012 year
absent <- read.csv("./output/absent_freq.csv")
absent %>% select( -otter.status) -> absent_cc

absent_cc1 <- as.matrix(absent_cc[,2:3])
#colnames(GD_woSIT_1214age) <- c("1214")
c <- c(7:10, 13:16, 18:33, 35:76, 78:81,83,86:88,91:96,98,100,102)
c2 <-dput(as.character(c))
rownames(absent_cc1) <- c2

runCCA(fnam="absent_cc1", hnam=NULL, ioenv=.GlobalEnv)