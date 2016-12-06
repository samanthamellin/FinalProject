# We want to look at the emmitance data taken so far to find the full width 50 and 
# Investigate any anomolus readings as necessary. 


setwd("C:\\Users\\smellin\\Desktop\\FinalProject")


#library(tictoc)
#library(boot)
#library(ggplot2)
#library(plotly)


# # load the data sample ; THIS ENDED UP NOT BEING A USEFUL WAY
# To Acess the data
# 
# # this data was all taken on the 23rd of November
# twoPLC.test <- read.csv("CurrentReadings2PLCs_11_23.csv")
# threePLC.test <- read.csv("CurrentReadings3PLCs_11_23.csv")
# fourPLC.test <- read.csv("CurrentReadings4PLCs_11_23.csv")
# fivePLC.test <- read.csv("CurrentReadings5PLCs_11_23.csv")
# sixPLC.test <- read.csv("CurrentReadings6PLCs_11_23.csv")
# # this data was taken perviously, aproximately 30 minutes
# # appart on the 15th of November
# twoA.PLC.test <- read.csv("CurrentReadings2PLC100.csv")
# threeB.PLC.test <- read.csv("CurrentReadings2PLC100num2.csv")

##
# next we need to select the data we are interested in
# we are going to look at the 1st set of currents, the location 
# of the max current, and 5 steps before the max current. 

# due to difficutlties with the stage, and a drift in location of the
# maxima (possibly also associated with stage step mis-match in y)
# we cannot simply say take the middle data point. 
# I had to go though and find the maxima on each of the data sets 
# by eye (which was done in excell)

# maxima <- array(data=NA, dim =  c(7,100))
# maxima_rightshifted <-array(data=NA, dim =  c(7,100))
# first_xprimeLocation <- array(data,NA, dim = c(7,100))

# initally I was going to write a script to do this, but because of the
# lack of consistancy , I have hacked the data and  just made the arrays 
# that I want. 
maxima.test <- read.delim("maxima.txt", header = FALSE, sep="\n")
maxima <- maxima.test[,1]
maxima_rightshifted.test <- read.delim("rightshifted.csv", header = FALSE, sep="\n")
maxima_rightshifted <- maxima_rightshifted.test[,1]
first_xprimeLocation.test <- read.delim("xprime_1.csv", header = FALSE, sep="\n")
first_xprimeLocation <- first_xprimeLocation.test[,1]




# UGH, Reading this is a mess, so I'm gonna continue to hack
# I have to go through and remove any negitive currents, negitive current 
# represents electrons in the detector which are typically repelled
# by the bias applied to the detector. 
# I also want to remove any data that is wildly out of sync with the rest; 
# I also don't think I need to look at any outliers. 
# I expect outliers are caused by errors in reading that are 
# outside of the noise we are looking at, these are things like
# the system being bumped as people walk around the lab, 
# but not the noise caused by the floor vibrating when 
# people are walking around and not physically hitting the 
# system. 
old.par <- par(mfrow=c(1,2))
plot(maxima, main="PreClean")
for (i in 1:700){
  if ( maxima[i] >= (median(maxima)+3*sd(maxima)) || maxima[i] <= (median(maxima)-4*sd(maxima))){
    maxima[i] = median(maxima)
  }
  if (maxima_rightshifted[i]<=0){
    maxima_rightshifted[i] = median(maxima)
  }
  if (first_xprimeLocation[i] <= 0){
    first_xprimeLocation[i] = median(maxima)
  }
}
plot(maxima, main="postClean")
par(old.par)

# looking at the data cleaned up shows me that there wasn't 
# consistent amounts of noise across samples, 
# but that there was no noticible relationship
# between PLC and amount of noise, 

# however we can really check this by running our code from week 7

