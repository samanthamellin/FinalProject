# We want to look at the emmitance data taken so far to find the full width 50 and 
# Investigate any anomolus readings as necessary. 


setwd("C:\\Users\\smellin\\Desktop\\FinalProject")


#library(tictoc)
#library(boot)
#library(ggplot2)
#library(plotly)


# load the data sample

# this data was all taken on the 23rd of November
twoPLC.test <- read.csv("CurrentReadings2PLCs_11_23.csv")
threePLC.test <- read.csv("CurrentReadings3PLCs_11_23.csv")
fourPLC.test <- read.csv("CurrentReadings4PLCs_11_23.csv")
fivePLC.test <- read.csv("CurrentReadings5PLCs_11_23.csv")
sixPLC.test <- read.csv("CurrentReadings6PLCs_11_23.csv")
# this data was taken perviously, aproximately 30 minutes
# appart on the 15th of November
twoA.PLC.test <- read.csv("CurrentReadings2PLC100.csv")
threeB.PLC.test <- read.csv("CurrentReadings2PLC100num2.csv")

# now we need to fix the orentation of this data
# we want to be able to compaire areas that are
# at the same x' value to see how noise was effected
# across sampling rates so we will make an array
# where each column is a new x' for each of our data
twoPLC.currents<-array(data = NA, dim = c(100, 20))
twoPLC.xPrime <-c()
