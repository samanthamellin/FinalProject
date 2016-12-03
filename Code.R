# We want to look at the emmitance data taken so far to find the full width 50 and 
# Investigate any anomolus readings as necessary. 


setwd("C:\\Users\\smellin\\Desktop\\FinalProject")


library(tictoc)
library(boot)
library(ggplot2)
library(plotly)


## this will calculate the full width 50's of the files you input
BlackBox <- function(samp) {
  write.csv(samp, file="resample.csv", quote=FALSE, row.names=FALSE)
  as.numeric(shell('C:/Python34/python.exe CalcFW50.py', intern=TRUE))
}

# load the data sample

two.test <- read.csv("CurrentReadings2PLCs_11_23.csv")
three.test <- read.csv("CurrentReadings3PLCs_11_23.csv")
four.test <- read.csv("CurrentReadings4PLCs_11_23.csv")
five.test <- read.csv("CurrentReadings5PLCs_11_23.csv")
six.test <- read.csv("CurrentReadings6PLCs_11_23.csv")