setwd("C:\\Users\\smellin\\Desktop\\FinalProject")

library(tictoc)
library(boot)
library(ggplot2)
library(plotly)
# define the process by which we will resample and determine performance in one of its passes
BlackBox <- function(samp) {
  write.csv(samp, file="resample.csv", quote=FALSE, row.names=FALSE)
  as.numeric(shell('C:/Python34/python.exe CalcFW50.py', intern=TRUE))
}

# load the data sample
OnePLC <- read.csv("CurrentReadings1PLC100.csv")
# TwoPLC <- read.csv("CurrentReadings2PLC100.csv")
# ThreePLC <- read.csv("CurrentReadings3PLC100.csv")
# # second round of same data
# OnePLC.two <- read.csv("CurrentReadings1PLC100num2.csv")
# TwoPLC.two <- read.csv("CurrentReadings2PLC100num2.csv")
# ThreePLC.two <- read.csv("CurrentReadings3PLC100num2.csv")
# 
# #Testing evens and odds(11/23)
# two.test <- read.csv("CurrentReadings2PLCs_11_23.csv")
# three.test <- read.csv("CurrentReadings3PLCs_11_23.csv")
# four.test <- read.csv("CurrentReadings4PLCs_11_23.csv")
# five.test <- read.csv("CurrentReadings5PLCs_11_23.csv")
# six.test <- read.csv("CurrentReadings6PLCs_11_23.csv")

#data.medium <- read.csv("MediumOnePLC.csv", header = TRUE)
#data.slow <- read.csv("Slow6PLC.csv", header = TRUE)
len <- 100
num.pos <- 21
heads <- c("number of current readings","number of resamples","low bound","estimate","high bound")
results <- c()
fw50vals <- c(1:100)

# the thirty sample sizes
sample.sizes <- 1:30
iterations <- 350
#clear out values

#function that resamples some of the data
Resample <- function(data){
  for (j in sample.sizes) {
    # desired size of subsample
    N <- j
    results.raw <- c()
    # desired number of times to subsample
    for (i in 1:iterations) {
      data.subsampled.averaged <- c()
      #aggregate(Current ~ X, One, function(x) min(x))
      for(pos in 1:num.pos){
        data.subsampled <- data[(len*(pos-1)) + sample(1:len, N,)] #grab a random collection of rows in current position
        the.next.row <- with(data.subsampled, c(median(FIBX), median(FIBY), median(X), median(Y), median(Current)))
        data.subsampled.averaged <- rbind(data.subsampled.averaged, the.next.row)
      }
      results.raw <- c(results.raw, BlackBox(data.subsampled.averaged))
      #smp <- sample(seq(1,len,1),N)
      #result <- c(result, BlackBox(smp))
    }
    fw50vals <- rbind(fw50vals, results.raw)
    
    confidence.HW <- 1.96 * sd(results.raw)
    FW50_mean <- mean(results.raw)
    FW50_lo <- FW50_mean - confidence.HW
    FW50_hi <- FW50_mean + confidence.HW
    
    data.vals <- c(N,iterations,FW50_lo,FW50_mean,FW50_hi)
    results <- rbind(results,data.vals)
    
    # echo results
    #paste("for", N, "samples:")
    #paste("Energy spread FW50 is ",FW50_mean,"rads")
    #paste("We're 95% confident that FW50 is between",FW50_lo,"and",FW50_hi)
  }  
  #write.csv(fw50vals, file="FW50_valuesOnePLC.csv", quote=FALSE, row.names=FALSE)
  colnames(results) <- heads
  results
}


subset <- 100
Boostrap <- function(data){
    # desired size of subsample
    N <- subset
    rawResults <- c()
    fw50vals <- c()
    
    data.iterated <- c()
    # desired number of times to subsample
    for (i in 1:1000) {
      data.subsampled.averaged <- c()
      new.data.set <- c()
      #aggregate(Current ~ X, One, function(x) min(x))
      for(pos in 1:num.pos){
        data.subsampled <- data[(len*(pos-1)) + sample(1:len, N, replace = TRUE),] #grab a random collection of rows in current position
        the.next.row <- with(data.subsampled, c(median(FIBX), median(FIBY), median(X), median(Y), median(Current)))
        data.subsampled.averaged <- rbind(data.subsampled.averaged, the.next.row)
        new.data.set <- cbind(new.data.set, mean(data.subsampled$Current))
      }
      data.iterated <- rbind(data.iterated, new.data.set)
      #rawResults <- c(rawResults, BlackBox(data.subsampled.averaged))
      #smp <- sample(seq(1,len,1),N)
      #result <- c(result, BlackBox(smp))
      fw50vals <- rbind(fw50vals, BlackBox(data.subsampled.averaged))
    }
    #confidence.HW <- 1.96 * sd(rawResults)
    #FW50_mean <- mean(rawResults)
    #FW50_lo <- FW50_mean - confidence.HW
    #FW50_hi <- FW50_mean + confidence.HW
    
    #data.vals <- c(N,iterations,FW50_lo,FW50_mean,FW50_hi)
    #results <- rbind(results,data.vals)
    #allData <- list(results, rawResults)
    # echo results
  #paste("for", N, "samples:")
  #paste("Energy spread FW50 is ",FW50_mean,"rads")
  #paste("We're 95% confident that FW50 is between",FW50_lo,"and",FW50_hi)
  write.csv(fw50vals, file="FW50_valuesThreePLC.csv", quote=FALSE, row.names=FALSE)
  #colnames(results) <- heads
  data.iterated
}
#saves the 350 different FW50 values found via bootstrapping 100 points from each location
write.csv(Boostrap(three.test), file = "FW50bootstrap_allData.csv", quote = FALSE, row.names = FALSE)
  # write results to disk
bootstrap.data <- read.csv("FW50bootstrap_allData.csv")
Three.data <- read.csv("FW50_valuesThreePLC.csv")
confidence.HW <- 1.96 * sd(Three.data$V1)
FW50_mean <- mean(Three.data$V1)
FW50_lo <- FW50_mean - confidence.HW
FW50_hi <- FW50_mean + confidence.HW

three.percent.uncertainty <- ((confidence.HW)/FW50_mean)*100

boxplot(bootstrap.data, xlab = "position number", ylab = "Current value (pA)", main = "Boxplot of mean current from 1000 bootstrap samples")

x.axis.boot <- list(title = "Bootstrapping of 100", showticklabels = TRUE)
y.axis.boot <- list(title = "Percentage away from ideal FW50 value", showticklabels = TRUE, exponentformat = "E")

plot_ly(y = Three.data$V1, type = "box", name = "Bootstraping") %>%
  layout(title = "FW50 values from boostrapping data 1000 times",xaxis = x.axis.boot, yaxis = y.axis.boot)

#write.csv(results, file="FW50_summary.csv", quote=FALSE, row.names=FALSE)
#write.csv(fw50vals, file="FW50_values.csv", quote=FALSE, row.names=FALSE)
write.csv(Resample(OnePLC), file = "FW50_summaryTwo.csv", quote = FALSE, row.names = FALSE)
# write.csv(Resample(three.test), file = "FW50_summaryThree.csv", quote = FALSE, row.names = FALSE)
# write.csv(Resample(four.test), file = "FW50_summaryFour.csv", quote = FALSE, row.names = FALSE)
# write.csv(Resample(five.test), file = "FW50_summaryFive.csv", quote = FALSE, row.names = FALSE)
# write.csv(Resample(six.test), file = "FW50_summarySix.csv", quote = FALSE, row.names = FALSE)

#write.csv(Resample(OnePLC), file="FW50_summaryOnePLC.csv", quote=FALSE, row.names=FALSE)
#write.csv(fw50vals, file="FW50_valuesOnePLC.csv", quote=FALSE, row.names=FALSE)
write.csv(Resample(data.medium), file = "FW50_summaryMedium.csv", quote = FALSE, row.names = FALSE)
write.csv(Resample(data.slow), file = "FW50_summarySlow.csv", quote = FALSE, row.names = FALSE)

write.csv(Resample(OnePLC.two), file = "FW5_summaryOnePLCround2.csv", quote = FALSE, row.names = FALSE)
write.csv(Resample(TwoPLC.two), file = "FW5_summaryTwoPLCround2.csv", quote = FALSE, row.names = FALSE)
write.csv(Resample(ThreePLC.two), file = "FW5_summaryThreePLCround2.csv", quote = FALSE, row.names = FALSE)

write.csv(Resample(TwoPLC), file="FW50_summaryTwoPLC.csv", quote=FALSE, row.names=FALSE)
#write.csv(fw50vals, file="FW50_valuesTwoPLC.csv", quote=FALSE, row.names=FALSE)

write.csv(Resample(ThreePLC), file="FW50_summaryThreePLC.csv", quote=FALSE, row.names=FALSE)
#write.csv(fw50vals, file="FW50_valuesThreePLC.csv", quote=FALSE, row.names=FALSE)

OnePLC.summary <- read.csv("FW50_summaryOnePLC.csv")
TwoPLC.summary <- read.csv("FW50_summaryTwoPLC.csv")
ThreePLC.summary <- read.csv("FW50_summaryThreePLC.csv")

OnePLC.summary.two <- read.csv("FW5_summaryOnePLCround2.csv")
TwoPLC.summary.two <- read.csv("FW5_summaryTwoPLCround2.csv")
ThreePLC.summary.two <- read.csv("FW5_summaryThreePLCround2.csv")

medium.summary <- read.csv("FW50_summaryMedium.csv")
slow.summary <- read.csv("FW50_summarySlow.csv")

#Checking even vs odd rates
two.summary <- read.csv("FW50_summaryTwo.csv")
three.summary <- read.csv("FW50_summaryThree.csv")
four.summary <- read.csv("FW50_summaryFour.csv")
five.summary <- read.csv("FW50_summaryFive.csv")
six.summary <- read.csv("FW50_summarySix.csv")

#One.ideal.two <- tail(OnePLC.summary.two$estimate, n=1)
two.ideal <- tail(two.summary$estimate, n=1)
three.ideal <- tail(three.summary$estimate, n=1)
four.ideal <- tail(four.summary$estimate, n=1)
five.ideal <- tail(five.summary$estimate, n=1)
six.ideal <- tail(six.summary$estimate, n=1)

#Comparing all the threes
three.ideal.one <- tail(ThreePLC.summary$estimate, n=1)
three.ideal.two <- tail(ThreePLC.summary.two$estimate, n=1)

three.diff.one <- ((ThreePLC.summary$high.bound - three.ideal.one)/three.ideal.one)*100
three.diff.two <- ((ThreePLC.summary.two$high.bound - three.ideal.two)/three.ideal.two)*100

plot_ly(y = three.difference, x = three.summary$number.of.current.readings*.177211*21, type = "scatter", name = "Three.difference", marker = list(color = "blue"))%>%
  add_trace(y = three.diff.one, x = ThreePLC.summary$number.of.current.readings*.177211*21, type = "scatter", name = "Three.difference.First", marker = list(color = "magenta"))%>%
  add_trace(y = three.diff.two, x = ThreePLC.summary.two$number.of.current.readings*.177211*21, type = "scatter", name = "Three.difference.Second", marker = list(color = "green"))%>%
  layout(title = "Percentage Away From the Ideal Versus the Time Taken To Gather Readings",xaxis = x.axis, yaxis = y.axis)


medium.ideal <- tail(medium.summary$estimate, n=1)
slow.ideal <- tail(slow.summary$estimate, n=1)

medium.difference <- ((medium.summary$high.bound - medium.ideal)/medium.ideal)*100
slow.difference <- ((slow.summary$high.bound - slow.ideal)/slow.ideal)*100

plot_ly(y = OnePLC.difference, x = OnePLC.summary$number.of.current.readings, type = "scatter", name = "One.difference", marker = list(color = "blue"))%>%
  add_trace(y = slow.difference, x = slow.summary$number.of.current.readings*.3269*31, type = "scatter", name = "slow.difference", marker = list(color = "magenta"))%>%
  layout(title = "Percentage Away From the Ideal Versus the Time Taken To Gather Readings",xaxis = x.axis, yaxis = y.axis)

#One.difference.two <- ((OnePLC.summary.two$high.bound - One.ideal.two)/One.ideal.two)*100
two.difference <- ((two.summary$high.bound - two.ideal)/two.ideal)*100
three.difference <- ((three.summary$high.bound - three.ideal)/three.ideal)*100
four.difference <- ((four.summary$high.bound - four.ideal)/four.ideal)*100
five.difference <- ((five.summary$high.bound - five.ideal)/five.ideal)*100
six.difference <- ((six.summary$high.bound - six.ideal)/six.ideal)*100

x.axis <- list(title = "Number of Readings", showticklabels = TRUE)
y.axis <- list(title = "Percentage away from ideal FW50 value", showticklabels = TRUE)
plot_ly(y = two.difference, x = two.summary$number.of.current.readings*.12594*21, type = "scatter", name = "Two.difference", marker = list(color = "blue"))%>%
  add_trace(y = three.difference, x = three.summary$number.of.current.readings*.177211*21, type = "scatter", name = "Three.difference", marker = list(color = "magenta"))%>%
  add_trace(y = four.difference, x = four.summary$number.of.current.readings*.22739*21, type = "scatter", name = "Four.difference", marker = list(color = "green"))%>%
  add_trace(y = five.difference, x = five.summary$number.of.current.readings*.2772*21, type = "scatter", name = "Five.difference", marker = list(color = "red"))%>%
  add_trace(y = six.difference, x = six.summary$number.of.current.readings*.32675*21, type = "scatter", name = "Six.difference", marker = list(color = "brown"))%>%
  layout(title = "Percentage Away From the Ideal Versus the Time Taken To Gather Readings",xaxis = x.axis, yaxis = y.axis)

plot_ly(y = One.difference.two, x = OnePLC.summary.two$number.of.current.readings*.07671*21, type = "scatter", name = "One.difference", marker = list(color = "blue"))%>%
  add_trace(y = Two.difference.two, x = TwoPLC.summary.two$number.of.current.readings*.1269*21, type = "scatter", name = "Two.difference", marker = list(color = "magenta"))%>%
  add_trace(y = Three.difference, x = ThreePLC.summary.two$number.of.current.readings*.17688*21, type = "scatter", name = "Three.difference", marker = list(color = "green"))%>%
  layout(title = "Percentage Away From the Ideal Versus the Time Taken To Gather Readings",xaxis = x.axis, yaxis = y.axis)





a <- list(title = "Time to take readings(s)", showticklabels = TRUE)
b <- list(title = "FW50 values (radians)", showticklabesl = TRUE, exponentformat = "E")

One.plot <- plot_ly(y = OnePLC.summary$low.bound, x = OnePLC.summary$number.of.current.readings*.07671*21, type = "scatter", name = "One:Low bound", marker = list(color = "deepskyblue"))%>%
  add_trace(y = OnePLC.summary$estimate, x = OnePLC.summary$number.of.current.readings*.07671*21, type = "scatter", name = "One:Estimate", marker = list(color = "blue"))%>%
  add_trace(y = OnePLC.summary$high.bound, x = OnePLC.summary$number.of.current.readings*.07671*21, type = "scatter", name = "One:High bound", marker = list(color = "royalblue"))%>%
  layout(title = "Summary: One PLC", xaxis = a, yaxis = b)
One.plot

Two.plot <-  plot_ly(y = TwoPLC.summary$low.bound, x = TwoPLC.summary$number.of.current.readings*.1269*21, type = "scatter", name = "Two:Low Bound", marker = list(color = "deeppink"))%>%
  add_trace(y = TwoPLC.summary$estimate, x = TwoPLC.summary$number.of.current.readings*.1269*21, type = "scatter", name = "Two:Estimate", marker = list(color = "magenta"))%>%
  add_trace(y = TwoPLC.summary$high.bound, x = TwoPLC.summary$number.of.current.readings*.1269*21, type = "scatter", name = "Two:High Bound", marker = list(color = "red"))%>%
  layout(title = "Summary: Two PLC", xaxis = a, yaxis = b)
Two.plot

Three.plot <-  plot_ly(y = ThreePLC.summary$low.bound, x = ThreePLC.summary$number.of.current.readings*.17688*21, type = "scatter", name = "Three:Low Bound", marker = list(color = "green"))%>%
  add_trace(y = ThreePLC.summary$estimate, x = ThreePLC.summary$number.of.current.readings*.17688*21, type = "scatter", name = "Three: Estimate", marker = list(color = "darkgreen"))%>%
  add_trace(y = ThreePLC.summary$high.bound, x = ThreePLC.summary$number.of.current.readings*.17688*21, type = "scatter", name = "Three:High Bound", marker = list(color = "limegreen"))%>%
  layout(title = "Summary: Three PLC", xaxis = a, yaxis = b)
Three.plot

All.plots <- plot_ly(y = OnePLC.summary$low.bound, x = OnePLC.summary$number.of.current.readings*.07671*21, type = "scatter", name = "One:Low bound", marker = list(color = "deepskyblue"))%>%
  add_trace(y = OnePLC.summary$estimate, x = OnePLC.summary$number.of.current.readings*.07671*21, type = "scatter", name = "One:Estimate", marker = list(color = "blue"))%>%
  add_trace(y = OnePLC.summary$high.bound, x = OnePLC.summary$number.of.current.readings*.07671*21, type = "scatter", name = "One:High bound", marker = list(color = "royalblue"))%>%
  add_trace(y = TwoPLC.summary$low.bound, x = TwoPLC.summary$number.of.current.readings*.1269*21, type = "scatter", name = "Two:Low Bound", marker = list(color = "deeppink"))%>%
  add_trace(y = TwoPLC.summary$estimate, x = TwoPLC.summary$number.of.current.readings*.1269*21, type = "scatter", name = "Two:Estimate", marker = list(color = "magenta"))%>%
  add_trace(y = TwoPLC.summary$high.bound, x = TwoPLC.summary$number.of.current.readings*.1269*21, type = "scatter", name = "Two:High Bound", marker = list(color = "red"))%>%
  add_trace(y = ThreePLC.summary$low.bound, x = ThreePLC.summary$number.of.current.readings*.17688*21, type = "scatter", name = "Three:Low Bound", marker = list(color = "green"))%>%
  add_trace(y = ThreePLC.summary$estimate, x = ThreePLC.summary$number.of.current.readings*.17688*21, type = "scatter", name = "Three: Estimate", marker = list(color = "darkgreen"))%>%
  add_trace(y = ThreePLC.summary$high.bound, x = ThreePLC.summary$number.of.current.readings*.17688*21, type = "scatter", name = "Three:High Bound", marker = list(color = "limegreen"))%>%
  layout(title = "All three Plot summaries", xaxis = a, yaxis = b)
All.plots
