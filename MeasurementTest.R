setwd("C:\\Users\\smellin\\Desktop\\FinalProject")

#library(hexbin)
library(ggplot2)
library(plotly)
library(reshape2)
library(devtools)

R <- NULL
R.0 <- NULL
s2.s1 <- NULL

#Calculate xprime and add it as another column
CalcXprime <- function(df){
  if(df$X[1] > df$X[2]){
    R <- sqrt( (df$Y - tail(df$Y))^2 + (df$X - tail(df$X))^2 )
    R.0 <- - sqrt((-0.0016 - tail(df$Y))^2 + (0.00307 - tail(df$X))^2)
  }else{
    R <- sqrt((df$Y - df$Y[1])^2 + (df$X - df$X[1])^2)
    R.0 <- - sqrt((-0.0016 - df$Y[1])^2 + (0.00307 - df$X[1])^2)
  }
  s2.s1 <- (R - R.0) - (df$FIBX + 0.00353)
  atan(s2.s1/0.398731)
}

# #Data with medium and slow - 31 points, 50 readings per point
# data.fifty <- read.csv("11_8 Speed Test.csv", header = TRUE)
# data.fifty$Xprime <- CalcXprime(data.fifty)

# p <- plot_ly(y = data.fifty$Medium, x = ~data.fifty$Xprime, type = "box", name = "Medium", pointpos = -2, boxmean = "sd") %>%
#   add_trace(y = data.fifty$Slow, x = ~data.fifty$Xprime, type = "box", name = "Slow", pointpos = 2, boxmean = "sd")%>%
#   layout(boxmode = "group")
# p

#First set of data for the three settings
OnePLC <- read.csv("CurrentReadings1PLC100.csv", header = TRUE)
TwoPLC <- read.csv("CurrentReadings2PLC100.csv", header = TRUE)
ThreePLC <- read.csv("CurrentReadings3PLC100.csv", header = TRUE)
#Second time performing the scans
OnePLC.second <- read.csv("CurrentReadings1PLC100num2.csv", header = TRUE)
TwoPLC.second <- read.csv("CurrentReadings2PLC100num2.csv", header = TRUE)
ThreePLC.second <- read.csv("CurrentReadings3PLC100num2.csv", header = TRUE)


sixPLC <- read.csv("CurrentReadings6PLCs_11_23.csv", header = TRUE)
threePLC <- read.csv("CurrentReadings3PLC100.csv", header = TRUE)
plot(sixPLC$X,sixPLC$Current)
grid()

plot(threePLC$X,threePLC$Current)
grid()
xVals <- unique(OnePLC$X)

df.Complete <- OnePLC
colnames(df.Complete) <- c("FIBX", "FIBY", "X", "Y", "One")
df.Complete$Two <- TwoPLC$Current
df.Complete$Three <- ThreePLC$Current

write.csv(df.Complete, file = "CompleteData.csv", row.names = FALSE)

df.test <- df.Complete
df.test$FIBX <- NULL
df.test$FIBY <- NULL
df.test$Xprime <- NULL
df.test$Y <- NULL

test.One <- df.test
test.One$Two <- NULL
test.One$Three <- NULL
dfl <- melt(test.One, id.vars = c("One"))
dfw <- dcast(dfl, One ~ variable)

one.avg <- NULL
two.avg <- NULL
three.avg <- NULL
dev.Oneplc <- NULL
for(i in 1:length(xVals)){
  for(j in 1:100){
    one.avg[i] <- mean(df.test$One[(i-1)*100 + j])
    two.avg[i] <- mean(df.test$Two[(i-1)*100 + j])
    three.avg[i] <- mean(df.test$Three[(i-1)*100 + j])
    j=j+1
  }
  i= i + 1
}

avgs.data <- as.data.frame(unique(df.Complete$X), row.names = NULL)
avgs.data$Y <- as.data.frame(unique(df.Complete$X), row.names = NULL) 
avgs.data$One <- as.data.frame(one.avg, row.names =NULL)
avgs.data$Two <- as.data.frame(two.avg, row.names =NULL)
avgs.data$Three <- as.data.frame(three.avg, row.names =NULL)

write.csv(avgs.data, file = "AveragedData.csv", row.names = FALSE)

df.Complete$xPrime <- CalcXprime(df.Complete)

pl <- plot_ly(y = df.Complete$Current, x = ~df.Complete$xPrime, type = "scatter", name = "One PLC (medium)", boxmean = "sd")
  #add_trace(y = df.Complete$Two, x = ~df.Complete$X, type = "box", name = "Two PLCs", boxmean = "sd")%>%
  #add_trace(y = df.Complete$Three, x = ~df.Complete$X, type = "box", name = "Three PLCs", boxmean = "sd")%>%
  #layout(boxmode = "group")
pl


xVals <- unique(OnePLC$X)

df.Complete.Second <- OnePLC.second
colnames(df.Complete.Second) <- c("FIBX", "FIBY", "X", "Y", "One")
df.Complete.Second$Two <- TwoPLC.second$Current
df.Complete.Second$Three <- ThreePLC.second$Current

pl.second <- plot_ly(y = df.Complete.Second$One, x = ~df.Complete.Second$X, type = "box", name = "One PLC (medium)", boxmean = "sd") %>%
  add_trace(y = df.Complete.Second$Two, x = ~df.Complete.Second$X, type = "box", name = "Two PLCs", boxmean = "sd")%>%
  add_trace(y = df.Complete.Second$Three, x = ~df.Complete.Second$X, type = "box", name = "Three PLCs", boxmean = "sd")%>%
  layout(boxmode = "group")
pl.second

summary(df.Complete$One)


#Making the first plot of just slow and medium
# Med <- NULL
# Slow <- NULL
# for (i in 1:length(xVals)){
#   Med[[i]] <- mylist[[i]]$Medium
#   slow[[i]] <- mylist[[i]]$Slow
# }
# df.medium <- as.data.frame(medium)
# df.slow <- as.data.frame(slow)
# 
# boxplot(df.medium)
# boxplot(df.slow)

Dev.Medium <- NULL
Dev.Slow <- NULL
# for (i in 1: length(df.medium)){
#   Dev.Medium[i] = sd(df.medium[[i]])
#   Dev.Slow[i] = sd(df.slow[[i]])
# }
# summary(Dev.Medium)
# summary(Dev.Slow)

# 
# #TEST
# a <- plot_ly(ggplot2::diamonds, x = ~cut, y = ~price, color = ~clarity, type = "box")%>%
#   layout(boxmode = "group")
# print("to see the data input a into command line")