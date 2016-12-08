modelUnif = function(data){
  n = length(data) 
  deltaB = (max(data)*10-max(data))/1000
  bVals = seq(max(data)-.5, max(data)+.5, deltaB) 
  probabilityEmperical = -n*sum(log(bVals))+log(deltaB);
  return(probabilityEmperical)
}
modelExp = function(data){
  n = length(data)
  deltaT = (max(data)-min(data))/1000 
  tVals = seq(min(data), max(data), deltaT) 
  X = sum(data)
  LOGprobabiltiyEmperical = sum(log(deltaT)+n*log(1/tVals)-X/tVals)
  return(LOGprobabiltiyEmperical)
}

modelPareto = function(data){
  step = (max(data)-min(data))/1000; 
  a=seq(min(data), max(data), step); 
  b = min(data); 
  l = length(data);
  X = sum(log(data)); 
  LOGEmpericalProbability = sum(l*log(a*b^a) - (1+a)*X + log(step));
  return(LOGEmpericalProbability)
}

modelLogristic = function(data){
  library(pracma)
  s = sd(data)
  u = seq(mean(data)-.5, mean(data)+.5, .01)
  logEmpProb = 0
  for (j in 1:length(u)){
    for (d in 1:length(data)){
      logEmpProb = logEmpProb + log(1/(4*s)) + 2*log(sech((data[d]-u[j])/s))
    }
  }
  return(logEmpProb)
}

ModelComparison = function(data){
  Pareto = modelPareto(data)
  Expo = modelExp(data)
  Uniform = modelUnif(data)
  Logistic = modelLogristic(data)

  if (Pareto!=FALSE && Pareto>Expo && Pareto>Uniform && Pareto > Logistic){
    print("the model that fits the data is Pareto ")
    return(1)
  }
  if (Expo > Uniform && Expo > Logistic && Expo > Pareto){
      print("the model that fits the data is Exponential")
      return(2)
  }
  if (Uniform > Expo && Uniform > Logistic && Uniform > Pareto){
      print ("the model that fits the data is Uniform")
      return(3)
  }
  if (Logistic > Uniform && Logistic > Expo && Logistic > Pareto){
    print("the model that fits the data is Logistic")
    return(4)
  }

} 



OurProbabilityDistribution = function(data){
  library(pracma)
  Distribution = ModelComparison(data)
  if (Distribution == 1){
    # if our data is a Pareto Model
    step = (max(data)-min(data))/length(data); # this will be how big a step we take to sample a
    a=seq(min(data), max(data), step); # our possible a's, this will break down if a>10 so you will have to
    # manually check your data. 
    b = min(data); # our minimum X value - this may be something to check
    # to see how b effects our distribution if I have time
    l = length(data); # this is how many data points there are
    likely = c()
    for ( i in 1:length(a)){
      likely = c(likely,  sum(l*log(a[i]*b^a[i]) - (1+a[i])*sum(log(data))));
    }
    like = exp(likely - max(likely))
    normalization = sum(like)
    probability = like / normalization
    plot(a, probability)
    indicy = (which(like == max(like), arr.ind = TRUE))
    return(c('and has a rate of value', a[indicy], "with probability", max(probability)))
  
  }
  
  if (Distribution == 2){
    # we have an exponential Distribution
    deltaT = (max(data)-min(data))/length(data) # this is our step size
    taulist = seq(min(data), max(data), deltaT)
    logl = numeric();
    # using algebra I split our log(likelyhood) into 2 parts, seperating the 
    # exponential portion from the fraction. 
    for (tau in taulist){
      l = -length(data)*log(tau)  # this was our log(1/tau) case for the product of 
      # likelyhoods given a single rate and the set of data
      k = -sum(data)/tau          # this is our exponintail for taken as log(exp(data/rate))
      logl = c(logl, c(l+k));     # the result of Log(A*B) = log(A + B) so we update our list of likelyhoods
    }
    like = exp(logl - max(logl))
    normalization = sum(like)
    probability = like / normalization
    plot(taulist, probability)
    indicy = (which(like == max(like), arr.ind = TRUE))
    return(c('and has a rate of value', taulist[indicy], "with probability", max(probability)))
  }
  if (Distribution == 3){
    # we have uniform distribution
    # and we can find our a and b values p=1/(a+b)
    #
    # we know from our data set that b is the max of the data
    # and a is the min of the data
    
    return(c('and has a probability distribution', "1/", max(data), "-", min(data), "with probability distribution", 1/(max(data)-min(data))))
  }
  if (Distribution ==4){
    # we have a logistic distribution
    s = seq(sd(data)-.1, sd(data)+.1, .01)
    u = seq(mean(data)-.5, mean(data)+.5, .001)
    logEmpProb = array(data = 0, dim = c(length(s), length(u)))
    for (i in 1:length(s)){
      for (j in 1:length(u)){
        logEmpProb[i,u] = log(1/(4*s[i])) + sum(2*log(sech((data-u[j])/s[i])))
      }
    }
    return(which(logEmpProb = max(logEmpProb)))
  }
}


