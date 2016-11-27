library(ggplot2)

## the setup function, with filling in true positive rates, etc.
shuffleBayesRates <- function(TP, FP, TN, FN, n=1000) {
  #
  TPv <- rep('TP', ceiling(TP*n))
  FPv <- rep('FP', ceiling(FP*n))
  TNv <- rep('TN', ceiling(TN*n))
  FNv <- rep('FN', ceiling(FN*n))
  # put them together
  all <- c(TPv,FPv,TNv,FNv) # this is where we'd subset for the different stats (speficity, etc.)
  #shuffle them
  shuffled <- sample(all,n, replace = F)
  #print(shuffled)
    # Bayes Theorum
  x = (length(TPv) + length(FNv)) / n # prior prob
  y = length(TPv) / (length(TPv) + length(FNv)) # prob of new evidence if true
  z = length(FPv) / (length(FPv) + length(TNv)) # prob of new evidence if false
  bayesProb = (x*y) / ( (x*y) + (z*(1-x)) )
  print("$$$$$$$$$$$$$$$$$")
  print(paste("BAYES PROB:", bayesProb))
  print("$$$$$$$$$$$$$$$$$")
  #
  shuffled
}

## the setup function with filling in probabilities
shuffleBayesProbs <- function(x, y, z, n=1000) {
  # Bayes Theorum
  # x = prior prob
  # y = prob of new evidence if true
  # z = prob of new evidence if false
  bayesProb = (x*y) / ( (x*y) + (z*(1-x)) )
  print("$$$$$$$$$$$$$$$$$")
  print(paste("BAYES PROB:", bayesProb))
  print("$$$$$$$$$$$$$$$$$")
  ########
  # make vector of observations
  ########
  true <- round(x * n, 0)
  false <- n - true
  #
  TP <- round(y * true, 0)
  FP <- round(z * false, 0)
  TN <- false - FP
  FN <- true - TP
  #
  TPv <- rep('TP', TP)
  FPv <- rep('FP', FP)
  TNv <- rep('TN', TN)
  FNv <- rep('FN', FN)
  # put them together
  all <- c(TPv,FPv,TNv,FNv) # this is where we'd subset for the different stats (speficity, etc.)
  #shuffle them
  shuffled <- sample(all,n, replace = F)
  #
  shuffled
}

# the plotting function
plotBayes <- function(shuffled, color = "test") {
  n <- length(shuffled)
  #assemble data frame
  nx <- ceiling(sqrt(n) * 40 /sqrt(1000)) # length of x-axis (setup so that it's 40 when n = 1000)
  ny <- ceiling(n / nx) # length of y-axis
  # fill in coordinate vectors
  rows <- rep(seq(1:nx), ny)
  cols <- numeric()
  for (i in seq(1:ny)) {
    cols <- c(cols, rep(i,nx))
  }
  # create data frame, add NA for blank spaces
  data <- data.frame(result = c(rep(NA, length(rows) - length(shuffled)), shuffled), 
                     x = rows, y = cols)
  # add truth column
  data$truth = ifelse(data$result == "FN" | data$result == "TP", T, F)
  # add test column
  data$test = ifelse(data$result == "FP" | data$result == "TP", "Positive", "Negative")
  #plot
  if (color == "test") {
    ggplot(data, aes(x=x, y=y, colour=test)) + geom_point(aes(shape = result, size=2))
  } else if (color == "truth") {
    ggplot(data, aes(x=x, y=y, colour=truth)) + geom_point(aes(shape = result, size=2))
  } else {
    print("$$$$$$$ INVALID COLOR ARGUMENT $$$$ using 'test' $$$$$$$$$$")
    ggplot(data, aes(x=x, y=y, colour=test)) + geom_point(aes(shape = result, size=2))
  }

}


# the function for giving you a stat and then plotting
plotStats <- function(shuffled, stat = "all") {
  if (stat == "all") {
    plotBayes(shuffled)
  } else if (stat == "precision") {
    print("PRECISION:")
    print(length(shuffled[shuffled=="TP"]) / length(shuffled[shuffled=="TP" | shuffled == "FP"]))
    return(plotBayes(shuffled[shuffled=="TP" | shuffled == "FP"]))
  } else if (stat == "recall" | stat == "sensitivity") {
    print("RECALL/SENSITIVITY:")
    print(length(shuffled[shuffled=="TP"]) / length(shuffled[shuffled=="TP" | shuffled == "FN"]))
    return(plotBayes(shuffled[shuffled=="TP" | shuffled == "FN"]))
  } else if (stat == "specificity") {
    print("SPECIFICITY:")
    print(length(shuffled[shuffled=="TN"]) / length(shuffled[shuffled=="TN" | shuffled == "FN"]))
    return(plotBayes(shuffled[shuffled=="TN" | shuffled == "FN"]))
  } else {
    print("$$$$$$$$$ INVALID STAT ARGUMENT $$$$ using 'all' $$$$$$$$$$$$")
    return(plotBayes(shuffled))
  }
}


#### EXAMPLE
# Breast Cancer, Silver p. 246

# shuffled = shuffleBayesRates(TP = 0.011,
#              FP = 0.099,
#              TN = 0.887,
#              FN = 0.003,
#              n = 1523)

shuffled = shuffleBayesProbs(x = 0.014,
                             y = 0.7857143,
                             z = 0.1004057,
                             n = 1523)


plotStats(shuffled)
plotStats(shuffled, "specificity")
plotStats(shuffled, "recall")
plotStats(shuffled, "precision")
