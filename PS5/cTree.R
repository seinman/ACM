f <- (x ~ y + z)
get.vars(lhs(f)) #label
get.vars(rhs(f)) #predictors


  cTree <- function(formula, data, minPoints, depth, costFnc, loop = 0) {
  library(formula.tools)
    library(assertthat)
    nam <- colnames(data)
    Y <- data[, get.vars(lhs(formula))] 
    X <- as.matrix(data[, get.vars(rhs(formula))])

    findThreshold <- function(x, y, costFnc) {
      library(dplyr)
      forsort <- data.frame(x = x, y=y)
      forsort <- arrange(forsort, x)
      x <- forsort$x
      y <- forsort$y
      
      noPoints <- length(x)
      errors <- rep(NA, noPoints-1)
      thresholds <- rep(NA, noPoints-1)
      splitLabels <- matrix(NA, ncol=2, nrow=noPoints-1)
      
      # we go sequentially over each point and cut between that point and the
      # closest neighbor
      for (idx in 1:(noPoints-1)) {
        
        # locate a potential threshold, a split between two points
        potThres <- mean(x[idx:(idx+1)])
        
        # check the classification error, when both sides, 
        # are classified with mean label
        predictedClasses <- rep(NA, noPoints)
        
        # Find out the most common class on each side of the threshold
        qLeft <- as.data.frame(table(y[x < potThres]))
        qRight <- as.data.frame(table(y[x >= potThres]))
        
        # There are reasons for this bit of code best known only to R. Srsly, screw R.
        qLeft$Var2 <- as.character(qLeft$Var1)
        qLeft$Var1 <- as.numeric(qLeft$Var2)
        qRight$Var2 <- as.character(qRight$Var1)
        qRight$Var1 <- as.numeric(qRight$Var2)
        
        bestChoiceLeft <- qLeft$Var1[qLeft$Freq == max(qLeft$Freq)]
        if(length(bestChoiceLeft)>1) {
          bestChoiceLeft <- sample(bestChoiceLeft, 1)
        }
        
        bestChoiceRight <- qRight$Var1[qRight$Freq == max(qRight$Freq)]
        if(length(bestChoiceRight)>1) {
          bestChoiceRight <- sample(bestChoiceRight, 1)
        }
        
        # And use these to predict classes for Y
        predictedClasses[x < potThres] <- bestChoiceLeft
        predictedClasses[x > potThres] <- bestChoiceRight[1]
        
        # error of this split
        if(costFnc == 'ME') {
          misError <- mean(predictedClasses != y)
        }
        

        allprobleft <- qLeft$Freq / (sum(qLeft$Freq)) 
        allprobright <- qRight$Freq / (sum(qRight$Freq))
        if(costFnc == 'Gini') {   
          allnotprobleft <- 1 - allprobleft
          allnotprobright <- 1 - allprobright
          ginileft <- allprobleft * allnotprobleft
          giniright <- allprobright * allnotprobright
          misError <- sum(ginileft) + sum(giniright)
        } else if(costFnc == 'Entropy') {
          entropyleft <- allprobleft * log(allprobleft)
          entropyright <- allprobright * log(allprobright)
          misError <- -(sum(entropyleft) + sum(entropyright))
        }
        
        
        # recording the accuracy, thresholds and labels of 
        # the splitted interval
        errors[idx] <- misError
        thresholds[idx] <- potThres
        splitLabels[idx,] <- c(predictedClasses[x < potThres][1],
                               predictedClasses[x > potThres][1])
      }
      # print(cbind(errors, thresholds, splitLabels))
      
      # next we find the minimum and the best threshold
      minError <- min(errors)
      bestThreshold <- thresholds[which(errors==minError)]
      # if more than 1 threshold has the same accuracy we choose one randomly
      if(length(bestThreshold) > 1) {
        bestThreshold <- sample(bestThreshold, 1)
      }
      #bestThreshold <- bestThreshold[1]
      
      # what are the final labels of the best split?
      labels <- splitLabels[which(thresholds==bestThreshold),]
      # print(cbind(minError, bestThreshold, labels))
      
      return(list(thres = bestThreshold, 
                  err = minError, 
                  labels = labels))
    }
    
    
    # This will end the tree if there are too few points in it
    #d <- as.data.frame(table(Y))    
    #response <- d$Y[d$Freq == max(d$Freq)]    

 #   if(max(c(nrow(X), 1)) < (minPoints)) {
  #    cat('gothere')
     # response <- sample(response, 1)
  #    probability <- max(d$Freq)/length(Y)
   #   cat(probability)
    #  return(list(prob = probability, predLabels = response))
    #}
    
    l <- loop
    cat('l')
    cat(l)
    # This is a slightly messy way of preventing the tree from growing too many branches
    if(l == depth) {
      cat('too deep')
      d <- as.data.frame(table(Y))    
      response <- d$Y[d$Freq == max(d$Freq)]    
      if(length(response) > 1) {
        response <- sample(response, 1)
      }
      probability <- max(d$Freq)/length(Y)
      return(list(prob = probability, predLabels = response)) 
    }
    
    # This will end the tree if all the responses in the branch are the same
  #  if(max(d$Freq) == length(Y)) {
  #    return(list(prob = 1, predLabels = response))
   # }
    
    else {
      
      # setting up the initial boundaries
      boundaries <- c(min(X),max(X))
      nvar <- ncol(X)
      errorcompare <- rep(NA, nvar)
      thresholdcompare <- rep(NA, nvar)
      labelcompare <- matrix(NA, nrow = nvar, ncol = length(unique(Y)))    
      
      for(var in 1:nvar) {
        x <- X[,var]
        intervals <- cut(x, boundaries, include.lowest = TRUE)
        
        # find the local splitting point
        results <- findThreshold(x, Y, costFnc = costFnc)
        splitLabels <- results$labels
        
        # add the threshold to our list of boundaries
        boundariesHH <- c(boundaries, results$thres)
        boundariesHH <- sort(boundariesHH)
        signsHH <- results$labels
        assign(paste0('signsHH', var), signsHH)
        
        # now we compute predictions with new boundaries based on the 
        # potential split
        predictedClasses <- cut(x, boundariesHH, include.lowest = TRUE)
        #levels(predictedClasses) <- signsHH 
        
        errorcompare[var] <- mean(predictedClasses != Y)
        thresholdcompare[var] <- results$thres    
      }

      bestError <- min(errorcompare)
      whichVar <- which(errorcompare == bestError)
      whichVar <- sample(whichVar, 1)

      bestThreshold.total <- thresholdcompare[whichVar]
      
      # add the new threshold to our list of boundaries
      boundaries.end <- c(boundaries, bestThreshold.total)
      boundaries.end <- sort(boundaries.end)
      
      # add the signs of the new threshold which indicates what is the label of the newly splitted interval
      signs <- labelcompare[whichVar,]
      # get the final predicted classes
      predictedClasses <- cut(X[,whichVar], boundaries.end, include.lowest = TRUE)
      
      whichsplit <- predictedClasses == levels(predictedClasses)[1]
      
      part1 <- data[whichsplit,]
      #names(part1) <- nam
      cat('ws')
      cat(sum(whichsplit))
      part2 <- data[!whichsplit,]
      cat('!ws')
      cat(sum(!whichsplit))
   #   levels(predictedClasses) <- signs
      
      # now we evaluate the final accuracy, after K iterations
    #  misError <- 1 - (mean(as.numeric(predictedClasses) != Y, na.rm = TRUE))
      
      if(sum(whichsplit) > minPoints & sum(!whichsplit) > minPoints) {
      
      return(list(boundaries = boundaries.end,
                  whichVar = whichVar,
                  branch1=cTree(formula = formula, data = part1, minPoints = minPoints, 
                                depth = depth, costFnc = costFnc, loop = (l + 1)), 
                  branch2=cTree(formula = formula, data = part2, minPoints = minPoints,
                                depth = depth, costFnc = costFnc, loop = (l + 1))))
      }
      else {
        d <- as.data.frame(table(Y))    
        response <- d$Y[d$Freq == max(d$Freq)]    
             cat('gothere')       
        if(length(response)>1) {          

        response <- sample(response, 1)
        }
        probability <- max(d$Freq)/length(Y)
        cat(probability)
        return(list(prob = probability, predLabels = response))
      }
        
        
    }
}
  
  

