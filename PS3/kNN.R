
kNN <- function(X, y, memory = NULL, 
                K = 1, p = 2, type="train") {
  library(dplyr)
  # test the inputs
  library(assertthat)
  not_empty(X); not_empty(y); 
  if (type == "train") {
    assert_that(nrow(X) == length(y))
  }
  is.string(type); assert_that(type %in% c("train", "predict"))
  is.vector(K); 
  assert_that(p %in% c(1, 2, Inf))
  if (type == "predict") {
    assert_that(not_empty(memory) & 
                  ncol(memory) == ncol(X) & 
                  nrow(memory) == length(y))
  }
  
  # Compute the distance between each point and all others 
  noObs <- nrow(X)
  cat('noObs')
  cat(noObs)
  # if we are making predictions on the test set based on the memory, 
  # we compute distances between each test observation and observations
  # in our memory
  if (type == "train") {
    distMatrix <- matrix(NA, noObs, noObs)
    for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      
      probe <- as.numeric(X[obs,])
      cat('lenprobe')
      cat(length(probe))
      probeExpanded <- matrix(probe, nrow = noObs, ncol = ncol(X), 
                              byrow = TRUE)
      
      # computing distances between the probe and exemplars in the
      # training X
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(X - 
                                             probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(X - probeExpanded), 1, max)
      }  
    }
  } else if (type == "predict") {
    noMemory <- nrow(memory)
    distMatrix <- matrix(NA, noObs, noMemory)
    for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      probe <- as.numeric(X[obs,])
      probeExpanded <- matrix(probe, nrow = noMemory, ncol = ncol(X), 
                              byrow = TRUE)
      
      # computing distances between the probe and exemplars in the memory
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(memory - 
                                             probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(memory - probeExpanded), 1, max)
      }  
    }
  }
  
  # Sort the distances in increasing numerical order and pick the first 
  # k elements

  neighbors <- apply(distMatrix, 1, order) %>% t
  fullanswers <- list()
  z <- 1
  for(k in K) {  
    # the most frequent class in the k nearest neighbors
    predictedClasses <- rep(NA, noObs)
    prob <- rep(NA, noObs)
    for (obs in 1:noObs) {
      labs <- data.frame(labs = y[neighbors[obs, 1:k]])
      labs <- group_by(labs, labs) %>% mutate(count = n())
      prob[obs] <- max(labs$count) / k
      best <- labs$labs[labs$count==max(labs$count)][1]
      predictedClasses[obs] <- best
    }
    
    # examine the performance, available only if training
    if (type == "train") {
      errorCount <- table(predictedClasses, y)
      accuracy <- mean(predictedClasses == y)
    } else if (type == "predict") {
      errorCount <- NA
      accuracy <- NA 
    }
    answers <- list(predictedClasses = predictedClasses, 
                    prob = prob,
                    accuracy = accuracy,
                    errorCount = errorCount)
    assign(paste0('answers', k), answers)
    fullanswers[[z]] <- answers
    z <- z + 1
  }
  # return the results
  names(fullanswers) <- K
  return(fullanswers)
}


