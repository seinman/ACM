filepath <- '/home/beeb/Documents/Data_Science/Advanced Computational Methods'
# NO CODE HAS EVER TAKEN LONGER TO RUN THAN THIS ONE DOES
# WHY IS THIS TAKING SO LONG
setwd(filepath)
source('Final/kNN.R')
# THE WORLD TURNS
library(class)
mnist <- read.csv('MNIST_training.csv')
mnist.test <- read.csv('MNIST_test.csv')
# CONTINENTS DRIFT
training.obvs <- sample(nrow(mnist), nrow(mnist)*4/5)
train <- mnist[training.obvs,]
test <- mnist[setdiff(1:nrow(mnist), training.obvs),]

trial <- kNN(test[,2:ncol(test)], train[,1], memory=train[,2:ncol(train)], 
             type = 'predict', K = c(1,3,5,7,9,15,25,51))
f <- rep(NA,8)
for(k in 1:8) {
  pred <- trial[[k]]$predictedClasses
  f[k] <- length(which(pred!=test[,1]))
}

# EMPIRES RISE AND FALL
# WHOLE SPECIES EVOLVE, FLOURISH, 
# AND BECOME EXTINCT

# wait did I just accidentally leave a haiku comment?
#............I comment my code in haikus
# brilliant

# THE CODE RUNS

perc.fail <- f/nrow(test)
plot(perc.fail)

# AND RUNS
best.k <- which(perc.fail == min(perc.fail))

# The time has finally come.
pred <- kNN(mnist.test, mnist[,1], memory=mnist[,2:ncol(mnist)], 
    type = 'predict', K = best.k)
predictions <- pred[[1]]$predictedClasses
write.table(predictions, 'MNIST_predictions.csv')

