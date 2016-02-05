filepath <- '/home/beeb/Documents/Data_Science/Advanced Computational Methods'
setwd(filepath)
source('Final/PS3/kNN.R')
source('genData.R')
library(reshape2)
library(ggplot2)

data <- genSpirals()

predict <- kNN(data[,1:2], data[,3])
predicted.values <- predict[[1]]$predictedClasses
probs <- predict[[1]]$prob

output <- data.frame(data, predLabels = predicted.values, prob = probs)
write.table(output, 'predictions.csv')

zp<- predicted.values

xp <- seq(min(output$x1), max(output$x1), length = 50); np <- length(xp)
yp <- seq(min(output$x2), max(output$x2), length = 50)

#I spent 3 hours trying to make this work. I give in.
d <- transform(melt(matrix(zp, np)), xp=xp, yp=yp)
ggplot(d, aes(xp, yp, z=Var2)) + 
  geom_contour() + 
  geom_point(aes(x1, x2, colour=y, z=NULL), data=output)
