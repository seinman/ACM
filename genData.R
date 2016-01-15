# HOMEWORK FOR HRVOJE

# REMEMBER TO ADD SOMETHING THAT WILL INSTALL THE PACKAGE IF IT IS NOT ALREADY INSTALLED
library(mvtnorm)
library(ggplot2)
setwd('/home/beeb/Documents/Data_Science/Advanced Computational Methods')

#Steal Hrvoje's covariance matrix function
sigmaXY <- function(rho, sdX, sdY) { 
  covTerm <- rho * sdX * sdY 
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 2, 2, byrow = TRUE)
  return(VCmatrix) }

# The function creates three groups: two of them are difficult to separate from
# one another due to the fact they cross over, and the third appears easy to separate
# at first, until you notice the presence of outliers which will complicate the task.
crescent.data <- function(obsunicorn, obsphoenix, obsmuggle,
                          muXunicorn, muYunicorn, muXphoenix, muYphoenix, muXmuggle, muYmuggle,
                          sdXunicorn, sdYunicorn, sdXphoenix, sdYphoenix, sdXmuggle, sdYmuggle,
                          rhounicorn, rhophoenix, rhomuggle,                           
                          dataSave = TRUE, plotSave = TRUE) {
  
  # Create the data for the unicorn
  covmat.unicorn <- sigmaXY(rhounicorn, sdXunicorn, sdYunicorn)
  unicorn <- rmvnorm(obsunicorn, mean = c(muXunicorn, muYunicorn), sigma = covmat.unicorn)  
  unicorn <- cbind(unicorn, 1)
  
  # Create the data for the phoenix
  covmat.phoenix <- sigmaXY(rhophoenix, sdXphoenix, sdYphoenix)
  phoenix <- rmvnorm(obsphoenix, mean = c(muXphoenix, muYphoenix), sigma = covmat.phoenix)
  phoenix <- cbind(phoenix, 2)
  
  # Create the muggles
  is.muggle <- rbinom(obsmuggle, 1, 0.01)
  covmat.muggle <- sigmaXY(rhomuggle, sdXmuggle, sdYmuggle)
  muggle <- rmvnorm(obsmuggle, mean = c(muXmuggle, muYmuggle), sigma = covmat.muggle)
  muggle[,2] <- muggle[,2] + is.muggle * muggle[,2] * 30
  muggle <- cbind(muggle, 3)
  
  dataset <- as.data.frame(rbind(unicorn, phoenix, muggle))
  dataset$V3 <- as.factor(dataset$V3)
  graph <-ggplot(data = crescent, aes(x = V1, y = V2, colour = V3)) + geom_point() + theme_bw() +
    scale_color_manual("", values = c("violet", "skyblue", "violetred4"), labels = c('unicorn', 'phoenix', 'muggle')) +
    labs(x = 'Age', y = 'Magical ability')
  if(plotSave) {
    ggsave('dataPlot.pdf')
  }
  if(dataSave) {
    write.csv(dataset, 'dataset.csv')
  }
  return(dataset)
}

# Now we run the function to create the dataset and graph
set.seed(100)
crescent <- crescent.data(obsunicorn = 400, obsphoenix = 400, obsmuggle = 400,
                          muXunicorn = 60, muXphoenix = 60, muXmuggle = 45,
                          muYunicorn = 150, muYphoenix = 150, muYmuggle = 5,
                          sdXunicorn = 17, sdXphoenix = 17, sdXmuggle = 12,
                          sdYunicorn = 40, sdYphoenix = 40, sdYmuggle = 1.5,
                          rhounicorn = 0.8, rhophoenix = -0.8, rhomuggle = 0,
                          dataSave = TRUE, plotSave = TRUE)
