# Some of Hrvoje's code to set things up

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

# creating a function for all of this
loanData <- function(noApproved, noDenied, noUndecided, 
                     muApproved, muDenied, muUndecided,
                     sdApproved, sdDenied, sdUndecided,
                     rhoApproved, rhoDenied, rhoUndecided,
                     seed=1111) {
  
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho = rhoUndecided, sdX = sdUndecided[1], sdY = sdUndecided[2])
  
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed + 2)
  
  loanDf <- as.data.frame(rbind(approved,denied, undecided))
  deny <- c(rep("Approved", noApproved), 
            rep("Denied", noDenied), 
            rep("Undecided", noUndecided))
  target = c(rep(0, noApproved), rep(1, noDenied), rep(2, noUndecided))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

noDenied <- 50
noApproved <- 50
noUndecided <- 50

loanDf <- loanData(noApproved=noApproved, noDenied=noDenied, noUndecided = noUndecided,
                   muApproved = c(4, 150), muDenied = c(10, 100), muUndecided = c(5, 125),
                   sdApproved = c(1,20), sdDenied = c(2,30), sdUndecided = c(1.5, 25),
                   rhoApproved = -0.1, rhoDenied = 0.6, rhoUndecided = 0.3,
                   seed = 1221)

# illustrating the data, note that with ggplot we need to additionally 
# specify font family
ggplot(data = loanDf, 
       aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
  geom_point() +
  xlab("solvency") +
  ylab("PIratio") +
  theme_bw() +
  theme(text=element_text(family="Arial"))

# Add the coding for the targets in the mysterious 'particular way' Hrvoje requests
loanDf <- cbind(loanDf, 
                targetapp = c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided)),
                targetden = c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided)),
                targetund = c(rep(0, noApproved), rep(0, noDenied), rep(1, noUndecided))
)

# Extract the coefficients
X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), 
                     loanDf[,c("PIratio", "solvency")]))
Y <- as.matrix(loanDf[,5:7])
weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y

# Create the boundary lines
create.boundary <- function(coeffs1, coeffs2, between = 'Boundary') {
  slope <- (coeffs2['solvency'] - coeffs1['solvency']) / (coeffs1['PIratio'] - coeffs2['PIratio'])
  intercept <- (coeffs2['ind'] - coeffs1['ind'])  / (coeffs1['PIratio'] - coeffs2['PIratio'])
  x <- seq(min(loanDf["solvency"]), max(loanDf["solvency"]), 
           length.out = noApproved+noDenied+noUndecided)
  y <- intercept + slope * x
  line <- data.frame(solvency = x, PIratio =y,
                     deny=rep(between, length(x)))
  coeffs <- c(intercept, slope)
  names(coeffs) <- c('intercept', 'slope')
  return(list(line=line, coeffs = coeffs))
}

lineAppUnd <- create.boundary(weightsOptim[,'targetapp'], weightsOptim[,'targetund'], 'AppUnd')
lineAppDen <- create.boundary(weightsOptim[,'targetapp'], weightsOptim[,'targetden'], 'AppDen')
lineDenUnd <- create.boundary(weightsOptim[,'targetund'], weightsOptim[,'targetden'], 'UndDen')

# Now we turn lines into polygons
# Is there an easy way of doing this?
# We're just gonna have to do it manually, by calculating the corners of each of the
# regions and then telling R to draw polygons in between them.
intersectionApp <- c(0,0)
names(intersectionApp) <- c('solvency', 'PIratio')
intersectionApp['solvency'] <- (lineAppDen$coeffs['intercept'] - lineAppUnd$coeffs['intercept']) / 
  (lineAppUnd$coeffs['slope'] - lineAppDen$coeffs['slope'])
intersectionApp['PIratio'] <- lineAppDen$coeffs['intercept'] + intersectionApp['solvency'] * lineAppDen$coeffs['slope']

cornerApp1 <- c(200,0)
cornerApp1[2] <- lineAppDen$coeffs['intercept'] + lineAppDen$coeffs['slope'] * cornerApp1[1]
cornerApp2 <- c(0,2)
cornerApp2[1] <- (cornerApp2[2] - lineAppUnd$coeffs['intercept'])/lineAppUnd$coeffs['slope']
cornerApp3 <- c(200, 2)

intersectionDen <- c(0,0)
names(intersectionDen) <- c('solvency', 'PIratio')
intersectionDen['solvency'] <- (lineAppDen$coeffs['intercept']- lineDenUnd$coeffs['intercept']) / 
  (lineDenUnd$coeffs['slope'] - lineAppDen$coeffs['slope'])
intersectionDen['PIratio'] <- lineAppDen$coeffs['intercept'] + intersectionDen['solvency'] * lineAppDen$coeffs['slope']
cornersAppx <- c(cornerApp1[1], intersectionApp[1], cornerApp2[1], cornerApp3[1])
cornersAppy <- c(cornerApp1[2], intersectionApp[2], cornerApp2[2], cornerApp3[2])
polyApp <- data.frame(solvency = cornersAppx, PIratio = cornersAppy, deny = 'Approval')

cornerDen1 <- cornerApp1
cornerDen2 <-  c(30,0)
cornerDen2[2] <- lineDenUnd$coeffs['intercept'] + lineDenUnd$coeffs['slope'] * cornerDen2[1]
cornerDen3 <- c(30, 18)
cornerDen4 <- c(200, 18)
cornersDenx <- c(cornerDen1[1], intersectionDen[1], cornerDen2[1], cornerDen3[1], cornerDen4[1])
cornersDeny <- c(cornerDen1[2], intersectionDen[2], cornerDen2[2], cornerDen3[2], cornerDen4[2])
polyDen <- data.frame(solvency = cornersDenx, PIratio = cornersDeny, deny = 'Denial')

intersectionUnd <- intersectionDen
cornerUnd1 <- cornerApp2
cornerUnd2 <- cornerDen2
cornerUnd3 <- c(30,2)
cornersUndx <- c(cornerUnd1[1], intersectionUnd[1], cornerUnd2[1], cornerUnd3[1])
cornersUndy <- c(cornerUnd1[2], intersectionUnd[2], cornerUnd2[2], cornerUnd3[2])
polyUnd <- data.frame(solvency = cornersUndx, PIratio = cornersUndy, deny = 'Indecision')

polygon <- rbind(polyUnd, polyApp, polyDen)
polygon$deny <- as.factor(polygon$deny)

# plotting
ggplot(data = loanDf, aes(x = solvency, y = PIratio, 
                          fill=deny)) + 
  xlab("solvency") +
  ylab("PI ratio") +
  theme_bw(base_size = 14, base_family = "Helvetica") + 
  ylim(2,18) +
  geom_polygon(data=polygon, aes(group = deny), guide = 'none') + 
  geom_point(aes(colour=deny)) +
  scale_fill_manual("", values = c("lightpink", "red", "aliceblue", 'blue', 'plum', 'magenta3'), 
                    labels = c('', 'approval', '', 'denial', '', 'undecided'), guide = 'none') +
  scale_colour_manual("", values = c('red', 'blue', 'magenta3'), 
                      labels = c('Approved', 'Denied', 'Undecided')) 

ggsave('discFunction3C.pdf')

# compute predictions
predictions <- X %*% weightsOptim
head(predictions)
pred.category <- apply(predictions, 1, which.max)

# Put it all together into one beautiful dataframe
loanData3C <- cbind(loanDf[,1:4], pred.category)
loanData3C$target <- loanData3C$target + 1
# And save
write.csv(loanData3C, 'predictions.csv')
