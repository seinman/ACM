library(shiny)
library(ggplot2)
library(mvtnorm)
#################################################################
# DEFINE NECESSARY FUNCTIONS
##################################################################

sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

# creating a function for all of this
loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                     sdDenied, rhoApproved, rhoDenied, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
  target = c(rep(0, noApproved), rep(1, noDenied))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}


create.line <- function(model) {
  weights <- coef(model)[c("solvency", "PIratio")] 
  bias <- coef(model)[1]
  intercept <- (-bias + 0.5)/weights["PIratio"] 
  slope <- -(weights["solvency"]/weights["PIratio"])
  return(data.frame(beta1 = intercept, beta0 = slope))
  }


##########################################################
# BEGIN SHINING
##########################################################

shinyServer(function(input, output) {

  
  output$plot <- renderPlot({
    loanDf <- loanData(noApproved = 50, noDenied = 50,
                         muApproved = append(input$muApprovedx, input$muApprovedy), 
                         muDenied = append(input$muDeniedx, input$muDeniedy),
                         sdApproved = append(input$sdApprovedx, input$sdApprovedy), 
                         sdDenied = append(input$sdDeniedx, input$sdDeniedy),
                         rhoApproved = input$rhoapproved, rhoDenied = input$rhodenied, 
             seed=1111)
    datafit <- lm(target ~ solvency + PIratio, data = loanDf)
    line <- create.line(datafit)

    ggplot(data = loanDf, 
           aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
      geom_abline(data = line, aes(slope = beta0, intercept= beta1))   +
      geom_point() +
      xlab("solvency") +
      ylab("PIratio") +
      ylim(c(min (loanDf$PIratio), max(loanDf$PIratio))) +
      xlim(c(min(loanDf$solvency), max(loanDf$solvency))) +
      theme_bw() +
      theme(text=element_text(family="Arial"))

  })
  
  output$confusion <- renderTable({
    loanDf <- loanData(noApproved = 50, noDenied = 50,
                       muApproved = append(input$muApprovedx, input$muApprovedy), 
                       muDenied = append(input$muDeniedx, input$muDeniedy),
                       sdApproved = append(input$sdApprovedx, input$sdApprovedy), 
                       sdDenied = append(input$sdDeniedx, input$sdDeniedy),
                       rhoApproved = input$rhoapproved, rhoDenied = input$rhodenied, 
                       seed=1111)
    datafit <- lm(target ~ solvency + PIratio, data = loanDf)
    predictedLabels <- ifelse(predict(datafit) < 0.5, "Approved", "Denied")
    confMatrixFreq <- table(loanDf$deny, predictedLabels) 
    confMatrixFreq
  })
})