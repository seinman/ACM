library(shiny)
library(dplyr)
shinyUI(fluidPage(
  titlePanel('Loansies'),
  
  plotOutput('plot'),
  
  tableOutput('confusion'),
  
  fluidRow(
    column(6,
           h3('Approved'),
           fluidRow(
             h4('Mu'),
             column(3,
              numericInput(inputId = "muApprovedx",
                           label = 'PI ratio',
                           value = 10)),
             column(3,
              numericInput(inputId = "muApprovedy",
                           label = 'Solvency',
                           value = 10))),
           fluidRow(
             h4('Std Dev'),
             column(3,
              numericInput(inputId = "sdApprovedx",
                           label = 'PI ratio',
                           value = 10)),
             column(3,
              numericInput(inputId = "sdApprovedy",
                           label = 'Solvency',
                           value = 10))),
          sliderInput('rhoapproved',
                      label = 'Correlation, Approved',
                      min = -1,
                      max = 1,
                      value = 0,
                      step = 0.1)
    ),  
    fluidRow(
      column(6,
             h3('Denied'),
             fluidRow(
               h4('Mu'),
               column(3,
                      numericInput(inputId = "muDeniedx",
                                   label = 'PI ratio',
                                   value = 10)),
               column(3,
                      numericInput(inputId = "muDeniedy",
                                   label = 'Solvency',
                                   value = 10))),
             fluidRow(
               h4('Std Dev'),
               column(3,
                      numericInput(inputId = "sdDeniedx",
                                   label = 'PI ratio',
                                   value = 10)),
               column(3,
                      numericInput(inputId = "sdDeniedy",
                                   label = 'Solvency',
                                   value = 10))),
             sliderInput('rhodenied',
                         label = 'Correlation',
                         min = -1,
                         max = 1,
                         value = 0,
                         step = 0.1)
      ))
  ))
)