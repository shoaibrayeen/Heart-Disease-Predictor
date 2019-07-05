#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(FNN)
library(ElemStatLearn)

data(SAheart)
# Function to Calculate KNN
Helper_KNN <-function (testdata) {
  traindata <- cbind(SAheart[,9] , SAheart[,8] , SAheart[,7] , SAheart[,2] , SAheart[,6])
  cl <- SAheart[ , 10]
  temp <- knn.reg(traindata ,testdata , cl , k = 89)
  return(temp)
}
Helper_regression <- function(testdata) {
  regression_model <- glm(chd ~ age + alcohol + obesity + tobacco + typea , family ="binomial" , data = SAheart)
  temp <- predict(regression_model , testdata)
  return(temp)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  reactive_panel <- reactive({
    ageInput <- input$slider1
    alcoholInput <- input$slider2
    obesityInput <- input$slider3
    tobaccoInput <- input$slider4
    typeaInput <- input$slider5
    testdata <- data.frame(age = ageInput, alcohol = alcoholInput, obesity = obesityInput,
                           tobacco = tobaccoInput, typea = typeaInput )
    Helper_KNN(testdata)
  })
  
  reactive_panel2 <- reactive({
    ageInput <- input$slider1
    alcoholInput <- input$slider2
    obesityInput <- input$slider3
    tobaccoInput <- input$slider4
    typeaInput <- input$slider5
    testdata <- data.frame(age = ageInput, alcohol = alcoholInput, obesity = obesityInput,
                           tobacco = tobaccoInput, typea = typeaInput )
    Helper_regression(testdata)
  })
  output$knn <- renderText({
    temp <- reactive_panel()
    temp <- temp$pred
    temp*100
  })
  output$pred1 <- renderText({
    temp <- reactive_panel2()
    temp <-exp(temp)/(1+exp(temp)) 
    temp*100
  })
  # generate bins based on input$bins from ui.R
  
})
