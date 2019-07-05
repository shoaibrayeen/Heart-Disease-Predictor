#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Heart Disease Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider1","Age",min = 10,max = 65,value = 42.82),
      sliderInput("slider2","Alcohol Consumption",min = 0,max = 150,value = 17.04),
      sliderInput("slider3","Obesity(based on BMI)",min = 14,max = 47,value = 26.04),
      sliderInput("slider4","Tobacco Level",min = 0,max = 32,value = 3.6356),
      sliderInput("slider5","Bortner Short Rating Scale (Type A Behaviour)",min = 13,max = 78,value = 53.1)
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Instruction",
                           p(" "),
                           p("This web application calculates the probability of having Heart Disease based on five variables."),
                           p("Just change  the five slider values and see the probability value changes correspondingly.")),
                  tabPanel("Probability of having Heart Disease ( in % ) ", 
                           p(h1("Logistic Regression : ")) , h1(textOutput("pred1")) ,
                           p(h1("KNN Regression: ")) ,  h1(textOutput("knn")))
                  
      )
    )
  )
)
)
