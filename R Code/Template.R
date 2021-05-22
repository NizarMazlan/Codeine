# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(plyr)
library(dplyr)

# Data preparation
setwd("C:/R Studio/First Shiny App")
occupationData <- read.csv("01-Occupation.csv")
#genderData <- read.csv("02-Gender.csv")
#ageData <- read.csv("03-Age.csv")
#academicData <- read.csv("04-Academic.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  theme = "superhero",  
                  "DRUGGER-tune",
                  # Embed R Markdown under this tab
                  tabPanel("About",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("History of Drug Addicts in Malaysia"),
                             
                             h4("In this panel, we embed the R Markdown"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Allocation", "This panel is intentionally left blank"),
                  tabPanel("Statistics", "This panel shows the statistics of drug
                           addicts by categories.",
                           sidebarPanel(
                             # choose the color of bar
                             selectInput(inputId="color1",label="Choose Color",
                                         choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                                         selected = "Green",multiple = F),
                             # choose color of border
                             radioButtons(inputId = "border1",label = "Select Border",
                                          choices = c("Black"="#000000","White"="#ffffff")),
                             
                             # choose the type of dataset
                             selectInput(inputId ="channel1", label = "Choose Dataset",
                                         choices = c("Occupation","Academic","Gender","Age")),
                             
                             # choose range of year
                             sliderInput(inputId = "yearRange",
                                         label = "Range (Years):",
                                         min = 2017,
                                         max = 2019,
                                         value = 3)
                             
                           ),
                           
                           
                           
                           # main panel for displaying histogram
                           mainPanel(
                             plotOutput(outputId = "distPlot")
                           )
                           
                           
                           )
                           
                  
                ) # navbarPage
) # drugPage


# Define server function  
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  
  # output under Statistics tab
  # output choosing bar color
  output$distPlot <- renderPlot({
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    
    
  })
  
  # output under statistics tab
  # output choose dataset

  
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
