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
                             # choose the type of dataset
                             selectInput(inputId ="channel1", label = "Choose Dataset",
                                         choices = c("Occupation"="occ",
                                                     "Academic"="acd",
                                                     "Gender"="gen",
                                                     "Age"="age")),
                             checkboxInput("chAverage", "Average", FALSE)
                             
                           ),
                           
                           
                           # main panel for displaying histogram
                           mainPanel(
                             plotOutput("barPlot")
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
  output$barPlot <- renderPlot({
    if(input$channel1 == "occ"){
      if(input$chAverage == TRUE){
        # Sort data
        bar <- occupationData$Occupation
        val <- occupationData$Average
        
        df <- data.frame(bar,val)
        
        
        
        # Bar chart
        ggplot(data = df, aes(x = reorder(bar, val), y = val)) +
          geom_bar(stat='identity',aes(fill = val) )+ 
          coord_flip() +
          ggtitle("Drug Addicts in Malaysia Sorted by Occupation") +
          xlab("Occupation") +
          ylab("Average Count from 2017-2019") +
          scale_fill_gradient("Average", low="darkolivegreen1",high="deepskyblue3") +
          theme_bw(base_size = 15)
      }else{
        # Sort data
        df <- data.frame(
          Y2017 = occupationData$X2017,
          Y2018 = occupationData$X2018,
          Y2019 = occupationData$X2019,
          
          Occupation = as.factor(occupationData$Occupation)
        )
        
        # Gather and Update data frame
        df_new <- df %>%
          gather("Year", "Count", -Occupation)
        
        
        # Plotting Multiple Bar Graph
        p <- ggplot(df_new, aes(Occupation, y = Count, 
                           fill = Year)) +
          ggtitle("Drug Addicts in Malaysia Sorted by Occupation from Year 2017 until 2019") +
          xlab("Year") +
          ylab("Average Count from 2017-2019") +
          geom_col(position = "dodge", stat='identity') + 
          geom_text(aes(label=Count), vjust=0.3, color="black",
                    position = position_dodge(0.9))
        p + coord_flip()
      }
      
    }else if(input$channel1 == "acd"){
      if(input$chAverage == TRUE){
        bar <- academicData$Academic_Qualification
        val <- mean(c(academicData$x2017,academicData$x2018,academicData$x2019))
        
        df <- data.frame(bar,val)
        
        # Multiple Bar Chart
        ggplot(data = df, aes(x = reorder(bar, val), y = val)) +
          geom_bar(stat='identity',aes(fill = val) )+ 
          coord_flip() +
          ggtitle("Average of Drug Addicts in Malaysia Sorted by 
                  Academic Qualification From Year 2017 until 2019") +
          xlab("Academic Qualification") +
          ylab("Average Count from 2017-2019") +
          scale_fill_gradient("Average", low="darkolivegreen1",high="deepskyblue3") +
          theme_bw(base_size = 15)
        
      }else{
        # Sort data
        df <- data.frame(
          Y2017 = academicData$X2017,
          Y2018 = academicData$X2018,
          Y2019 = academicData$X2019,
          Qualification = as.factor(academicData$Academic_Qualification)
        )
        
        # Gather and Update data frame
        df_new <- df %>%
          gather("Year", "Count", -Qualification)
        
        # Plotting Multiple Bar Graph
        ggplot(df_new, aes(Qualification, y = Count, 
                           fill = Year)) +
          ggtitle("Drug Addicts in Malaysia Sorted by Academic Qualification from Year 2017 until 2019") +
          xlab("Academic Qualification") +
          ylab("Average Count from 2017-2019") +
          geom_col(position = "dodge", stat='identity') + 
          geom_text(aes(label=Count), vjust=-0.25, color="black",
                    position = position_dodge(0.9))
      }
      
    }else if(input$channel1 == "age"){
      if(input$chAverage == TRUE){
        bar <- ageData$Age
        val <- mean(c(ageData$x2017,ageData$x2018,ageData$x2019))
        
        df <- data.frame(bar,val)
        
        # Multiple Bar Chart
        ggplot(data = df, aes(x = reorder(bar, val), y = val)) +
          geom_bar(stat='identity',aes(fill = val) )+ 
          coord_flip() +
          ggtitle("Average of Drug Addicts in Malaysia Sorted by 
                  Range of Age From Year 2017 until 2019") +
          xlab("Range of Age") +
          ylab("Average Count from 2017-2019") +
          scale_fill_gradient("Average", low="darkolivegreen1",high="deepskyblue3") +
          theme_bw(base_size = 15)
      }else{
        # Sort data
        df <- data.frame(
          Y2017 = ageData$X2017,
          Y2018 = ageData$X2018,
          Y2019 = ageData$X2019,
          Range = as.factor(ageData$Age)
        )
        
        # Gather and Update data frame
        df_new <- df %>%
          gather("Year", "Count", -Range)
        
        # Plotting Multiple Bar Graph
        ggplot(df_new, aes(Range, y = Count, 
                           fill = Year)) +
          ggtitle("Drug Addicts in Malaysia Sorted by Age from Year 2017 until 2019") +
          xlab("Age Range") +
          ylab("Count from 2017-2019") +
          geom_col(position = "dodge", stat='identity') + 
          geom_text(aes(label=Count), vjust=-0.25, color="black",
                    position = position_dodge(0.9))
      }
      
    }else{#gender
      if(input$chAverage == TRUE){
        # Sort data
        df <- data.frame(
          Male = genderData$Average_M,
          Female = genderData$Average_F,
          State = as.factor(genderData$States)
        )
        
        # Gather and Update data frame
        df_new <- df %>%
          gather("Gender", "Average", -State)
        
        # Plotting Multiple Bar Graph
        p <- ggplot(df_new, aes(reorder(State,Average), y = Average, fill = Gender)) +
          geom_col(position = "dodge", stat='identity') + 
          geom_text(aes(label=Average), vjust=0.3, color="black",
                    position = position_dodge(0.93))
        
        p + coord_flip()
      }else{
        
      }
    }
  })

  
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
