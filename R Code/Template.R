# Load R packages
library(png)
library(shiny)
library(shinythemes)
library(shinydashboardPlus)
library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)
library(leaflet)
library(readxl)
library(tidyverse) #for bubble graph
library(shinydashboard) #for bubble graph
library(bubbles) #for bubble graph


# Data preparation
occupationData <- read.csv("C:/Users/User/Documents/GitHub/my-drug-hotspot/Dataset_Latest/01-Occupation.csv")
genderData <- read.csv("C:/Users/User/Documents/GitHub/my-drug-hotspot/Dataset_Latest/02-Gender.csv")
ageData <- read.csv("C:/Users/User/Documents/GitHub/my-drug-hotspot/Dataset_Latest/03-Age.csv")
academicData <- read.csv("C:/Users/User/Documents/GitHub/my-drug-hotspot/Dataset_Latest/04-Academic.csv")
negeriData <- read.csv("C:/Users/User/Documents/GitHub/my-drug-hotspot/Dataset_Latest/negeriData.csv")
ppd <- read_excel("C:/Users/User/Documents/GitHub/my-drug-hotspot/Dataset_Latest/Lokasi-Pusat-Pemulihan-Dadah.xlsx")
namaTempat <- ppd$Institution
listppd <- read_excel("C:/Users/User/Documents/GitHub/my-drug-hotspot/Dataset_Latest/Alamat-Pusat-Pemulihan.xlsx")
reasons <- read_excel("C:/Users/User/Documents/GitHub/my-drug-hotspot/Dataset_Latest/Sebab-Sebab.xlsx")

#total for total cases
total2014 = sum(negeriData$X2014)
total2015 = sum(negeriData$X2015)
total2016 = sum(negeriData$X2016)
total2017 = sum(negeriData$X2017)
total2018 = sum(negeriData$X2018)
total2019 = sum(negeriData$X2019)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                shinythemes::themeSelector(),
                #setting for all html tags
                #change fonts and what not
                tags$head(
                  tags$style(HTML("
                      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
                      /* Change font of header text */
                      p {
                          color: black;
                          font-family: sans-serif;
                          font-size: 130%;
                        }
                      ul {
                          color: black;
                          font-family: sans-serif;
                          font-size: 130%;
                        }
                      /* Make text visible on inputs */
                      .shiny-input-container {
                        color: #474747;
                      }"))
                ),
                
                #starting of navbar
                navbarPage(
                  theme = "superhero",  
                  "Codeine",
                  # Home panel
                  tabPanel("Home",
                           sidebarPanel(
                             h2("Codeine"),
                             p("Our Group name is Codeine, we chose this name because it is a type of weak drug that is used as a pain reliever and cough supressent"),
                             br(),
                             br(),
                             HTML('<center><img src="https://raw.githubusercontent.com/NizarMazlan/my-drug-hotspot/main/R%20Code/www/logoCodeine.png", height = 200, width = 200></center>')
                           ),
                           mainPanel(
                             h1("Hello fellow Malaysian"),
                             p("This website is created by us to help Malaysians understand fully situation of drug addiction in Malaysia. We hope that everyone have awareness on this issue and take action that drug addiction is avoidable.
                               For those of you who do not know what drug addiction is, drug addiction is a disease that affects a person's brain and behavior that lead to inability to control the use of a legal or illegal drug or medication.
                               Substance such as alcohol, marijuana and nicorine also are considered as drug. When you are addicted, you may continue using the drug despite the harm it causes.
                               Although the risk of addiciton and how fast you become addicted varies by drug. Some drugs, such as opioid painkillers, have a higher risk and cuase addiction more quickly than others"),
                             br(),
                             br(),
                             strong(h3("How do we come to Drug Addiction Project?")),
                             strong(h4("Problem statement")),
                             p("The use, misuse, and abuse of substances and drugs among adolescents and youngsters has been found to be on the rise. Because there hasn't been much research done in Malaysia to analyze this issue."),
                             br(),
                             br(),
                             strong(h4("Solution")),
                             p("Drug abuse is a complex issue and has been a serious public health problem in Malaysia. As a result, we created this project that has informations on statistic of drug addiction ,places you can get help if you face drug abuse problem and we did it to spread awareness among Malaysians."),
                             br(),
                             strong(h3("The Questions?")),
                             p("- What is the range of age who take drugs?"),
                             p("- What is the trends of change of drugers through year 2014 - 2019 in Malaysia?"),
                             p("- How many drug addicts who have jobs and those who dont?"),
                             p("- Is drug addicts among teenagers is higher than adults?"),
                             br(),
                             strong(h3("Objective")),
                             p("- To identify patterns of drug addicts' trends from 2017 to 2019 in Malaysia"),
                             p("- To show the graph illustration of drugs addicts by occupation"),
                             p("- To display drug addicts according to their age"),
                             p("- To monitor graphical presentation of drug addicts by gender"),
                             p("- To display drug addicts based on academic qualifications"),
                             p("- To display the places where drug addicts can get professional help"),
                             br(),
                             strong(h3("Datasets Used")),
                             p("We use datasets from the official website of National Anti-Drugs Agency and from the website of Department of Statistics Malaysia"),
                             verbatimTextOutput("txtout"),
                           ) # mainPanel
                           
                  ),
                  #Bubble chart tab
                  tabPanel("By State", 
                           h3("This panel shows the bubble graph for states"),
                           fluidRow(
                             valueBoxOutput("T2014"),
                             valueBoxOutput("T2015"),
                             valueBoxOutput("T2016"),
                             valueBoxOutput("T2017"),
                             valueBoxOutput("T2018"),
                             valueBoxOutput("T2019")
                           ),
                           
                           fluidRow(
                             sidebarPanel(
                               sliderInput(inputId = "bubbleYear", label = "Choose Year", min = 2014, max = 2019, value = 2014),
                             ),
                             box(
                               width = 8, status = "info", solidHeader = TRUE,
                               title = "Drug Addicts by States in Malaysia",
                               bubblesOutput("bubbleDrug", width = "100%", height = 600)
                             )
                           )
                           
                  ),
                  #Stats Tab
                  tabPanel("Statistics",
                           h2("Statistics of Drug Addicts in Malaysia by Categories."),
                           sidebarPanel(
                             # choose the type of dataset
                             selectInput(inputId ="channel1", label = "Choose Dataset",
                                         choices = c("Occupation"="occ",
                                                     "Academic"="acd",
                                                     "Gender"="gen",
                                                     "Age"="age")),
                             h6(""),
                             h6("Do you want to see the average for the dataset choosen?"),
                             checkboxInput("chAverage", "Average", FALSE),
                             radioButtons("sum", 
                                          
                                          "You can choose the type of summary for this graph:",
                                          c("Simplified" = "norm",
                                            "Description" = "uniq"))
                           ),
                           # main panel for displaying histogram
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Plot", plotOutput("barPlot")), 
                               tabPanel("Summary",
                                        
                                        verbatimTextOutput("summary"),
                                        verbatimTextOutput("class"),
                                        verbatimTextOutput("features"),
                                        verbatimTextOutput("str"),
                                        verbatimTextOutput("min"),
                                        verbatimTextOutput("max"))  
                             )
                             
                           )
                           
                  ),
                  tabPanel("Reasons",
                           h2("Reasons of People in Malaysia for Using Drugs."),
                           HTML('<center><img src="https://raw.githubusercontent.com/NizarMazlan/my-drug-hotspot/main/R%20Code/www/radialplot.png", width = 40%></center>'),
                           br(),
                           br(),
                           h3("The chart above explains the initial reasons for people to start being addicted into drugs. The reasons are :"),
                           tags$ul(
                             tags$li(" Peer influence"),
                             tags$li(" Curiosity"),
                             tags$li(" For Fun"),
                             tags$li(" Stress"),
                             tags$li(" Relief Pain"),
                             tags$li(" Stimulant"),
                             tags$li(" Ignorance / Unaware"),
                             tags$li(" Others")
                           ),
                           br(),
                           br(),
                           p("As you all can see, Peer Influence is the most common reason for people to get into drugs. Biggest part of this might be because they want to fit in amongst their peers. They might think it is not a big deal since their peers do drugs so easily without thinking the consequences of being addicted to drugs.
                              Well human beings are social creatures, it is important for humans to feel normal and like we belong or fit in. This can contribute them to try and influenced to take drugs. At first, they may think they can control how much and how often to use it. But over time, drugs change how your brain works.
                             These physical changed can last a long time. They make you lose control and can lead to damaging behaviors. Drug addiction can also cause other disease such as Cardiovascular disease, stimulants such as cocaine methamphetamines, can damage the heart and blood vessels.
                             The long term use of these drugs can lead to coronary artery disease, arrhythmia, and heart attack. Other diseases like respiratory problems, kidney damage, liver disease and of course ovedose can be caused by drug addiction"),
                           br(),
                           br(),
                           p("But if you ever in the situation in drug addiction there are things that you can do. It is never too late to quit using drugs. Reducing and quitting drugs can improve your life significantly. It can improve your physical and mentall wellbeing.
                             Other than that, it can reduce your risk of permanent damage to your vital organs. By having good emotional wellbeing you will definitely improve your relationship with friends and family and be connected to your emotions.
                             Quitting drugs is not easy, it may vary between people, drugs and rang from mild to serious. They can last from few das to a few weeks. Craving for it will sometimes be week and at other times very strong. Learning how to control and manage them is important to stay drug-free.
                             There are a few ways to quit drugs. Firstly, you have to admit you have a problem. If you realized that you are having drug addiction problem, you then must take action to either see a specialist. It is very important to do so as they can help you get appropriate help and support.
                             Reach out and get help, there is a lot of NGOs, Government, and private sectors provide services to cope with drug addiction."),
                           br(),
                           br(),
                           p("It is also important for you to learn how to help others, for example your friends and family. Firstly you must be mindful and alert to some possible indications.
                             For instance, problems at school or work, frequently missing shcool or work, a sudden disinterest in school event, activities, or work, or drop in grades or work performances. Next would be their physical health, lack of energy or motivation to take care of their wellbeing.
                             Neglected their appearance such as lack of interest in clothing or grooming might also be a possible indication. But most importantly, look for a change in behavior. They might have exaggerated efforts to bar family members from entering his or her room or being secretive aobut where he or she is going or going with"),
                           br(),
                           br(),
                           p("As a Malaysian, I think we also play a big role in fighting drug addiction and drug abuse problem in Malaysia. Even though maybe some of you might not have any experience meeting a drug addicts or even be a drug addict, you should still learn what it is about. Drug addiction can really destroy lives.
                             The least you can do is to learn about the effects of drugs that can help you understand why there are some people are drug addicts and why is it hard to quit. Secondly, show that you care without judging. Being calm and respectful may encourage them to open up and honest with you, they might even try harder to quit from being a drug addict.
                             In addition, always be positive and encouraging rather than negative and nagging other people that have drug addiction problems. Relapse may happen to them and sometimes it is inevatible, but that does not mean that the person can't try again to quit. Lastly, if you encounter a drug addict that you know, offer them practical support.
                             Sometimes just being there is enough, but you can offer to got with them to parties or join them for a walk, run or go to a rehabilitation centre. One small kindness can really change peoples lives.")
                           
                  ),
                  #Locations tab
                  tabPanel("Locations of Rehab Centre",
                           h3("This panel shows the locations of Drug Rehabilitation Centre in Malaysia."),
                           
                           leaflet(data=ppd) %>%
                             addProviderTiles("Esri.WorldImagery") %>%
                             addMarkers(lng = ~ Longitud, lat = ~ Latitud,
                                        clusterOptions = markerClusterOptions(),
                                        popup = namaTempat),
                           mainPanel(
                             h4("List of the location with their address and contact number", align = "center")
                           ),
                           br(),
                           fluidRow(
                             column(12, dataTableOutput('table'), align = "center")
                           )),
                  #About us Panel
                  tabPanel("About Us",
                           h1("Group Members", align = "center"),
                           HTML('<center><img src="https://raw.githubusercontent.com/NizarMazlan/my-drug-hotspot/main/R%20Code/www/3.png", width = 75%></center>'),
                           h3("Here is our group member for this project, all 4 of us are First Year students in University of Malaya, majoring in Data Science. Of course the names are just our aliases. Our true names are :",align = "center"),
                           h3("- DJ Salik is IRFAN ABIDIN AS SALIK", align = "center"),
                           h3("- Roger Aiman is AIMAN WAFIQ", align = "center"),
                           h3("- Andrew Nzr is MOHAMAD NIZAR MUSTAQEEM", align = "center"),
                           h3("- Dayah the Explorer is SITI NORHIDAYAH", align = "center")
                  ),
                  #footer
                  footer = dashboardFooter(
                    left = "By Codeine.R",
                    right = "Malaysia, 2021"
                  )
                  
                  
                ) # navbarPage
) # drugPage


# Define server function  
server <- function(input, output) {
  
  
  #output table of address and contact number
  output$table <- renderDataTable(listppd)
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  
  # The # of "2014"
  output$T2014 <- renderValueBox({
    valueBox(
      value = format(total2014, big.mark=","),
      subtitle = "Total # of 2014",
      color = "aqua" 
    )
  })
  
  # The # of "2015"
  output$T2015 <- renderValueBox({
    valueBox(
      value = format(total2015, big.mark=","),
      subtitle = "Total # of 2015"
    )
  })
  
  # The # of "2016"
  output$T2016 <- renderValueBox({
    valueBox(
      value = format(total2016, big.mark=","),
      subtitle = "Total # of 2016"
    )
  }) 
  
  # The # of "2017"
  output$T2017 <- renderValueBox({
    valueBox(
      value = format(total2017, big.mark=","),
      subtitle = "Total # of 2017"
    )
  })
  
  # The # of "2018"
  output$T2018 <- renderValueBox({
    valueBox(
      value = format(total2018, big.mark=","),
      subtitle = "Total # of 2018"
    )
  })
  
  # The # of "2019"
  output$T2019 <- renderValueBox({
    valueBox(
      value = format(total2019, big.mark=","),
      subtitle = "Total # of 2019"
    )
  })
  
  
  # The bubble drug plot
  output$bubbleDrug <- renderBubbles({
    if (nrow(negeriData) == 0)
      return()
    if(input$bubbleYear == 2014){
      bubbles(value = negeriData$X2014, 
              label = negeriData$Negeri,
              # label = element_text(size = 10, face ="bold", colour ="black"),
              color = rainbow(16, alpha=NULL)[sample(16)] )
    }else if(input$bubbleYear == 2015){
      bubbles(value = negeriData$X2015, 
              label = negeriData$Negeri, 
              color = rainbow(16, alpha=NULL)[sample(16)] )
    }else if(input$bubbleYear == 2016){
      bubbles(value = negeriData$X2016, 
              label = negeriData$Negeri, 
              color = rainbow(16, alpha=NULL)[sample(16)] )
    }else if(input$bubbleYear == 2017){
      bubbles(value = negeriData$X2017, 
              label = negeriData$Negeri, 
              color = rainbow(16, alpha=NULL)[sample(16)] )
    }else if(input$bubbleYear == 2018){
      bubbles(value = negeriData$X2018, 
              label = negeriData$Negeri, 
              color = rainbow(16, alpha=NULL)[sample(16)] )
    }else if(input$bubbleYear == 2019){
      bubbles(value = negeriData$X2019, 
              label = negeriData$Negeri, 
              color = rainbow(16, alpha=NULL)[sample(16)] )
    }
    
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
        # Sort data
        bar <- academicData$Academic_Qualification
        val <- academicData$Average
        
        df <- data.frame(bar,val)
        
        # Bar chart
        ggplot(data = df, aes(x = reorder(bar, val), y = val)) +
          geom_bar(stat='identity',aes(fill = val) )+ 
          coord_flip() +
          ggtitle("Average Drug Addicts in Malaysia Sorted by Academic Qualification from Y2017-Y2018") +
          xlab("Academic Qualification") +
          ylab("Average Count from 2017-2019") +
          scale_fill_gradient("Average", low="darkolivegreen1",high="deepskyblue3") +
          theme_bw(base_size = 10)
        
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
        val <- ageData$Average
        
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
                
        # prepare data
        
        m17 <- round(sum(genderData$M17)/sum(genderData$Total17)*100,2)
        f17 <- round(sum(genderData$F17)/sum(genderData$Total17)*100,2)
        m18 <- round(sum(genderData$M18)/sum(genderData$Total18)*100,2)
        f18 <- round(sum(genderData$F18)/sum(genderData$Total18)*100,2)
        m19 <- round(sum(genderData$M19)/sum(genderData$Total19)*100,2)
        f19 <- round(sum(genderData$F19)/sum(genderData$Total-19)*100,2)
        
        df <- data.frame(m17,f17,m18,f18,m19,f19)
        percentage <- c(df[,"m17"], df[,"f17"], df[,"m18"], df[,"f18"],
                        df[,"m19"], df[,"f19"])
        gender <- rep(c("Male","Female"), times = 3)
        year <- rep(c("2017","2018","2019"),each = 2)
        
        df2 <- data.frame(gender, year,percentage)
        
        ggplot(df2, aes(x = year, y = percentage, fill = gender, color = year)) +
          ggtitle("Drug Addicts in Malaysia Sorted by Gender from Year 2017 until 2019")+
          geom_col(size=.8) +
          scale_color_manual(values=c("red", "blue", "purple")) +
          scale_fill_viridis_d() +
          coord_polar("y")
      }
      
    }
    
  })
  
  
  # output for summary under statistics tab
  output$summary <- renderPrint({
    if(input$channel1 == "occ"){
      if(input$sum == "norm"){
        summary(occupationData)
      }else{
        
      }
    }else if(input$channel1 == "acd"){
      if(input$sum == "norm"){
        summary(academicData)
      }else{
        
      }
    }else if(input$channel1 == "age"){
      if(input$sum == "norm"){
        summary(ageData)
      }else{
        
      }
    }else{
      if(input$sum == "norm"){
        summary(genderData)
      }else{
        
      }
    }
 
  })
    
   # output for description: class
  output$class <- renderPrint({
    str <- "Type of class: "
    if(input$channel1 == "occ"){
      if(input$sum == "uniq"){
        paste(str,class(occupationData))
      }
      
    }else if(input$channel1 == "acd"){
      if(input$sum == "uniq"){
        paste(str,class(academicData))
      }
    }else if(input$channel1 == "age"){
      if(input$sum == "uniq"){
        paste(str,class(ageData))
      }
    }else{
      if(input$sum == "uniq"){
        paste(str,class(genderData))
      }
    }
    
  })
  
  # output for description: names of features
  output$features <- renderPrint({
    
    if(input$channel1 == "occ"){
      if(input$sum == "uniq"){
        names(occupationData)
      }
      
    }else if(input$channel1 == "acd"){
      if(input$sum == "uniq"){
        names(academicData)
      }
    }else if(input$channel1 == "age"){
      if(input$sum == "uniq"){
        names(ageData)
      }
    }else{
      if(input$sum == "uniq"){
        names(genderData)
      }
    }
    
  })

  # output for description: Format
  output$str <- renderPrint({
    
    if(input$channel1 == "occ"){
      if(input$sum == "uniq"){
        str(occupationData)
      }
      
    }else if(input$channel1 == "acd"){
      if(input$sum == "uniq"){
        str(academicData)
      }
    }else if(input$channel1 == "age"){
      if(input$sum == "uniq"){
        str(ageData)
      }
    }else{
      if(input$sum == "uniq"){
        str(genderData)
      }
    }
    
  })
  
  # output for description: minimum value
  output$min <- renderPrint({
    str <- "Minimum total of drug addicts reached in 2019: "
    if(input$channel1 == "occ"){
      if(input$sum == "uniq"){
        min <- min(occupationData$X2019)
        paste(str,min)
      }
      
    }else if(input$channel1 == "acd"){
      if(input$sum == "uniq"){
        min <- min(academicData$X2019)
        paste(str,min)
      }
    }else if(input$channel1 == "age"){
      if(input$sum == "uniq"){
        min <- min(ageData$X2019)
        paste(str,min)
        
      }
    }else{
      if(input$sum == "uniq"){
        min <- min(genderData$Total19)
        paste(str,min)
      }
    }
  })
  
 # output for description: minimum value
  output$min <- renderPrint({
    str <- "Minimum total of drug addicts reached in 2019: "
    if(input$channel1 == "occ"){
      if(input$sum == "uniq"){
        min <- min(occupationData$X2019)
        paste(str,min)
      }
      
    }else if(input$channel1 == "acd"){
      if(input$sum == "uniq"){
        min <- min(academicData$X2019)
        paste(str,min)
      }
    }else if(input$channel1 == "age"){
      if(input$sum == "uniq"){
        min <- min(ageData$X2019)
        paste(str,min)
        
      }
    }else{
      if(input$sum == "uniq"){
        min <- min(genderData$Total19)
        paste(str,min)
      }
    }
  })
  
  # output for description: maximum value
  output$max <- renderPrint({
    str <- "Maximum total of drug addicts reached in 2019: "
    if(input$channel1 == "occ"){
      if(input$sum == "uniq"){
        max <- max(occupationData$X2019)
        paste(str,max)
      }
      
    }else if(input$channel1 == "acd"){
      if(input$sum == "uniq"){
        max <- max(academicData$X2019)
        paste(str,max)
      }
    }else if(input$channel1 == "age"){
      if(input$sum == "uniq"){
        max <- max(ageData$X2019)
        paste(str,max)
        
      }
    }else{
      if(input$sum == "uniq"){
        max <- max(genderData$Total19)
        paste(str,max)
      }
    }
  })
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
