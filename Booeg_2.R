# Libraries
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(dplyr)
library(shinythemes)
library(tidyverse)
library(reshape2)

# Load datasets
burnTimes <- read.csv("sechselaeuten.csv")
temperatures <- read.csv2("TemperaturesZurich.csv", dec = ".")

#temperaturen und burnTimes mergen
mergedData <- merge(burnTimes, temperatures, by.x="year", by.y = "Year")

coToText <- function(c,v1="temperature", v2="precipitation"){
  t1 <- {
    c <- round(c,digits=2)
    if(c < -0.7) {
      ("strong negative correlation")
    } else if(c < -0.3){
      ("moderate negative correlation")
    } else if(c < 0){
      ("weak negative correlation")
    } else if(c < 0.3){
      ("weak positive correlation")
    } else if(c < 0.7){
      ("moderate positive correlation")
    }else if(c == 0){
      ("no correlation")
    } else {
      ("strong positive correlation")
    }      
  }
  t2 <- {
    if(c < 0) {
      paste("That means, the lower the",v1,"the higher the",v2,"and vice versa")
    }else{
      paste("That means, the higer the",v1,"the higher the",v2,"and vice versa")
    }
  }
  return(paste("There is a",t1,"with a correlation coefficient of r=",round(c,digits=2),".",t2,sep = " "))
}


#Aufbau Seite
ui <- 
  tagList(
    navbarPage(
    #Theme auswählen
    theme = shinytheme("flatly"),
    "Boeoeg",
    #Inputs
    
    #-------------------------------------------------------------------------------
    tabPanel(
      "Question 1",
      sidebarPanel(
        width = 3,
        radioButtons("Q1_radio", "Temperature:",
                     c("Mean" = "meanT",
                       "Max" = "maxT",
                       "Min" = "minT")),
        
        sliderInput("Q1_monthRange", label = h3("Select Months"), min = 1, max = 12, 
                    value = c(1, 12)),
      ),
      
      #Plots
      mainPanel(
        h3("Is there a (negative) correlation between the time it takes for the Boeoeg head to explode and a good weather in the subsequent summer (warm temperatures and low precipitation)?"),
        tableOutput("correlations"),
        plotlyOutput("HeatMap"),
        plotlyOutput("TempPercBurn"),
        plotlyOutput("BurnPerc"),
        p(textOutput("textQ1_2")),
        plotlyOutput("BurnTemp"),
        p(textOutput("textQ1_1"))
      )
    ),
    
    #-------------------------------------------------------------------------------
    tabPanel(
      "Question 2",
      sidebarPanel(
        width = 3,
        # radioButtons("dist3", "Temperature:",
        #              c("Mean" = "meanT",
        #                "Max" = "maxT",
        #                "Min" = "minT")),
        
        sliderInput("Q2_monthRange", label = h3("Select Months"), min = 1, max = 12, 
                    value = c(1, 12)),
        sliderInput("Q2_yearRange", label = h3("Select Years"), min = min(temperatures$Year), max = max(temperatures$Year), 
                    value = c(min(temperatures$Year), max(temperatures$Year))),
      ),
      
      #Plots
      mainPanel(
        h3("Is there a correlation between precipitation and temperatures?"),
        plotlyOutput("TempPerc"),
        p(textOutput("textQ2")),
      )
    ),
    
    #-------------------------------------------------------------------------------
    tabPanel(
      "Climate Change",
      sidebarPanel(
        width = 3,
        radioButtons("Q3_radio", "Temperature:",
                     c("Mean" = "meanT",
                       "Max" = "maxT",
                       "Min" = "minT")),
        sliderInput("Q3_monthRange", label = h3("Select Months"), min = 1, max = 12, 
                    value = c(1, 12)),
        sliderInput("Q3_yearRange", label = h3("Select Years"), min = min(temperatures$Year), max = max(temperatures$Year), 
                    value = c(min(temperatures$Year), max(temperatures$Year))),
      ),
      
      #Plots
      mainPanel(
        tableOutput("correlationsClimate"),
        plotlyOutput("HeatMap2"),
        plotlyOutput("Main"),
        p(textOutput("textQ3")),
        plotlyOutput("TempYear"),
        plotlyOutput("MaxMinTemp")
      )
    )
  )
)

#BackEnd
server <- function(input, output) {

  mergedDataF <- reactive({
    r <- switch(input$Q1_radio,"meanT" = mean,"maxT" = max,"minT" = min)
    values <- mergedData
    values <- values[values$Month >= input$Q1_monthRange[1] & values$Month <= input$Q1_monthRange[2],]
    values <- aggregate(values,by=list(values$year), FUN=r)
    values[,c("year","sec.burn", "Temperature", "Precipitation")]
  })
  
  temperaturesF2 <- reactive({
    t <- temperatures
    t <- t[t$Month >= input$Q2_monthRange[1] & t$Month <= input$Q2_monthRange[2],]
    t <- t[t$Year >= input$Q2_yearRange[1] & t$Year <= input$Q2_yearRange[2],]
  })
  
  temperaturesF <- reactive({
    t <- temperatures #Ausgewählte Monate
    t <- t[t$Month >= input$Q3_monthRange[1] & t$Month <= input$Q3_monthRange[2],]
    t <- t[t$Year >= input$Q3_yearRange[1] & t$Year <= input$Q3_yearRange[2],]
    tb <- merge(aggregate(t,by=list(t$Year), FUN=min), aggregate(t,by=list(t$Year), FUN=max), by.x="Year", by.y = "Year") # Max/Min
    tb <- merge(tb, aggregate(t,by=list(t$Year), FUN=mean), by.x="Year", by.y = "Year") #MaxMin / Durchschnitt
    tb$MaxT <- tb$Temperature.y
    tb$MinT <- tb$Temperature.x
    tb$MeanT <- tb$Temperature
    tb$MaxP <- tb$Precipitation.y
    tb$MinP <- tb$Precipitation.x
    tb$MeanP <- tb$Precipitation
    tb[2:13] <- NULL
    tb
  })
  
  corF <- reactive({
    values <- round(cor(mergedDataF()[complete.cases(mergedDataF()),]),2)
    values <- melt(values)
    values <- values[values$Var1 != "Month",]
    values <- values[values$Var2 != "Month",]
    values <- values[values$Var1 != "Group.1",]
    values <- values[values$Var2 != "Group.1",]
  })

  #-------------------------------------------Question 1 Figures---------------------------------------
  #Table Correlations
  output$correlations <- renderTable({
    cor(mergedDataF()[complete.cases(mergedDataF()),])
  },rownames = TRUE, hover = TRUE)   
  #HeatMapCorrelations
  
  output$HeatMap <- renderPlotly({
    ggplotly(
      ggplot(data = corF()[complete.cases(corF()),], aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile()
    )
  })
  
  output$TempPercBurn <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_bar(data=mergedDataF(), aes(x = year, y = sec.burn, colour="Brenndauer") , stat ="identity", position="dodge")+
        geom_line(data=mergedDataF(), aes(x = year, y = Temperature, colour="Temperatur"))+
        geom_line(data=mergedDataF(), aes(x = year, y = Precipitation, colour="Niederschlag"))+
        theme_ipsum()
    )
  })
  
  #Correlation BurnTime/Temperature
  output$BurnTemp <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_point(data=mergedDataF(), aes(x = Temperature, y = sec.burn) , stat ="identity")+
        geom_smooth(data=mergedDataF(), aes(x = Temperature, y = sec.burn, colour="F(x)"),method=lm)+
        theme_ipsum()
    )
  })
  output$textQ1_1 <- renderText({
    m <- mergedDataF()[complete.cases(mergedDataF()),]
    c <- cor(m$Temperature,m$sec.burn)
    coToText(c,v1="burn time", v2="temperature")
  })
  #Correlation BurnTime/Percipation
  output$BurnPerc <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_point(data=mergedDataF(), aes(x = Precipitation, y = sec.burn) , stat ="identity")+
        geom_smooth(data=mergedDataF(), aes(x = Precipitation, y = sec.burn, colour="F(x)"),method=lm)+
        theme_ipsum()
    )
  })
  output$textQ1_2 <- renderText({
    m <- mergedDataF()[complete.cases(mergedDataF()),]
    c <- cor(m$Precipitation,m$sec.burn)
    coToText(c,v1="burn time", v2="precipitation")
  })
  #-------------------------------------------Question 2 Figures---------------------------------------
  #Correlation Temperature/Percipation
  output$TempPerc <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_point(data=temperaturesF2(), aes(x = Temperature, y = Precipitation) , stat ="identity")+
        geom_smooth(data=temperaturesF2(), aes(x = Temperature, y = Precipitation, colour="Temperatur d1950"),method=lm)+
        theme_ipsum()
    )
  })
  
  
  output$textQ2 <- renderText({
    m <- temperaturesF2()[complete.cases(temperaturesF2()),]
    coToText(cor(m$Temperature,m$Precipitation))
  })
  
  #-------------------------------------------Climate Figures---------------------------------------
  #Overview Temperature

  output$correlationsClimate <- renderTable({
    cor(temperaturesF())
  },rownames = TRUE, hover = TRUE)
  
  output$HeatMap2 <- renderPlotly({
    ggplotly(
      ggplot(data = melt(round(cor(temperaturesF()),2)), aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile()
    )
  })
  
  output$textQ3 <- renderText({
    r <- switch(input$Q3_radio,"meanT" = 2,"maxT" = 3,"minT" = 4)
    m <- temperaturesF()[complete.cases(temperaturesF()),]
    c <- cor(m$Year,m[,r])
    coToText(c)
  })
  
  output$Main <- renderPlotly({
    
    r <- switch(input$Q3_radio,"meanT" = 2,"maxT" = 3,"minT" = 4)
    
    ggplotly(
      ggplot() +
        geom_line(data=temperaturesF(), aes(x = Year, y = temperaturesF()[,r], colour="Temperatur"))+
        geom_smooth(data=temperaturesF(), aes(x = Year, y = temperaturesF()[,r], colour="Temperatur d"),method="glm")+
        #(data=temperaturesF()[temperaturesF()$Year<=1950,], aes(x = Year, y = temperaturesF()[temperaturesF()$Year<=1950,r], colour="Temperatur d1950"),method=lm)+ #Gefiltert bis 1950
        #geom_smooth(data=temperaturesF()[temperaturesF()$Year>=1950,], aes(x = Year, y = temperaturesF()[temperaturesF()$Year>=1950,r], colour="Temperatur d1950"),method=lm)+
        theme_ipsum()
    )
  })
  
  #Correlation Temperature/Year
  output$TempYear <- renderPlotly({
    r <- switch(input$Q3_radio,"meanT" = 2,"maxT" = 3,"minT" = 4)
    ggplotly(
      ggplot() +
        geom_point(data=temperaturesF(), aes(x = Year, y = temperaturesF()[,r]) , stat ="identity")+
        geom_smooth(data=temperaturesF(), aes(x = Year, y = temperaturesF()[,r], colour="F(x)"),method=lm)+
        theme_ipsum()
    )
  })
  
  #Overview MaxMinTemperature
  output$MaxMinTemp <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_line(data=temperaturesF(), aes(x = Year, y = MaxT, colour="Max Temp"))+
        geom_line(data=temperaturesF(), aes(x = Year, y = MinT, colour="Min Temp"))+
        geom_smooth(data=temperaturesF(), aes(x = Year, y = MaxT, colour="Temp Trend Max"),method="glm")+
        geom_smooth(data=temperaturesF(), aes(x = Year, y = MinT, colour="Temp trend Min"),method="glm")+
        theme_ipsum()
    )
  })
}
shinyApp(ui = ui, server = server)