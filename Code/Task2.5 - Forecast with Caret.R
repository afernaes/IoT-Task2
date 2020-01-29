#### ---- Libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_minimal())
library(zoo)
library(plotly)
library(ggplot2)
library(ggfortify) #http://www.sthda.com/english/wiki/ggfortify-extension-to-ggplot2-to-handle-some-popular-packages-r-software-and-data-visualization
#library(forecast)
library(magrittr)
library(reshape2)
library(reshape)
library(TTR)
library(imputeTS)
library(broom)
library(caret)
library(seasonal)
library(prophet)
library(tseries)
library(doParallel)
library(prophet)
library(Metrics)
library(scales) #To tune scales dates in ggplot
Sys.setlocale("LC_TIME", "English") #If we dont put this, we will have some things in spanish
setwd("C:/Users/Ariadna/Downloads/UBIQUM COURSE/3.IoT Analytics/Task2")

#### ---- Importing the Data ----

    EnergyConsumption <- readRDS(file = './Input/EnergyConsumption_External_4years.RDS')

#### ---- Daily Time Series Creation and Analysis -----
  
  #Create Daily Observations, and add the Date (for future Prophet)

    Group_Daily <- EnergyConsumption %>% 
      group_by(Year, Quarter, Month, Weekend, Day) %>% 
      summarise(Kitchen=round(sum(Kitchen/1000),3),
                          Laundry=round(sum(Laundry/1000),3),
                          Heater_AC=round(sum(Heater_AC/1000),3),
                          Others=round(sum(Others/1000),3),
                          GAP=round(sum(GAP/1000),3),
                          Temperature=mean(Temperature),
                          Cost=round(sum(Cost),3)) %>% 
      mutate(Date = as_date(paste(Year, Month, Day, sep="-"), "%Y-%m-%d"))
  
    #Let's use the variables for create a model for GAP
    names(Group_Daily)
    Group_Daily %<>% select (GAP, Date, Year, Quarter, Weekend, Temperature)

    #Dummify Data
      #The function applies to factors only:
      str(Group_Daily)
      Group_Daily$Month<-NULL    
      Group_Daily$Year<-as.numeric(Group_Daily$Year)
      summary(Group_Daily$Year)
      
      #Reasign year, that lost the property when we changed the type
      Group_Daily$Year[Group_Daily$Year == 1] <- 2007
      Group_Daily$Year[Group_Daily$Year == 2] <- 2008
      Group_Daily$Year[Group_Daily$Year == 3] <- 2009
      Group_Daily$Year[Group_Daily$Year == 4] <- 2010
      
      #Dummify
      
      newDataFrame <- dummyVars(" ~ .", data = Group_Daily, fullRank = T)
      Group_Daily<- data.frame(predict(newDataFrame, newdata = Group_Daily))
      
    #Train and Test Set
      
      train <- Group_Daily %>% filter(Year %in% c(2006, 2007, 2008, 2009))
      test <- Group_Daily %>% filter(Year == 2010)
      
    #Modelling -Linear Model
      
      fit_LM1 <- caret::train(GAP~. ,data = train %>% select(-Date, -Year),
                             method = "lm",
                             preProcess = c("scale"))

    
      test$Predictions_LM1 <- predict(fit_LM1, test)
      postResample(test$Predictions_LM1, test$GAP)
    
    #Check important variables
    varTun <- varImp(fit_LM1)
    plot(varTun, main = "Top variance importance")
      
     
    #Plotting
    test$Date<-as_date(test$Date)
    train$Date<-as_date(train$Date)
    

    ggplot(test, aes(x=Date)) + scale_x_date(labels = date_format("%m-%Y"))+
       geom_line(aes(y=Predictions_LM1), color="red")+
      geom_line(data=train, aes(x=Date, y=GAP)) + xlab("Date") + ylab("kWh")+
      ggtitle("Daily Forecast for next Year") + theme(plot.title = element_text(hjust = 0.5))
    
    
    a<-mape(test$GAP,test$Predictions_LM1)
    
    #Modelling -Random Forest
    
    fit_RF1 <- caret::train(GAP~. ,data = train %>% select(-Date, -Year),
                            method = "rf",
                            preProcess = c("scale"))
    
    
    test$Predictions_RF1 <- predict(fit_RF1, test)
    postResample(test$Predictions_RF1, test$GAP)
    
    #Check important variables
    varTun2 <- varImp(fit_RF1)
    plot(varTun2, main = "Top variance importance")
    
    
    #Plotting
    test$Date<-as_date(test$Date)
    train$Date<-as_date(train$Date)
    
    
    ggplot(test, aes(x=Date)) + scale_x_date(labels = date_format("%m-%Y"))+
      geom_line(aes(y=GAP), color="blue", linetype= "dashed") + geom_line(aes(y=Predictions_RF1), color="red")+
      geom_line(data=train, aes(x=Date, y=GAP)) 
    
  
   