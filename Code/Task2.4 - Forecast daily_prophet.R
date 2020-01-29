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
Sys.setlocale("LC_TIME", "English") #If we dont put this, we will have some things in spanish
setwd("C:/Users/Ariadna/Downloads/UBIQUM COURSE/3.IoT Analytics/Task2")

#### ---- Importing the Data ----

    EnergyConsumption <- readRDS(file = './Input/EnergyConsumption_External_4years.RDS')

#### ---- Daily Time Series Creation and Analysis -----
  
  #Create Daily Observations, and add the Date (for future Prophet)

    Group_Daily <- EnergyConsumption %>% 
      group_by(Year, Month, Day) %>% 
      summarise(Kitchen=round(sum(Kitchen/1000),3),
                          Laundry=round(sum(Laundry/1000),3),
                          Heater_AC=round(sum(Heater_AC/1000),3),
                          Others=round(sum(Others/1000),3),
                          GAP=round(sum(GAP/1000),3),
                          Temperature=mean(Temperature),
                          Cost=round(sum(Cost),3)) %>% 
      mutate(Date = as_date(paste(Year, Month, Day, sep="-"), "%Y-%m-%d"))
      
  #Reorder and Variables Selection
    summary(Group_Daily) #We have different observations per day, we need to take into account
    names(Group_Daily)
    Group_Daily<- subset(Group_Daily, 
                           select=c(Date, Kitchen: Cost))

    ## ---- Prophet ----
      
      #Test and Train Set creation in Prophet format 
      
        train_prophet <- Group_Daily %>% 
          filter(year(Date) != 2010) %>% 
          select(Date, GAP)
        colnames(train_prophet)[1]<-"ds"
        colnames(train_prophet)[2]<-"y"
        
        test_prophet <- Group_Daily %>% 
          filter(year(Date) == 2010) %>% 
          select(Date, GAP)
        colnames(test_prophet)[1]<-"ds"
        colnames(test_prophet)[2]<-"y"
      
        #Prophet modelling
        
      prophet_fit <- prophet(train_prophet, daily.seasonality = T)
      future <- make_future_dataframe(prophet_fit, periods = 330)
      
      prediction <- predict(prophet_fit, future)
      plot(prediction$yhat)
      
      plot(prophet_fit, prediction)
      prophet_plot_components(prophet_fit, prediction)
      
        
      test<-mape(test_prophet$y, prediction$yhat)
      
      #Plotting
      
      prediction$ds<-as_date(prediction$ds)
      train_prophet$ds<-as_date(train_prophet$ds)
      test_prophet$ds<-as_date(test_prophet$ds)
      
      ggplot() + 
        geom_line(data = prediction, aes(x = ds, y = yhat, color = "blue")) +
        geom_line(data = train_prophet, aes(x = ds, y = y, color = "red"), linetype="dashed") +
        geom_line(data = test_prophet, aes(x = ds, y = y, color = "green")) +
        xlab('Dates') +
        ylab('percent.change')+
        scale_colour_manual(name = 'Series', 
                            values =c('green'='green','red'='red', 'blue'='blue'), labels = c('Prophet','Train Set','Test Set'))
      

      #Cross Validation
      
      df.cv <- cross_validation(prophet_fit, 
                                initial = 500, period = 180, 
                                horizon = 1, units = 'days')
      head(df.cv)
      
      df.p <- performance_metrics(df.cv)
      head(df.p)

      plot_cross_validation_metric(df.cv, metric = 'mape')
      
      #Calculate overal MAPE:
      
      test<-mape(df.cv$y, df.cv$yhat)
      

      
