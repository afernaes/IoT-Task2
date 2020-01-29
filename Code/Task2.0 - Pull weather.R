#### ---- Libraries ----
  library(dplyr)
  library(lubridate) #To deal with TimeSeries
  library(rstudioapi) #To set manually WD, but does not work
  library(zoo) #As yearmon
  library(tidyverse) 
  library(magrittr) # To add %<>% operator
  library(stringr) #To separate factors
  library(darksky) #To pull the weather
  Sys.setenv("DARKSKY_API_KEY"= "d8e435e08a9d8184fde48fd4e394c51e")
  Sys.getenv("DARKSKY_API_KEY")
  
  Sys.setlocale("LC_TIME", "English") #If we dont put this, we will have some things in spanish
  setwd("C:/Users/Ariadna/Downloads/UBIQUM COURSE/3.IoT Analytics/Task2")

#### ---- Importing Weather Data ----

    #2007 Weather
    year2007_weather <- seq(as.Date("2007-01-01"), as.Date("2007-12-31"), by='days') %>% 
      map(~get_forecast_for(48.77, 2.28, .x)) %>% map_df("hourly")
    saveRDS(year2007_weather, './Output/year2007_weather.RDS')
    write.csv2(year2007_weather,'./Output/year2007_weather.csv', row.names = FALSE)
    
    #2008 Weather
    year2008_weather <- seq(as.Date("2007-01-01"), as.Date("2007-12-31"), by='days') %>% 
      map(~get_forecast_for(48.77, 2.28, .x)) %>% map_df("daily")
    saveRDS(year2008_weather, './Output/year2008_weather.RDS')
    write.csv2(year2008_weather,'./Output/year2008_weather.csv', row.names = FALSE)
    
    #2009 Weather
    year2009_weather <- seq(as.Date("2009-01-01"), as.Date("2009-12-31"), by='days') %>% 
      map(~get_forecast_for(48.77, 2.28, .x)) %>% map_df("hourly")
    saveRDS(year2009_weather, './Output/year2009_weather.RDS')
    write.csv2(year2009_weather,'./Output/year2009_weather.csv', row.names = FALSE)
    
    #2010 Weather
    year2010_weather <- seq(as.Date("2010-01-01"), as.Date("2010-12-31"), by='days') %>% 
      map(~get_forecast_for(48.77, 2.28, .x)) %>% map_df("hourly")
    saveRDS(year2010_weather, './Output/year2010_weather.RDS')
    write.csv2(year2010_weather,'./Output/year2010_weather.csv', row.names = FALSE)
    
    #All together
    
    year2007_weather<-readRDS(file = './Output/year2007_weather.RDS')
    year2008_weather<-readRDS(file = './Output/year2008_weather.RDS')
    year2009_weather<-readRDS(file = './Output/year2009_weather.RDS')
    year2010_weather<-readRDS(file = './Output/year2010_weather.RDS')
    
    weather_hourly<-rbind(year2007_weather,year2008_weather,year2009_weather, year2010_weather)
    saveRDS(weather_hourly, './Output/weather_hourly.RDS')
    write.csv2(weather_hourly,'./Output/weather_hourly.csv', row.names = FALSE)
    
    #2007 Weather_daily
    year2007_weather_daily <- seq(as.Date("2007-01-01"), as.Date("2007-12-31"), by='days') %>% 
      map(~get_forecast_for(48.77, 2.28, .x)) %>% map_df("daily")
    saveRDS(year2007_weather_daily, './Output/year2007_weather_daily.RDS')
    write.csv2(year2007_weather_daily,'./Output/year2007_weather_daily.csv', row.names = FALSE)
    
    #2008 Weather_daily
    year2008_weather_daily <- seq(as.Date("2008-01-01"), as.Date("2008-12-31"), by='days') %>% 
      map(~get_forecast_for(48.77, 2.28, .x)) %>% map_df("daily")
    saveRDS(year2008_weather_daily, './Output/year2008_weather_daily.RDS')
    write.csv2(year2008_weather_daily,'./Output/year2008_weather_daily.csv', row.names = FALSE)
    
    #2009 Weather_daily
    year2009_weather_daily <- seq(as.Date("2009-01-01"), as.Date("2009-12-31"), by='days') %>% 
      map(~get_forecast_for(48.77, 2.28, .x)) %>% map_df("daily")
    saveRDS(year2009_weather_daily, './Output/year2009_weather_daily.RDS')
    write.csv2(year2009_weather_daily,'./Output/year2009_weather_daily.csv', row.names = FALSE)
    
    #2010 Weather_daily
    year2010_weather_daily <- seq(as.Date("2010-01-01"), as.Date("2010-12-31"), by='days') %>% 
      map(~get_forecast_for(48.77, 2.28, .x)) %>% map_df("daily")
    saveRDS(year2010_weather_daily, './Output/year2010_weather_daily.RDS')
    write.csv2(year2010_weather_daily,'./Output/year2010_weather_daily.csv', row.names = FALSE)
    
    
    #All together
    
    year2007_weather_daily<-readRDS(file = './Output/year2007_weather_daily.RDS')
    year2008_weather_daily<-readRDS(file = './Output/year2008_weather_daily.RDS')
    year2009_weather_daily<-readRDS(file = './Output/year2009_weather_daily.RDS')
    year2010_weather_daily<-readRDS(file = './Output/year2010_weather_daily.RDS')
    
    
    
    weather_daily<-rbind(year2007_weather_daily,year2008_weather_daily,year2009_weather_daily, year2010_weather_daily)
    saveRDS(weather_daily, './Output/weather_daily.RDS')
    write.csv2(weather_daily,'./Output/weather_daily.csv', row.names = FALSE)
    