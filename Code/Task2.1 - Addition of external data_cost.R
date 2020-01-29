#### ---- Libraries ----
  library(dplyr)
  library(lubridate) #To deal with TimeSeries
  library(rstudioapi) #To set manually WD, but does not work
  library(zoo) #As yearmon
  library(tidyverse) 
  library(magrittr) # To add %<>% operator
  library(stringr) #To separate factors

  Sys.setlocale("LC_TIME", "English") #If we dont put this, we will have some things in spanish
  setwd("C:/Users/Ariadna/Downloads/UBIQUM COURSE/3.IoT Analytics/Task2")

#### ---- Importing Data ----

  EnergyConsumption <- readRDS(file = './Input/EnergyConsumptionClean_4years.RDS')
  
#### ---- Importing Electricity Cost Data -----
    
  #Data was taken from: https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nrg_pc_204&lang=en
    #assuming a medium annual consumption (Band DC)
    Electricity_Price <- read_csv('./Input/nrg_pc_204_1_Data.csv')
    glimpse(Electricity_Price) #To see the data type per attribute
    Electricity_Price$Value<-as.numeric(Electricity_Price$Value)
    Electricity_Price %<>% filter(TAX == "All taxes and levies included", 
                        CURRENCY == "Euro") %>% select(TIME, Value)
    #TO separate into two variables it needs to be a factor
    Electricity_Price$TIME<-as.factor(Electricity_Price$TIME)
    Electricity_Price<-separate(Electricity_Price,TIME, into=c("Year", "Semester"), sep="S")
      #Electricity_Price %<>% filter (Year != "2010")
    #There was one value missing
    Electricity_Price[1,3] = 0.1218
    #Put the elements of th left_join in the same type of the original dataset
    Electricity_Price$Semester<-as.integer(Electricity_Price$Semester)
    Electricity_Price$Year<-as.factor(Electricity_Price$Year)
    str(Electricity_Price)
    #Add semester to be able to deal with Electricity Price Data
    EnergyConsumption$Semester<-semester(EnergyConsumption$DateTime)
    # To join the datasets
    EnergyConsumption <- left_join(EnergyConsumption, Electricity_Price, by = c("Year", "Semester"))
    #The GAP is Wh, while the cost is in kWh
    EnergyConsumption$CostGAP <- EnergyConsumption$Value/1000 * EnergyConsumption$GAP
    EnergyConsumption$CostKitchen <- EnergyConsumption$Value/1000 * EnergyConsumption$Kitchen
    EnergyConsumption$CostLaundry <- EnergyConsumption$Value/1000 * EnergyConsumption$Laundry
    EnergyConsumption$CostHeater <- EnergyConsumption$Value/1000 * EnergyConsumption$Heater_AC
    EnergyConsumption$CostOthers <- EnergyConsumption$Value/1000 * EnergyConsumption$Others
    
    
    summary(EnergyConsumption$Others)
    
    
    #Eliminate not usefull columns
    EnergyConsumption$Semester<-NULL
    EnergyConsumption$Value<-NULL
  
#### ---- Importing Weather Data ----

    WeatherData <- readRDS(file = './Output/weather_hourly.RDS')
    
    #I need all these columns in order to be able to match the information
    #with the original dataset, otherwise I will have NA
    WeatherData %<>% select(time,temperature) %>% 
      mutate(Temperature=(temperature-32)*5/9)
    WeatherData$temperature<-NULL
    colnames(WeatherData)[1] <-"Time"
    WeatherData$Hour <- hour(WeatherData$Time)
    WeatherData$Day <- day(WeatherData$Time)
    WeatherData$Month <- month(WeatherData$Time)
    WeatherData$Year <- year(WeatherData$Time)
    
    #Data needs to be in the same way in the two datasets to be able to handle it
    levels(EnergyConsumption$Month)
    WeatherData$Month <- factor(WeatherData$Month,
                                      levels = c(1:12),
                                      labels = c("January","February","March","April",
                                                 "May", "June", "July", "August", "September",
                                                 "October", "November", "December"))
    levels(WeatherData$Month)
    WeatherData$Year<-as.factor(WeatherData$Year)
    #Left_join function
    EnergyConsumption <- left_join(EnergyConsumption, WeatherData, by = c("Year", "Month", "Day", "Hour"))
    EnergyConsumption$Time<-NULL
    
    
#### ---- Importing Sunlight Data ----
    
    SunLightData <- readRDS(file = './Output/weather_daily.RDS')

    #Select and Gather de Data
    SunLightData%<>%select(time, sunriseTime, sunsetTime) %>%
      gather('sunriseTime', 'sunsetTime', key="Light_temp", value="DateTime")
    #Remove useless variables
    SunLightData$time<-NULL
    #Add it into original dataset
    EnergyConsumption<-left_join(EnergyConsumption,SunLightData,by="DateTime")
    
    #First to factor, then to numeric and Sunrise is 1 and sunset is 2
    EnergyConsumption$Light_temp<-as.factor(EnergyConsumption$Light_temp)
    EnergyConsumption$Light_temp<-as.numeric(EnergyConsumption$Light_temp)
    summary(EnergyConsumption$Light_temp)
    names(EnergyConsumption)
    
    #na.locf is a function for replacing each NA with the most recent 
    #non-NA prior to it. However, as the first day does not have any value,
    #if I apply de function it removes the first day, if I put the first
    #observation with a non-NA, it detects it and it fills the data prior
    #to the first sunrise occurence, as it is 12pm, I will put a 2 (sunset, i.e. Dark)
    EnergyConsumption[1,21]<-2
    EnergyConsumption <- na.locf(EnergyConsumption) #It is removing the first day.....
    
    #1 is Light and 2 is Dark
    EnergyConsumption %<>%  mutate(Light = ifelse(Light_temp==1, "Light", "Dark"))
    EnergyConsumption$Light_temp<-NULL
    str(EnergyConsumption)
    #Change variable type
    EnergyConsumption$Light<-as.factor(EnergyConsumption$Light)
    levels(EnergyConsumption$Light)
    summary(EnergyConsumption$Light)
    
#### ---- Save Data with Final external variables
    
    EnergyConsumption_3years<-EnergyConsumption %>% filter (Year != 2010)
    
    EnergyConsumption_3years$Year<-factor(EnergyConsumption_3years$Year)
    str(EnergyConsumption_3years)
    
    EnergyConsumption_3years %<>% gather(key="Submeters", value= "Consumption", Kitchen, Laundry, Heater_AC, Others)
    
    EnergyConsumption_3years %<>% gather(key="SubmetersCost", value= "Cost", CostKitchen, CostLaundry, CostHeater, CostOthers)
    
    
    saveRDS(EnergyConsumption_3years, './Output/EnergyConsumption_cost.RDS')
    write.csv2(EnergyConsumption_3years,'./Output/EnergyConsumption_cost.csv', row.names = FALSE)
    
    summary(EnergyConsumption_3years$Kitchen)

    summary(EnergyConsumption_3years$CostKitchen)
    summary(EnergyConsumption_3years$CostLaundry)
  
    
    
    