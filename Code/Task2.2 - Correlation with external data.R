#### ---- Libraries ----
library(dplyr)
library(lubridate) #To deal with TimeSeries
library(zoo) #As yearmon
library(tidyverse) 
library(magrittr) # To add %<>% operator
library(ggcorrplot) #For the correlation matrix as a heatmap
library(corrr)

Sys.setlocale("LC_TIME", "English") #If we dont put this, we will have some things in spanish
setwd("C:/Users/Ariadna/Downloads/UBIQUM COURSE/3.IoT Analytics/Task2")


#### ---- Importing Data ----

  EnergyConsumption <- readRDS(file = './Output/EnergyConsumption_External.RDS')
  
#### ---- Correlation with Temperature ----
  
  names(EnergyConsumption)
  
  corrData<-EnergyConsumption %>% select (GAP, Others, Kitchen,Laundry, Heater_AC, Temperature)

  correlations<-cor(corrData)
  summary(correlations)
  ggcorrplot(correlations, lab=TRUE, outline.col = "white", title="Correlation Matrix")  #method="circle" when possible is better the visualization

  ##Grouping by Seasons
  
  Group_Seasons <- EnergyConsumption %>% group_by(Seasons) %>%
    summarise(
              Mean_GAP=mean(GAP),
              Mean_Others=mean(Others),
              Mean_Kitchen=mean(Kitchen),
              Mean_Laundry=mean(Laundry),
              Mean_Heater_AC=mean(Heater_AC),
              Mean_Temperature=mean(Temperature))
  
  corr_GroupData <- Group_Seasons%>% select(-Seasons)
  correlations_group<-cor(corr_GroupData)
  summary(correlations_group)
  ggcorrplot(correlations_group, lab=TRUE, outline.col = "white", title="Correlation Matrix")  #method="circle" when possible is better the visualization
  
  ##Grouping by Month
  
  Group_Month <- EnergyConsumption %>% group_by(Month) %>%
    summarise(
      Mean_GAP=mean(GAP),
      Mean_Others=mean(Others),
      Mean_Kitchen=mean(Kitchen),
      Mean_Laundry=mean(Laundry),
      Mean_Heater_AC=mean(Heater_AC),
      Mean_Temperature=mean(Temperature))
  
  corr_GroupData_Month <- Group_Month%>% select(-Month)
  correlations_group_month<-cor(corr_GroupData_Month)
  summary(correlations_group_month)
  ggcorrplot(correlations_group_month, lab=TRUE, outline.col = "white", title="Correlation Matrix")  #method="circle" when possible is better the visualization
  
#### ---- Correlation with Corrr Package ----
  #All the infi: https://www.datanovia.com/en/blog/easy-correlation-matrix-analysis-in-r-using-corrr-package/
  res.cor <- correlate(corrData)
  res.cor

#### ---- Anova test for Sunrise and Sunset ----
  
  # I do not why it does not store the proper numbers....
  a <- c("GAP ~ Light","Others ~ Light", "Kitchen ~ Light", "Laundry ~ Light", "Heater_AC ~ Light" )
  compare_models <- c()
  
  for ( i in a) {
      anova_test <- aov(formula(i), data = EnergyConsumption)
      data<-as.data.frame((anova(anova_test)))
      compare_models<-rbind(compare_models,data)
      
  }

  b <- c("GAP","GAP_res", "Others","Others_res", "Kitchen", "Kitchen_res",
         "Laundry","Laundry_res", "Heater_AC", "Heater_AC_res" )
  
  row.names(compare_models)<-b

  # Manually
  anova_GAP_Light <- aov(GAP~Light, data = EnergyConsumption)
  summary(anova_GAP_Light)
  
  anova_Others_Light <- aov(Others~Light, data = EnergyConsumption)
  summary(anova_Others_Light)
  
  anova_Kitchen_Light <- aov(Kitchen~Light, data = EnergyConsumption)
  summary(anova_Kitchen_Light)
  
  anova_Laundry_Light <- aov(Laundry~Light, data = EnergyConsumption)
  summary(anova_Laundry_Light)
  
  anova_Heater_Light <- aov(Heater_AC~Light, data = EnergyConsumption)
  summary(anova_Heater_Light)
  
  