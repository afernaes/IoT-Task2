#### ---- Libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_minimal())
library(zoo)
library(plotly)
library(ggplot2)
library(ggfortify) #http://www.sthda.com/english/wiki/ggfortify-extension-to-ggplot2-to-handle-some-popular-packages-r-software-and-data-visualization
library(forecast)
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


  #Time Series Creation   
    DailyTS <- ts(Group_Daily[,2:8], start = c(2007,1) , end = c(2010,330), frequency = 365)
    autoplot(DailyTS[,"GAP"])
    
    ggseasonplot(DailyTS[,"GAP"])

  #Train and Test Sets
    
    TSTrainSet<-window(DailyTS, start=c(2007,1), end=c(2009,365))
    TSTestSet<-window(DailyTS, start=c(2010,1), end=c(2010,330))
    
    autoplot(TSTrainSet[,"GAP"])+autolayer(TSTestSet[,"GAP"])
    
#### ---- MODELLING ----
    correlations<-data.frame(matrix(ncol = 0, nrow = 3))
    metrics<-c()
    
    ## ---- TBATS Model ----
  
    #with two seasonalities 7 and 365
      TBATS_TS<- msts(TSTrainSet[,"GAP"], seasonal.periods=c(7, 365))
      DailyGAP_TBATS <- tbats(TBATS_TS)
      fc_DailyGAP_TBATS <- forecast(DailyGAP_TBATS, h=330)
      autoplot(fc_DailyGAP_TBATS)
    
    #Cross Validation
    
      autoplot(fc_DailyGAP_TBATS) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
      corr<-as.data.frame(postResample(fc_DailyGAP_TBATS$mean,TSTestSet[,"GAP"]))
      ac<-as.data.frame(accuracy(fc_DailyGAP_TBATS,TSTestSet[,"GAP"]))
      correlations<-cbind(correlations, corr)
      metrics<-rbind(metrics,ac)
      row.names(metrics)[1]<-"TBATS Trainig Set"
      row.names(metrics)[2]<-"TBATS Test Set"
      colnames(correlations)[1]<-"TBATS"
   
    ## ---- SARIMAX with Fourier Terms ----
      
      #### ---- 1. Omitting Lambda ---- K=1
      
      ## Finding the best K (fourier terms) 
      bestfit1 <- list(aicc=Inf)
      for(K in seq(8)) {
        fit <- auto.arima(TSTrainSet[,"GAP"],
                          xreg=fourier(TSTrainSet[,"GAP"], K=K),
                          seasonal=F)
        if(fit[["aicc"]] < bestfit1[["aicc"]]) {
          bestfit1 <- fit
          bestK1 <- K
        }
      } 
      
      #Checking by Plotting
      plots <- list()
      for (i in seq(8)) {
        fit <- auto.arima(TSTrainSet[,"GAP"], xreg = fourier(TSTrainSet[,"GAP"], K = i),
                          seasonal = FALSE)
        plots[[i]] <- autoplot(forecast(fit,
                                        xreg=fourier(TSTestSet[,"GAP"], K=i, h=330))) +
          xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2)))
      }
      gridExtra::grid.arrange(
        plots[[1]],plots[[2]],plots[[3]],
        plots[[4]],plots[[5]],plots[[6]], plots[[7]], plots[[8]], nrow=4)
      
      #K=1 minimises the AICC, therefore, this one should be used
      #AICC is a measure of the goodness of fit and parsimony
      
      fc_DailyGAP_SARIMAX1 <- forecast(bestfit1,   
                                       xreg=fourier(TSTestSet[,"GAP"], K=bestK1, h=330)) 
  
      #Cross-Validation
      
      autoplot(fc_DailyGAP_SARIMAX1) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
      corr<-as.data.frame(postResample(fc_DailyGAP_SARIMAX1$mean,TSTestSet[,"GAP"]))
      ac<-as.data.frame(accuracy(fc_DailyGAP_SARIMAX1,TSTestSet[,"GAP"]))
      correlations<-cbind(correlations, corr)
      metrics<-rbind(metrics,ac)
      row.names(metrics)[3]<-"SARIMAX1 Trainig Set"
      row.names(metrics)[4]<-"SARIMAX1 Test Set"
      colnames(correlations)[2]<-"SARIMAX1"
    
      #### ---- 2. Lambda=0 ---- K=8
      
      ## Finding the best K (fourier terms) 
      bestfit2 <- list(aicc=Inf)
      for(K in seq(8)) {
        fit <- auto.arima(TSTrainSet[,"GAP"],
                          xreg=fourier(TSTrainSet[,"GAP"], K=K),
                          seasonal=F,lambda = 0)
        if(fit[["aicc"]] < bestfit1[["aicc"]]) {
          bestfit2 <- fit
          bestK2 <- K
        }
      } 
      
      #Checking by Plotting
      plots <- list()
      for (i in seq(8)) {
        fit <- auto.arima(TSTrainSet[,"GAP"], xreg = fourier(TSTrainSet[,"GAP"], K = i),
                          seasonal = FALSE, lambda = 0)
        plots[[i]] <- autoplot(forecast(fit,
                                        xreg=fourier(TSTestSet[,"GAP"], K=i, h=330))) +
          xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2)))
      }
      gridExtra::grid.arrange(
        plots[[1]],plots[[2]],plots[[3]],
        plots[[4]],plots[[5]],plots[[6]], plots[[7]], plots[[8]], nrow=4)
      
      #K=8 minimises the AICC, therefore, this one should be used
      #AICC is a measure of the goodness of fit and parsimony
      
      fc_DailyGAP_SARIMAX2 <- forecast(bestfit2,   
                                       xreg=fourier(TSTestSet[,"GAP"], K=bestK2, h=330)) 
      
      #Cross-Validation
      
      autoplot(fc_DailyGAP_SARIMAX2) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
      corr<-as.data.frame(postResample(fc_DailyGAP_SARIMAX2$mean,TSTestSet[,"GAP"]))
      ac<-as.data.frame(accuracy(fc_DailyGAP_SARIMAX2,TSTestSet[,"GAP"]))
      correlations<-cbind(correlations, corr)
      metrics<-rbind(metrics,ac)
      row.names(metrics)[5]<-"SARIMAX2 Trainig Set"
      row.names(metrics)[6]<-"SARIMAX2 Test Set"
      colnames(correlations)[3]<-"SARIMAX2"
      
      #### ---- 2. Lambda=auto ---- K=6
      
      ## Finding the best K (fourier terms) 
      bestfit3 <- list(aicc=Inf)
      for(K in seq(8)) {
        fit <- auto.arima(TSTrainSet[,"GAP"],
                          xreg=fourier(TSTrainSet[,"GAP"], K=K),
                          seasonal=F,lambda = "auto")
        if(fit[["aicc"]] < bestfit3[["aicc"]]) {
          bestfit3 <- fit
          bestK3 <- K
        }
      } 
      
      checkresiduals(bestfit3)
      
      #Checking by Plotting
      plots <- list()
      for (i in seq(8)) {
        fit <- auto.arima(TSTrainSet[,"GAP"], xreg = fourier(TSTrainSet[,"GAP"], K = i),
                          seasonal = FALSE, lambda = "auto")
        plots[[i]] <- autoplot(forecast(fit,
                                        xreg=fourier(TSTestSet[,"GAP"], K=i, h=330))) +
          xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2)))
      }
      gridExtra::grid.arrange(
        plots[[1]],plots[[2]],plots[[3]],
        plots[[4]],plots[[5]],plots[[6]], plots[[7]], plots[[8]], nrow=4)
      
      #K=6 minimises the AICC, therefore, this one should be used
      #AICC is a measure of the goodness of fit and parsimony
      
      fc_DailyGAP_SARIMAX3 <- forecast(bestfit3,   
                                       xreg=fourier(TSTestSet[,"GAP"], K=bestK3, h=330)) 
      
      #Cross-Validation
      
      autoplot(fc_DailyGAP_SARIMAX3) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
      corr<-as.data.frame(postResample(fc_DailyGAP_SARIMAX3$mean,TSTestSet[,"GAP"]))
      ac<-as.data.frame(accuracy(fc_DailyGAP_SARIMAX3,TSTestSet[,"GAP"]))
      correlations<-cbind(correlations, corr)
      metrics<-rbind(metrics,ac)
      row.names(metrics)[7]<-"SARIMAX3 Trainig Set"
      row.names(metrics)[8]<-"SARIMAX3 Test Set"
      colnames(correlations)[4]<-"SARIMAX3"
      
      
      #### ---- Saving and plotting all the data ----
      
      saveRDS(correlations, './Output/Daily_PostResample.RDS')
      write.csv2(correlations,'./Output/Daily_PostResample.csv', row.names = TRUE)
      
      saveRDS(metrics, './Output/Daily_metrics.RDS')
      write.csv2(metrics,'./Output/Daily_metrics.csv', row.names = TRUE)
      
      autoplot(TSTrainSet[,"GAP"]) +
        autolayer(fc_DailyGAP_TBATS$mean, series="TBATS") +
        autolayer(fc_DailyGAP_SARIMAX1$mean, series="ARIMAX K=1") +
        autolayer(fc_DailyGAP_SARIMAX1$mean, series="ARIMAX K=8") +
        autolayer(fc_DailyGAP_SARIMAX1$mean, series="ARIMAX K=6") +
        ggtitle("Forecasts for Daily Energy Consumption") + theme(plot.title = element_text(hjust = 0.5))+
        xlab("Year") + 
        ylab("kWh") +
        guides(colour=guide_legend(title="Forecast"))
      
      

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
      
      Metrics :: mape (prediction$yhat, train_prophet$y)
      
      plot(prophet_fit, prediction)
      prophet_plot_components(prophet_fit, prediction)
      
      #Cross Validation
      
      df.cv <- cross_validation(prophet_fit, 
                                initial = 300, period = 180, 
                                horizon = 365, units = 'days')
      head(df.cv)
      
      df.p <- performance_metrics(df.cv)
      head(df.p)

      plot_cross_validation_metric(df.cv, metric = 'mape')
      
      #Calculate overal MAPE:
      
      test<- Metrics ::mape(df.cv$y, df.cv$yhat)
      

      
      #### ---- MISCELLANEOUS ----

    #Testing things
    
    TSTrainSet[,"Temperature"] %>% mstl() %>%
      autoplot() + xlab("Week")
    
    TSTrainSet[,"GAP"] %>%  stlf() %>%
      autoplot() + xlab("Week")

    #Testing Frourier
    
    #Testing ARIMA with fourier
    
    plots <- list()
    for (i in seq(6)) {
      fit <- auto.arima(TSTrainSet[,"GAP"], xreg = fourier(TSTrainSet[,"GAP"], K = i),
                        seasonal = FALSE, lambda = 0)
      plots[[i]] <- autoplot(forecast(fit,
                                      xreg=fourier(TSTrainSet[,"GAP"], K=i, h=11))) +
        xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2)))
    }
    gridExtra::grid.arrange(
      plots[[1]],plots[[2]],plots[[3]],
      plots[[4]],plots[[5]],plots[[6]], nrow=3)
    

    