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
library(fpp2)
Sys.setlocale("LC_TIME", "English") #If we dont put this, we will have some things in spanish
setwd("C:/Users/Ariadna/Downloads/UBIQUM COURSE/3.IoT Analytics/Task2")



#### ---- Functions ----

# Function to determine if a ts is stationary or not using the adf.test
station <- function(ts) {
  if (adf.test(ts)$p.value > 0.05) {
    print("The time series is not stationary, the ADF test has not been passed")
    sprintf("The p-value is: %5.3f",adf.test(ts)$p.value)
    ggAcf(ts)
  } else {
    print("This time series is stationary, the ADF test has been passed")
    ggAcf(ts)
  }
}

#### ---- Importing the Data ----

EnergyConsumption <- readRDS(file = './Input/EnergyConsumption_External_4years.RDS')

#### ---- Monthly Time Series Creation and Analysis -----

  ##Subset data by month and summarise total energy usage. 
  #In kWh and 3 decimal numbers
    Group_month <- EnergyConsumption %>%
      group_by(Year, Month) %>%
      summarise(Kitchen=round(sum(Kitchen/1000),3),
                Laundry=round(sum(Laundry/1000),3),
                Heater_AC=round(sum(Heater_AC/1000),3),
                Others=round(sum(Others/1000),3),
                GAP=round(sum(GAP/1000),3),
                Temperature=mean(Temperature),
                Cost=round(sum(Cost),3))


  ##Create monthly time series
    names(Group_month)
    MonthlyTS <- ts(Group_month[,3:9],
                    frequency = 12,
                    start=c(2007,1),
                    end=c(2010,11))
    
    #Test and Train Sets Creation
    
      TSTrainSet<-window(MonthlyTS, start=c(2007,1), end=c(2009,12))
      TSTestSet<-window(MonthlyTS, start=c(2010,1), end=c(2010,11))
      
      autoplot(TSTrainSet)+autolayer(TSTestSet)
    
    #Correlations
      #Relationship between temperature and GAP
      qplot(Temperature, GAP, data=as.data.frame(MonthlyTS))
      #Correlations
      GGally::ggpairs(as.data.frame(MonthlyTS))
      
    #Checking Seasonality and Trend
      autoplot(stl(MonthlyTS[,"GAP"], s.window = "periodic"), ts.colour="blue")    
      
      MonthlyTS[,"GAP"] %>% seas(x11="") -> x11adj
      autoplot(x11adj)
      
      ggseasonplot(MonthlyTS[,"GAP"])
      
    #Autocorrelation: Look at the two notations
      autoplot(acf(MonthlyTS, plot = FALSE))
      
    ##Partial Autocorrelation: Clear Seasonality on GAP
      autoplot(pacf(MonthlyTS[,"GAP"], plot = FALSE))

#### ---- MODELLING -----
      correlations<-data.frame(matrix(ncol = 0, nrow = 3))
      metrics<-c()
  ## ---- Linear Model ----
      # Linear model with TREND and SEASON
      
        MonthlyGAP_LM1<- tslm(TSTrainSet[,"GAP"] ~ trend + season)
        summary(MonthlyGAP_LM1)
        glance(MonthlyGAP_LM1)
        checkresiduals(MonthlyGAP_LM1)
        autoplot(MonthlyGAP_LM1$x) + autolayer(MonthlyGAP_LM1$fitted.values, series="Simple linear model")
        
        fc_MonthlyGAP_LM1 <- forecast(MonthlyGAP_LM1,h=11, level=c(80,95))
        autoplot(fc_MonthlyGAP_LM1)
        autoplot(acf(fc_MonthlyGAP_LM1, plot = FALSE))
        
        #Cross Validation
        
        autoplot(fc_MonthlyGAP_LM1) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
        corr<-as.data.frame(postResample(fc_MonthlyGAP_LM1$mean,TSTestSet[,"GAP"]))
        ac<-as.data.frame(accuracy(fc_MonthlyGAP_LM1,TSTestSet[,"GAP"]))
        correlations<-cbind(correlations, corr)
        metrics<-rbind(metrics,ac)
        row.names(metrics)[1]<-"LM Trainig Set"
        row.names(metrics)[2]<-"LM Test Set"
        colnames(correlations)[1]<-"Linear Model"

  ## ---- Holt Winters
        
      #Seasonal Holt Winters
       
        MonthlyGAP_HW1 <- ets(TSTrainSet[,"GAP"])
        autoplot(MonthlyGAP_HW1$x)+ autolayer(MonthlyGAP_HW1$fitted, series="Holt Winters")
        checkresiduals(MonthlyGAP_HW1)
        
        fc_MonthlyGAP_HW1 <- forecast(MonthlyGAP_HW1, h = 11, level=c(80,95))
        autoplot(fc_MonthlyGAP_HW1)
        checkresiduals(fc_MonthlyGAP_HW1)
        accuracy(fc_MonthlyGAP_HW1)
        
        #Cross Validation
        
        autoplot(fc_MonthlyGAP_HW1) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
        corr<-as.data.frame(postResample(fc_MonthlyGAP_HW1$mean,TSTestSet[,"GAP"]))
        ac<-as.data.frame(accuracy(fc_MonthlyGAP_HW1,TSTestSet[,"GAP"]))
        correlations<-cbind(correlations, corr)
        metrics<-rbind(metrics,ac)
        row.names(metrics)[3]<-"HW Trainig Set"
        row.names(metrics)[4]<-"HW Test Set"
        colnames(correlations)[2]<-"Holt Winters"
        
  ## ---- Arima ----
      ### ---- Automatic SARIMA ----
        MonthlyGAP_AR1<-auto.arima(TSTrainSet[,"GAP"])
        MonthlyGAP_AR1
        autoplot(MonthlyGAP_AR1$x) + autolayer(MonthlyGAP_AR1$fitted, series= "SARIMA")
      
        fc_MonthlyGAP_AR1 <- forecast(MonthlyGAP_AR1, h = 11, level=c(80,95))
        autoplot(fc_MonthlyGAP_AR1)
        checkresiduals(fc_MonthlyGAP_AR1)
        accuracy(fc_MonthlyGAP_AR1)
        
        #Cross Validation
        
        autoplot(fc_MonthlyGAP_AR1) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
        corr<-as.data.frame(postResample(fc_MonthlyGAP_AR1$mean,TSTestSet[,"GAP"]))
        ac<-as.data.frame(accuracy(fc_MonthlyGAP_AR1,TSTestSet[,"GAP"]))
        correlations<-cbind(correlations, corr)
        metrics<-rbind(metrics,ac)
        row.names(metrics)[5]<-"AutoSAR Trainig Set"
        row.names(metrics)[6]<-"AutoSAR Test Set"
        colnames(correlations)[3]<-"Auto SARIMA"
        
      ### ---- ARIMAX ----
        MonthlyGAP_AR2<-auto.arima(TSTrainSet[,"GAP"], xreg=TSTrainSet[,"Temperature"])
        MonthlyGAP_AR2
        autoplot(MonthlyGAP_AR2$x) + autolayer(MonthlyGAP_AR2$fitted, series= "ARIMAX")
        
        fc_MonthlyGAP_AR2 <- forecast(MonthlyGAP_AR2, h = 11, level=c(80,95), xreg=TSTestSet[,"Temperature"])
        autoplot(fc_MonthlyGAP_AR2)
        checkresiduals(fc_MonthlyGAP_AR2)
        accuracy(fc_MonthlyGAP_AR2)
        
        #Cross Validation
        
        autoplot(fc_MonthlyGAP_AR2) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
        corr<-as.data.frame(postResample(fc_MonthlyGAP_AR2$mean,TSTestSet[,"GAP"]))
        ac<-as.data.frame(accuracy(fc_MonthlyGAP_AR2,TSTestSet[,"GAP"]))
        correlations<-cbind(correlations, corr)
        metrics<-rbind(metrics,ac)
        row.names(metrics)[7]<-"AutoARX Trainig Set"
        row.names(metrics)[8]<-"AutoARX Test Set"
        colnames(correlations)[4]<-"Auto ARIMAX"
        
      ### ---- Manual ARIMA ----
        #p,q and d selection
        station(MonthlyTS[,"GAP"])
        autoplot(acf(MonthlyTS[,"GAP"], plot = FALSE)) #p=1
        autoplot(pacf(MonthlyTS[,"GAP"], plot = FALSE)) #q=1
        
        
        MonthlyGAP_AR3<-arima(TSTrainSet[,"GAP"],
                              order=c(1,0,1), seasonal = list(order = c(1,1,0), period = 12))
        checkresiduals(MonthlyGAP_AR3)
        
        fc_MonthlyGAP_AR3 <- forecast(MonthlyGAP_AR3, h = 11, level=c(80,95))
        autoplot(fc_MonthlyGAP_AR3)
        checkresiduals(fc_MonthlyGAP_AR3)
        accuracy(fc_MonthlyGAP_AR3)
        
        #Cross Validation
        
        autoplot(fc_MonthlyGAP_AR3) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
        corr<-as.data.frame(postResample(fc_MonthlyGAP_AR3$mean,TSTestSet[,"GAP"]))
        ac<-as.data.frame(accuracy(fc_MonthlyGAP_AR3,TSTestSet[,"GAP"]))
        correlations<-cbind(correlations, corr)
        metrics<-rbind(metrics,ac)
        row.names(metrics)[9]<-"ManualAR Trainig Set"
        row.names(metrics)[10]<-"ManualAR Test Set"
        colnames(correlations)[5]<-"Manual ARIMA"
        
  ## ---- Seasonal Naive ----
        fc_MonthlyGAP_SN <- snaive(TSTrainSet[,"GAP"], h = 11, level=c(80,95))
        autoplot(fc_MonthlyGAP_SN)
        checkresiduals(fc_MonthlyGAP_SN)
        accuracy(fc_MonthlyGAP_SN)
        
        #Cross Validation
        
        autoplot(fc_MonthlyGAP_SN) + autolayer(TSTestSet[,"GAP"], series="Test Set Values")
        corr<-as.data.frame(postResample(fc_MonthlyGAP_SN$mean,TSTestSet[,"GAP"]))
        ac<-as.data.frame(accuracy(fc_MonthlyGAP_SN,TSTestSet[,"GAP"]))
        correlations<-cbind(correlations, corr)
        metrics<-rbind(metrics,ac)
        row.names(metrics)[11]<-"SNaive Trainig Set"
        row.names(metrics)[12]<-"SNaive Test Set"
        colnames(correlations)[6]<-"Seasonal Naive"
        
  ## ---- Plotting all the models and Saving Data ----
        
        autoplot(TSTrainSet[,"GAP"]) +
          autolayer(fc_MonthlyGAP_LM1$mean, series="Linear Model") +
          autolayer(fc_MonthlyGAP_HW1$mean, series="Holt Winters") +
          autolayer(fc_MonthlyGAP_AR1$mean, series="SARIMA") +
          autolayer(fc_MonthlyGAP_AR2$mean, series="ARIMAX") +
          autolayer(fc_MonthlyGAP_AR3$mean, series="Manual SARIMA") +
          autolayer(fc_MonthlyGAP_SN$mean, series="Seasonal Naive") +
          autolayer(TSTestSet[,"GAP"], series="Real Value", size=2) +
          ggtitle("Forecasts for monthly Energy Consumption") +
          xlab("Year") + 
          ylab("kWh") +
          guides(colour=guide_legend(title="Forecast"))

        #Saving all the predicions
        predictions<-Group_month %>% filter(Year==2010) %>% select (Year, Month, GAP) %>% ungroup()
        
        str(predictions)
        predictions_LM1<-as.data.frame(fc_MonthlyGAP_LM1$mean)
        predictions_HW1<-as.data.frame(fc_MonthlyGAP_HW1$mean)
        predictions_AR1<-as.data.frame(fc_MonthlyGAP_AR1$mean)
        predictions_AR2<-as.data.frame(fc_MonthlyGAP_AR2$mean)
        predictions_AR3<-as.data.frame(fc_MonthlyGAP_AR3$mean)
        measures<-cbind(predictions_LM1,predictions_HW1, 
                        predictions_AR1,predictions_AR2,predictions_AR3)
        colnames(measures)<-c("Linear Model", "HoltWinters", "Auto SARIMA", "ARIMAX", "Manual SARIMA")
        predictions<-cbind(predictions,measures)
        
        
        saveRDS(predictions, './Output/monthly_predictions.RDS')
        write.csv2(predictions,'./Output/monthly_predictions.csv', row.names = FALSE)
        
        
        saveRDS(correlations, './Output/Monthly_PostResample.RDS')
        write.csv2(correlations,'./Output/Monthly_PostResample.csv', row.names = TRUE)
        
        saveRDS(metrics, './Output/Monthly_metrics.RDS')
        write.csv2(metrics,'./Output/Monthly_metrics.csv', row.names = TRUE)
        
  #### ---- MISCELLANEOUS ----
        
        #Testing Cross Validations
        far <- function(x, h){
          forecast(arima(x, order=c(1,1,1),
                         seasonal = list(order = c(1,1,0), period = 12),
                         method="ML"), h=h)}
        
        
        e <- tsCV(TSTrainSet[,"GAP"], far , h=1)
    
        
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

        #K=2 minimises the AICC, therefore, this one should be used
        #AICC is a measure of the goodness of fit and parsimony