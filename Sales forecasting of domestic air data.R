
#######################loading data using readxl ########################

library(readxl)
Domesticfinal <- read_excel("G:/ExcelR Project/Domesticfinal.xlsx", 
                            col_types = c("date", "numeric"))
head(Domesticfinal)


############### Performing EDA on Air Data ###########################

str(Domesticfinal)

Domesticfinal$ds <- as.Date(Domesticfinal$ds) 

library(ggplot2)
(time_plot <- ggplot(Domesticfinal, aes(x = ds, y = y)) +
    geom_line() +
    scale_x_date(date_labels = "%m", date_breaks = "1 month") +
    theme_classic())
ggplot(data = Domesticfinal, aes(x = ds, y = y))+
  geom_line(color = "#00AFBB", size = 1)



###########################################################################


################## splitting into train and test for default dataset ###################




train<-Domesticfinal[1:425,]

test<-Domesticfinal[426:436,]




############## Using Prophet Package for Model Building ####################################


library(rlang)
library(Rcpp)
library(prophet)
m <- prophet(train,daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 11)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m,forecast)

prophet_plot_components(m, forecast)

##################### Analysis of Result with error rate and Accuracy#########################
 a <- forecast$yhat[426:436]
b <- test$y
rmse<-sqrt(mean((b-a)^2,na.rm = T)) # 494442.476
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

100-mape(b,a)
####################### Using Auto Arima for Model Building ####################################
library(forecast)

model <- auto.arima(train[,2])
fcast <- forecast(model,h=11)
plot(fcast)
c <- fcast$mean
100-mape(b,c) # 54.8327 Accuracy




############################### Anomalies detection and replacement ##############################################
library(tibble)
library(tidyverse)
library(anomalize)



Domestic <- Domesticfinal     


#Run this code three ties to remove all outliers and write csv file for next use

domestic_ts <- Domestic[,c(2,1)]



domestic_ts %>% time_decompose(y, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition()



domestic_ts %>% 
  time_decompose(y) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)


dom <- domestic_ts %>% 
  time_decompose(y) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes') 

dom1 <- domestic_ts %>% 
  time_decompose(y) %>%
  anomalize(remainder) %>%
  time_recompose()


Domestic$anomaly <- dom1$anomaly

Domestic$y <- ifelse(Domestic$anomaly == 'Yes',(dom1$recomposed_l2-dom1$remainder_l2),Domestic$y)

#After running above code three times saving Domestic.csv

write.csv(Domestic,"Domestic.csv")

#########################Splitting into train and test after Anomalies Detection ###############################################
library(forecast)
Domestic$y <- tsclean(Domestic$y) # replacing and removing outliers

ggplot(data = Domestic, aes(x = ds, y = y))+
  geom_line(color = "#00AFBB", size = 1)

train1<-Domestic[1:425,]

test1<-Domestic[426:436,]

############## Using Prophet Package for Model Building ####################################

m1 <- prophet(train1,daily.seasonality=TRUE)
future1 <- make_future_dataframe(m1, periods = 11)
tail(future1)
forecast1 <- predict(m1, future1)
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m1,forecast1)

prophet_plot_components(m1, forecast1)

##################### Analysis of Result with error rate and Accuracy#########################
a1 <- forecast1$yhat[426:436]
b1 <- test1$y
rmse<-sqrt(mean((b1-a1)^2,na.rm = T)) # 425650
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

100-mape(b1,a1) # 87.52

####################### Using Auto Arima for Model Building ####################################

model <- auto.arima(train1[,2])
fcast <- forecast(model,h=11)
plot(fcast)
c <- fcast$mean
100-mape(b1,c) # 87.29
