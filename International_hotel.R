library(readxl)
International_hotel <- read_excel("G:/ExcelR Project/International_hotel.xlsx", 
                                  col_types = c("date", "numeric"))
View(International_hotel)

International_hotel$ds <- as.Date(International_hotel$ds)
library(ggplot2)
(time_plot <- ggplot(International_hotel, aes(x = ds, y = y)) +
    geom_line() +
    scale_x_date(date_labels = "%m", date_breaks = "1 week") +
    theme_classic())
library(forecast)
International_hotel$y <- tsclean(International_hotel$y)
(time_plot <- ggplot(International_hotel, aes(x = ds, y = y)) +
    geom_line() +
    scale_x_date(date_labels = "%m", date_breaks = "1 week") +
    theme_classic())
write.csv(International_hotel,"InternationalHotel.csv")


train <-International_hotel[1:56,]
test <- International_hotel[57:63,]

library(rlang)
library(Rcpp)
library(prophet)
m <- prophet(train,weekly.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 7,freq = 604800 )
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m,forecast)

prophet_plot_components(m, forecast)

a <- forecast$yhat[57:63]
b <- test$y
rmse<-sqrt(mean((b-a)^2,na.rm = T)) # 47688.048
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

100-mape(b,a) # 79.35



library(tibble)
library(tidyverse)
library(anomalize)



Internationalh <- International_hotel     




international_ts <- Internationalh[,c(2,1)]



international_ts %>% time_decompose(y, method = "twitter", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition()



international_ts %>% 
  time_decompose(y) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)


inter<- international_ts %>% 
  time_decompose(y) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes') 

inter1 <-  international_ts %>% 
  time_decompose(y) %>%
  anomalize(remainder) %>%
  time_recompose()


Internationalh$anomaly <- inter1$anomaly

Internationalh$y <- ifelse(Internationalh$anomaly == 'Yes',(inter1$recomposed_l2-inter1$remainder_l2),Internationalh$y)

train1 <-Internationalh[1:56,]
test1 <- Internationalh[57:63,]
m1 <- prophet(train1,weekly.seasonality=TRUE)
future1 <- make_future_dataframe(m1, periods = 7,freq = 604800 )
tail(future1)
forecast1 <- predict(m1, future1)
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m1,forecast1)

prophet_plot_components(m1, forecast1)

a1 <- forecast1$yhat[57:63]
b1 <- test1$y
rmse<-sqrt(mean((b1-a1)^2,na.rm = T)) # 68799
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

100-mape(b1,a1) # 79

model <- auto.arima(train[,2])
fcast <- forecast(model,h=7)
plot(fcast)
c <- fcast$mean
100-mape(b,c) # 78.43
rmse<-sqrt(mean((c-a)^2,na.rm = T)) # 18526.55

