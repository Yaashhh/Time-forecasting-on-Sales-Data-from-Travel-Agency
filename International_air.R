library(readxl)
International_air <- read_excel("G:/ExcelR Project/International_air.xlsx", 
                                col_types = c("date", "numeric"))
View(International_air)
 International_air$ds <- as.Date(International_air$ds)
 library(ggplot2)
 (time_plot <- ggplot(International_air, aes(x = ds, y = y)) +
     geom_line() +
     scale_x_date(date_labels = "%m", date_breaks = "1 week") +
     theme_classic())
 library(forecast)
International_air$y <- tsclean(International_air$y)
(time_plot <- ggplot(International_air, aes(x = ds, y = y)) +
    geom_line() +
    scale_x_date(date_labels = "%m", date_breaks = "1 week") +
    theme_classic())

library(tibble)
library(tidyverse)
library(anomalize)



InternationalA <- International_air     




international_ts <- InternationalA[,c(2,1)]



international_ts %>% time_decompose(y, method = "stl", frequency = "auto", trend = "auto") %>%
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


InternationalA$anomaly <- inter1$anomaly

InternationalA$y <- ifelse(InternationalA$anomaly == 'Yes',(inter1$recomposed_l2-inter1$remainder_l2),InternationalA$y)


(time_plot <- ggplot(InternationalA, aes(x = ds, y = y)) +
    geom_line() +
    scale_x_date(date_labels = "%m", date_breaks = "1 week") +
    theme_classic())

InternationalA$y <- tsclean(InternationalA$y)
InternationalA$ds <- as.Date(InternationalA$ds)

write.csv(InternationalA,"InternationalAir.csv")




train <-International_air[1:56,]
test <- International_air[57:63,]

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
str(forecast1)
a <- forecast$yhat[57:63]
b <- test$y
rmse<-sqrt(mean((b-a)^2,na.rm = T)) 
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

100-mape(b,a) # 38



train1 <-InternationalA[1:60,]
test1 <- InternationalA[61:63,]


m1 <- prophet(train1,weekly.seasonality=TRUE)
future1 <- make_future_dataframe(m1, periods = 7,freq = 604800 )
tail(future1)
forecast1 <- predict(m1, future1)
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m1,forecast1)

prophet_plot_components(m1, forecast1)

a <- forecast1$yhat[61:63]
b <- test1$y
rmse<-sqrt(mean((b-a)^2,na.rm = T)) # 846267.679
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

100-mape(b,a) # 88.779

model <- auto.arima(train1[,2])
fcast <- forecast(model,h=7)
plot(fcast)
c <- fcast$mean
100-mape(b,c) # 76.07
rmse<-sqrt(mean((c-a)^2,na.rm = T)) 













