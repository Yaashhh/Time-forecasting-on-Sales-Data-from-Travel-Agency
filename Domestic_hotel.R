library(readxl)
Domestic_hotel <- read_excel("G:/ExcelR Project/Domestic_hotel.xlsx", 
                             col_types = c("date", "numeric"))
Domestic_hotel$ds <- as.Date(Domestic_hotel$ds) 
library(ggplot2)
(time_plot <- ggplot(Domestic_hotel, aes(x = ds, y = y)) +
    geom_line() +
    scale_x_date(date_labels = "%m", date_breaks = "1 week") +
    theme_classic())
library(forecast)
Domestic_hotel$y <- tsclean(Domestic_hotel$y)
(time_plot <- ggplot(Domestic_hotel, aes(x = ds, y = y)) +
    geom_line() +
    scale_x_date(date_labels = "%m", date_breaks = "1 week") +
    theme_classic())

train <- Domestic_hotel[1:56,]
test <- Domestic_hotel[57:63,]

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
rmse<-sqrt(mean((b-a)^2,na.rm = T)) # 153436.729
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

100-mape(b,a) # 90.56
