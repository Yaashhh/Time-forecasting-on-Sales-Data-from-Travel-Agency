library(readr)
sales <- read.csv(file.choose())

head(sales)
str(sales)
class(sales$InvoiceDate)

# Coerce to `Date` class
sales$InvoiceDate <- as.Date(sales$InvoiceDate, format = "%d-%m-%Y")

class(sales$InvoiceDate)
head(sales)

sales <- sales[do.call(order, sales), ]

sales$NetFare <- as.numeric(levels(sales$NetFare))[sales$NetFare]
row.names(sales) <- NULL

summary(sales_domestic_hotel$ProductType)

sales_domestic <- subset(sales,ItineraryType == "Domestic")
sales_domestic_air <- subset(sales_domestic,ProductType%in%c("Air","Air Cancellation","Air Loss"))
sales_domestic_air$ProductType <- factor(sales_domestic_air$ProductType)
sales_domestic_air$ItineraryType <- factor(sales_domestic_air$ItineraryType)
sales_domestic_air$year <- format(sales_domestic_air$InvoiceDate, format = "%Y")
sales_domestic_air$month <- format(sales_domestic_air$InvoiceDate, format = "%m")
write.csv(sales_domestic_air,"domestic_air.csv")

sales_domestic_hotel <- subset(sales_domestic,ProductType%in%c("Hotel","Hotel Cancellation","Hotel Loss"))
sales_domestic_hotel$ProductType <- factor(sales_domestic_hotel$ProductType)
sales_domestic_hotel$ItineraryType <- factor(sales_domestic_hotel$ItineraryType)
write.csv(sales_domestic_hotel,"domestic_hotel.csv")



sales_International <- subset(sales,ItineraryType == "International")
sales_International_air <- subset(sales_International,ProductType %in% c("Air","Air Cancellation","Air Loss"))
sales_International_air$ProductType <- factor(sales_International_air$ProductType)
sales_International_air$ItineraryType <- factor(sales_International_air$ItineraryType)
write.csv(sales_International_air,"international air.csv")
sales_International_hotel <- subset(sales_International,ProductType %in% c("Hotel","Hotel Cancellation","Hotel Loss"))
sales_International_hotel$ProductType <- factor(sales_International_hotel$ProductType)
sales_International_hotel$ItineraryType <- factor(sales_International_hotel$ItineraryType)
write.csv(sales_International_hotel,"International hotel.csv")

memory.size()
gc()















library(ggplot2)

ggplot(data = sales, aes(x = InvoiceDate, y = NetFare))+
  geom_line(color = "#00AFBB", size = 1)

sales_International$Year <- format(sales_International$ds, format = "%Y")
sales_International$month_num <- format(sales_International$ds, format = "%m")
sales_International$date_num <- format(sales_International$ds, format = "%d")













