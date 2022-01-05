#Setting working directory
setwd("C:/Shawn Desai/TSoM - September 2021/Data Handling and Decision-Making/Part 3 - R-Course/Day 29 - Logistic Reg. & Exponential Smoothing")

#Reading the data
sales <- read.csv("sales.csv")
View(sales) # this is monthly sales. And we want to predict the sales for the next 3 months

#What kind of data we have
plot(sales$sales,type="l")


#Create time series from the input data, [,1] is for first column and all rows. freq = 12, is for 12 months. For quarters it will be freq = 4
sales <- ts(data=sales,start=1995,freq=12)

#Let's view, what is the output
sales

#Plot Time Series
plot(sales)

#Divides into Seasonal, Trend and Remainder. S.Window controls how rapidly the seasonal component can change
#STL means Seasonal Trend Decomposition using Loess
install.packages("fpp2")
library(fpp2)
decom <- stl(sales, s.window= "periodic",robust = TRUE)
decom

# Plot sales time series into three components - seasonal, trend and remainder
plot(decom)

# For Exponential Seasoning we will use Holt-Winters estimation technique

hws1 <- HoltWinters(sales)  # sales should time series
hws1

#Plot the base level graph, giving limits to x
plot.ts(sales, xlim = c(1995,2014))

#Predict SES, Prediction interval gives me upper and lower bound of the confidence interval
sales.pred1 <- predict(hws1,n.ahead=12,prediction.interval=TRUE)

#Predicted values
sales.pred1

#Plot the base level graph, giving limits to x
plot.ts(sales, xlim = c(1995,2014))

#Historical Fitted values, no trend so both columns are same
hws1$fitted

#Fit the historical fitted values
lines(hws1$fitted[,1],col="green")

#Fit the future predicted values
lines(sales.pred1[,1],col="blue")

#Fit the upper interval predicted values
lines(sales.pred1[,2],col="red")
lines(sales.pred1[,3],col="red")

plot.ts(sales, xlim = c(1995,2014),ylim=c(150000,400000))
lines(hws1$fitted[,1],col="green")
lines(sales.pred1[,1],col="blue")
lines(sales.pred1[,2],col="red")
lines(sales.pred1[,3],col="red")