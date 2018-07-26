# Setting the directory.
setwd('C:\\Users\\Shubham\\Documents\\Time Series')
getwd()

# Loading readxl package for xls file.
library(readxl)

# Importing the file
uk <- read_xlsx('C:\\Users\\Shubham\\Documents\\Time Series\\uk.xlsx')

# print variable names.
view(uk1)
names(uk1)

# creating time series data frmae for all the five variables.
ukout <- ts(uk$Ireland, start=c(1996,1), end=c(2005,4),frequency=4)
ukout1 <- ts(uk$`Other EU not Ireland`, start=c(1996,1), end=c(2005,4),frequency=4)
ukout2 <- ts(uk$`Rest of Europe and Med`, start=c(1996,1), end=c(2005,4),frequency=4)
ukout3 <- ts(uk$`Rest of World`, start=c(1996,1), end=c(2005,4),frequency=4)
ukout4 <- ts(uk$Total, start=c(1996,1), end=c(2005,4),frequency=4)

# Printing the time series data frame.
print(ukout)
print(ukout1)
print(ukout2)
print(ukout3)
print(ukout4)

# Plotting.
plot(ukout)
print(ukout1)
print(ukout2)
print(ukout3)
print(ukout4)

# Installing and reading forecast package.
install.packages("forecast", dependencies = T)
library(forecast)

# Decomposing time series into trend, seasonal and irregular components.
fit <- stl(ukout, s.window="period")
fit1 <- stl(ukout1, s.window="period")
fit2 <- stl(ukout2, s.window="period")
fit3 <- stl(ukout3, s.window="period")
fit4 <- stl(ukout4, s.window="period")

# Plotting fit.
plot(fit)
plot(fit1)
plot(fit2)
plot(fit3)
plot(fit4)

# Forecasting the number of passengers to travel in coming next 2 quarters.
forecast(fit, h = 2, level = c(90,95)) # Ireland
forecast(fit1, h = 2, level = c(90,95)) # Other EU not Ireland
forecast(fit2, h = 2, level = c(90,95)) # Rest of Eu and med
forecast(fit3, h = 2, level = c(90,95)) # Rest of World
forecast(fit4, h = 2, level = c(90,95)) # total

#Exponential models- HOLTWINTERS METHOD
# simple exponential
expo <- HoltWinters(ukout4, beta=FALSE, gamma=FALSE)
ls(expo)
require(forecast)
accuracy(expo$fitted, ukout4)

# double exponential 
expo1 <- HoltWinters(ukout4, gamma=FALSE)
accuracy(expo1$fitted, ukout4)

# triple exponential
expo2 <- HoltWinters(ukout4)
accuracy(expo2$fitted, ukout4)

forecast(expo2, 2)

# ETS MODEL
library(forecast)
fit0<-ets(ukout4)
accuracy(fit0$fitted, ukout4)
summary(fit0)

# predict next two future values
library(forecast)
forecast(fit0, 2)
plot(forecast(fit0, 2))

# ARIMA using acf and pacf.
require(tseries)
require(forecast)

acf(ukout4)
pacf(ukout4)
ndiffs(ukout4)

# fit an ARIMA model of order P, D, Q
fit <- arima(ukout4, order=c(3, 1, 2))
summary(fit)

# using auto arima
fit <-auto.arima(ukout4)
ls(fit)
fit$model
fit$series
summary(fit)

# predictive accuracy
library(forecast)
accuracy(fit)

# predict next 2 observations
library(forecast)
forecast(fit, 2)
plot(forecast(fit, 2))

##################################################################################################


