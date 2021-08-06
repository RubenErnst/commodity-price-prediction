rm(list = ls())

library(tidyverse)
library(ggplot2)
library(rmaf)
library(zoo)
library(itsmr)
library(tseries)
library(forecast)

# Load data
load("clean data/timeseries.RData")

# Target functions
mape <- function(y, pred){
  out <- mean(abs((y - pred) / y))
  return(out)
}

mae <- function(y, pred){
  out <- mean(abs(y - pred))
  return(out)
}


### APSP
# Split data
ts.train <- ts(ts.apsp.monthly.absolute[1:floor(length(ts.apsp.monthly.absolute) * 0.8)], start = c(1990,1), frequency = 12)
ts.test <- ts(ts.apsp.monthly.absolute[floor(length(ts.apsp.monthly.absolute) * 0.8) + 1:length(ts.apsp.monthly.absolute)], start = c(2011, 10), end = c(2021, 2), frequency = 12)

# Check for intra-year seasonality
test <- data.frame("y" = as.double(ts.apsp.monthly.absolute))
test$period <- c(rep(1:12, 31), 1, 2)
temp <- c()
for (i in 1:31){
  temp <- c(temp, rep(paste("period", i, collapse = "-"), 12))
}
test$group <- c(temp, "period-32", "period-32"); rm(temp)

ggplot(test) +
  geom_line(aes(x = period, y = y, colour = group))

# Fit model
fit <- auto.arima(ts.train)
result <- Arima(ts.test, model = fit)

plot.ts(ts.apsp.monthly.absolute)
lines(fit$fitted, col = "blue")
lines(result$fitted, col = "red")

# Evaluate
checkresiduals(fit)
checkresiduals(result)

autoplot(forecast(fit, h = 96))
