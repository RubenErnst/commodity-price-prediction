rm(list = ls())

library(tidyverse)
library(tsibble)
library(forecast)
library(randomForest)


# Load data
load("clean data/timeseries.RData")

# Dealing with ts times
conv_to_ym <- function(ts_date){
  year <- floor(ts_date)
  month <- ts_date - year
  return(c(year, round(month * 12) + 1))
}

# Target functions
mape <- function(y, pred){
  out <- mean(abs((y - pred) / y))
  return(out)
}

mae <- function(y, pred){
  out <- mean(abs(y - pred))
  return(out)
}


### SANDBOX -------------------------------------------------------------------------------------------------------------------------------
ts.train <- ts(ts.apsp.monthly.log.returns[1:(length(ts.apsp.monthly.log.returns) - 12)],
               start = conv_to_ym(time(ts.apsp.monthly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.apsp.monthly.log.returns)[(length(ts.apsp.monthly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.apsp.monthly.log.returns[(length(ts.apsp.monthly.log.returns) - 11):length(ts.apsp.monthly.log.returns)],
              start = conv_to_ym(time(ts.apsp.monthly.log.returns)[(length(ts.apsp.monthly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.apsp.monthly.log.returns)[length(ts.apsp.monthly.log.returns)] - 0.083),
              frequency = 12)

apsp.log.returns.lagged <- data.frame("y" = ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)],
                                      "l1" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 1),
                                      "l2" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 2),
                                      "l3" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 3),
                                      "l4" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 4),
                                      "l5" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 5),
                                      "l6" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 6),
                                      "l7" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 7),
                                      "l8" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 8),
                                      "l9" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 9),
                                      "l10" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 10),
                                      "l11" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 11),
                                      "l12" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 12))


## Recursive multi-step forecast
set.seed(42)
fit <- randomForest(x = apsp.log.returns.lagged[c(-1:-12, -363:-374), 2:13], y = apsp.log.returns.lagged[c(-1:-12, -363:-374), 1])

pred <- c()
for (i in 1:12){
  pred[i] <- predict(fit, apsp.log.returns.lagged[(362 + i), 2:13])
}

mape(ts.apsp.monthly.log.returns[363:374], pred)

## Direct multi-step forecast
set.seed(42)

pred <- c()
for(i in 1:12){
  fit <- randomForest(x = apsp.log.returns.lagged[(25 - i):(363 - i), 2:13], y = apsp.log.returns.lagged[24:362, 1])
  pred[i] <- predict(fit, apsp.log.returns.lagged[362, 2:13])
}

mape(ts.apsp.monthly.log.returns[363:374], pred)
