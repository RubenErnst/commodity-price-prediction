rm(list = ls())

library(tidyverse)
library(forecast)
library(tsfgrnn)

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

# Dealing with ts times
conv_to_ym <- function(ts_date){
  year <- floor(ts_date)
  month <- ts_date - year
  return(c(year, round(month * 12) + 1))
}

# Resulting data frame
ann.results <- data.frame("commodity" = rep(c("APSP", "Brent", "Dubai", "US NatGas", "WTI"), 2),
                          "series" = c(rep("difference", 5), rep("log returns", 5)),
                          "model.dim" = rep(NA, 10),
                          "mae" = rep(NA, 10),
                          "mape" = rep(NA, 10))


### APSP first difference
# Split data
ts.train <- ts(ts.apsp.monthly.difference[1:(length(ts.apsp.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.apsp.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.apsp.monthly.difference)[(length(ts.apsp.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.apsp.monthly.difference[(length(ts.apsp.monthly.difference) - 11):length(ts.apsp.monthly.difference)],
              start = conv_to_ym(time(ts.apsp.monthly.difference)[(length(ts.apsp.monthly.difference) - 11)] - 0.083),
              end = conv_to_ym(time(ts.apsp.monthly.difference)[length(ts.apsp.monthly.difference)] - 0.083),
              frequency = 12)

# Train model
pred <- grnn_forecasting(ts.train, h = 12)

# Evaluate
ann.results$mae[1] <- mae(ts.test, pred$prediction)
ann.results$mape[1] <- mape(ts.test, pred$prediction)


### APSP log returns
# Split data
ts.train <- ts(ts.apsp.monthly.log.returns[1:(length(ts.apsp.monthly.log.returns) - 12)],
               start = conv_to_ym(time(ts.apsp.monthly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.apsp.monthly.log.returns)[(length(ts.apsp.monthly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.apsp.monthly.log.returns[(length(ts.apsp.monthly.log.returns) - 11):length(ts.apsp.monthly.log.returns)],
              start = conv_to_ym(time(ts.apsp.monthly.log.returns)[(length(ts.apsp.monthly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.apsp.monthly.log.returns)[length(ts.apsp.monthly.log.returns)] - 0.083),
              frequency = 12)

# Train model
pred <- grnn_forecasting(ts.train, h = 12)

# Evaluate
ann.results$mae[6] <- mae(ts.test, pred$prediction)
ann.results$mape[6] <- mape(ts.test, pred$prediction)



### SANDBOX ###########################################################
ts.train <- ts(ts.apsp.monthly.absolute[1:(length(ts.apsp.monthly.absolute) - 12)],
               start = conv_to_ym(time(ts.apsp.monthly.absolute)[1] - 0.083),
               end = conv_to_ym(time(ts.apsp.monthly.absolute)[(length(ts.apsp.monthly.absolute) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.apsp.monthly.absolute[(length(ts.apsp.monthly.absolute) - 11):length(ts.apsp.monthly.absolute)],
              start = conv_to_ym(time(ts.apsp.monthly.absolute)[(length(ts.apsp.monthly.absolute) - 11)] - 0.083),
              end = conv_to_ym(time(ts.apsp.monthly.absolute)[length(ts.apsp.monthly.absolute)] - 0.083),
              frequency = 12)

pred <- grnn_forecasting(timeS = ts.train, h = 12, msas = "recursive", transform = "none", sigma = 0.05)
mae(ts.test, pred$prediction)
mape(ts.test, pred$prediction)

plot(ts.apsp.monthly.absolute)
lines(pred$prediction, col = "red")

rolling <- rolling_origin(pred, h = 12)
pred$prediction
