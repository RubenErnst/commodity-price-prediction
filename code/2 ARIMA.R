rm(list = ls())

library(tidyverse)
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

# Dealing with ts times
conv_to_ym <- function(ts_date){
  year <- floor(ts_date)
  month <- ts_date - year
  return(c(year, round(month * 12) + 1))
}

# Resulting data frame
arima.results <- data.frame("commodity" = rep(c("APSP", "Brent", "Dubai", "US NatGas", "WTI"), 2),
                            "series" = c(rep("difference", 5), rep("log returns", 5)),
                            "model" = rep(NA, 10),
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

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[1] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[1] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[1] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


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

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[6] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[6] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[6] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Brent first difference
# Split data
ts.train <- ts(ts.brent.monthly.difference[1:(length(ts.brent.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.brent.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.brent.monthly.difference)[(length(ts.brent.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.brent.monthly.difference[(length(ts.brent.monthly.difference) - 11):length(ts.brent.monthly.difference)],
              start = conv_to_ym(time(ts.brent.monthly.difference)[(length(ts.brent.monthly.difference) - 11)] - 0.083),
              end = conv_to_ym(time(ts.brent.monthly.difference)[length(ts.brent.monthly.difference)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[2] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[2] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[2] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Brent log returns
# Split data
ts.train <- ts(ts.brent.monthly.log.returns[1:(length(ts.brent.monthly.log.returns) - 12)],
               start = conv_to_ym(time(ts.brent.monthly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.brent.monthly.log.returns)[(length(ts.brent.monthly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.brent.monthly.log.returns[(length(ts.brent.monthly.log.returns) - 11):length(ts.brent.monthly.log.returns)],
              start = conv_to_ym(time(ts.brent.monthly.log.returns)[(length(ts.brent.monthly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.brent.monthly.log.returns)[length(ts.brent.monthly.log.returns)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[7] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[7] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[7] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Dubai first difference
# Split data
ts.train <- ts(ts.dubai.monthly.difference[1:(length(ts.dubai.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.dubai.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.dubai.monthly.difference)[(length(ts.dubai.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.dubai.monthly.difference[(length(ts.dubai.monthly.difference) - 11):length(ts.dubai.monthly.difference)],
              start = conv_to_ym(time(ts.dubai.monthly.difference)[(length(ts.dubai.monthly.difference) - 11)] - 0.083),
              end = conv_to_ym(time(ts.dubai.monthly.difference)[length(ts.dubai.monthly.difference)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[3] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[3] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[3] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Dubai log returns
# Split data
ts.train <- ts(ts.dubai.monthly.log.returns[1:(length(ts.dubai.monthly.log.returns) - 12)],
               start = conv_to_ym(time(ts.dubai.monthly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.dubai.monthly.log.returns)[(length(ts.dubai.monthly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.dubai.monthly.log.returns[(length(ts.dubai.monthly.log.returns) - 11):length(ts.dubai.monthly.log.returns)],
              start = conv_to_ym(time(ts.dubai.monthly.log.returns)[(length(ts.dubai.monthly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.dubai.monthly.log.returns)[length(ts.dubai.monthly.log.returns)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[8] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[8] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[8] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### US NatGas first difference
# Split data
ts.train <- ts(ts.natgas.us.monthly.difference[1:(length(ts.natgas.us.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.natgas.us.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.natgas.us.monthly.difference)[(length(ts.natgas.us.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.natgas.us.monthly.difference[(length(ts.natgas.us.monthly.difference) - 11):length(ts.natgas.us.monthly.difference)],
              start = conv_to_ym(time(ts.natgas.us.monthly.difference)[(length(ts.natgas.us.monthly.difference) - 11)] - 0.083),
              end = conv_to_ym(time(ts.natgas.us.monthly.difference)[length(ts.natgas.us.monthly.difference)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[4] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[4] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[4] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### US NatGas log returns
# Split data
ts.train <- ts(ts.natgas.us.monthly.log.returns[1:(length(ts.natgas.us.monthly.log.returns) - 12)],
               start = conv_to_ym(time(ts.natgas.us.monthly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.natgas.us.monthly.log.returns)[(length(ts.natgas.us.monthly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.natgas.us.monthly.log.returns[(length(ts.natgas.us.monthly.log.returns) - 11):length(ts.natgas.us.monthly.log.returns)],
              start = conv_to_ym(time(ts.natgas.us.monthly.log.returns)[(length(ts.natgas.us.monthly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.natgas.us.monthly.log.returns)[length(ts.natgas.us.monthly.log.returns)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[9] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[9] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[9] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### US WTI first difference
# Split data
ts.train <- ts(ts.wti.monthly.difference[1:(length(ts.wti.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.wti.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.wti.monthly.difference)[(length(ts.wti.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.wti.monthly.difference[(length(ts.wti.monthly.difference) - 11):length(ts.wti.monthly.difference)],
              start = conv_to_ym(time(ts.wti.monthly.difference)[(length(ts.wti.monthly.difference) - 11)] - 0.083),
              end = conv_to_ym(time(ts.wti.monthly.difference)[length(ts.wti.monthly.difference)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[5] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[5] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[5] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### US WTI log returns
# Split data
ts.train <- ts(ts.wti.monthly.log.returns[1:(length(ts.wti.monthly.log.returns) - 12)],
               start = conv_to_ym(time(ts.wti.monthly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.wti.monthly.log.returns)[(length(ts.wti.monthly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.wti.monthly.log.returns[(length(ts.wti.monthly.log.returns) - 11):length(ts.wti.monthly.log.returns)],
              start = conv_to_ym(time(ts.wti.monthly.log.returns)[(length(ts.wti.monthly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.wti.monthly.log.returns)[length(ts.wti.monthly.log.returns)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[10] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
arima.results$mae[10] <- mae(ts.test, forecast(fit, h = 12)$mean)
arima.results$mape[10] <- mape(ts.test, forecast(fit, h = 12)$mean)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Save the results
save(arima, file = "results/econometric models/ARIMA.RData")
