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
ts.test <- ts(ts.apsp.monthly.absolute[(length(ts.apsp.monthly.absolute) - 11):length(ts.apsp.monthly.absolute)],
              start = conv_to_ym(time(ts.apsp.monthly.absolute)[(length(ts.apsp.monthly.absolute) - 11)] - 0.083),
              end = conv_to_ym(time(ts.apsp.monthly.absolute)[length(ts.apsp.monthly.absolute)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[1] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
apsp.pred.diff <- cumsum(c(ts.apsp.monthly.absolute[length(ts.apsp.monthly.absolute) - 12], forecast(fit, h = 12)$mean))[-1]
arima.results$mae[1] <- mae(ts.test, apsp.pred.diff)
arima.results$mape[1] <- mape(ts.test, apsp.pred.diff)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### APSP log returns
# Split data
ts.train <- log(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)] / lag(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)]))[2:(length(ts.apsp.monthly.absolute) - 12)]

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[6] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
apsp.pred.log.return <- ts.apsp.monthly.absolute[length(ts.apsp.monthly.absolute) - 12] * cumprod(exp(forecast(fit, h = 12)$mean))
arima.results$mae[6] <- mae(ts.test, apsp.pred.log.return)
arima.results$mape[6] <- mape(ts.test, apsp.pred.log.return)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Brent first difference
# Split data
ts.train <- ts(ts.brent.monthly.difference[1:(length(ts.brent.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.brent.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.brent.monthly.difference)[(length(ts.brent.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.brent.monthly.absolute[(length(ts.brent.monthly.absolute) - 11):length(ts.brent.monthly.absolute)],
              start = conv_to_ym(time(ts.brent.monthly.absolute)[(length(ts.brent.monthly.absolute) - 11)] - 0.083),
              end = conv_to_ym(time(ts.brent.monthly.absolute)[length(ts.brent.monthly.absolute)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[2] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
brent.pred.diff <- cumsum(c(ts.brent.monthly.absolute[length(ts.brent.monthly.absolute) - 12], forecast(fit, h = 12)$mean))[-1]
arima.results$mae[2] <- mae(ts.test, brent.pred.diff)
arima.results$mape[2] <- mape(ts.test, brent.pred.diff)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Brent log returns
# Split data
ts.train <- log(ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)] / lag(ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)]))[2:(length(ts.brent.monthly.absolute) - 12)]

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[7] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
brent.pred.log.return <- ts.brent.monthly.absolute[length(ts.brent.monthly.absolute) - 12] * cumprod(exp(forecast(fit, h = 12)$mean))
arima.results$mae[7] <- mae(ts.test, brent.pred.log.return)
arima.results$mape[7] <- mape(ts.test, brent.pred.log.return)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Dubai first difference
# Split data
ts.train <- ts(ts.dubai.monthly.difference[1:(length(ts.dubai.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.dubai.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.dubai.monthly.difference)[(length(ts.dubai.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.dubai.monthly.absolute[(length(ts.dubai.monthly.absolute) - 11):length(ts.dubai.monthly.absolute)],
              start = conv_to_ym(time(ts.dubai.monthly.absolute)[(length(ts.dubai.monthly.absolute) - 11)] - 0.083),
              end = conv_to_ym(time(ts.dubai.monthly.absolute)[length(ts.dubai.monthly.absolute)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[3] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
dubai.pred.diff <- cumsum(c(ts.dubai.monthly.absolute[length(ts.dubai.monthly.absolute) - 12], forecast(fit, h = 12)$mean))[-1]
arima.results$mae[3] <- mae(ts.test, dubai.pred.diff)
arima.results$mape[3] <- mape(ts.test, dubai.pred.diff)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Dubai log returns
# Split data
ts.train <- log(ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)] / lag(ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)]))[2:(length(ts.dubai.monthly.absolute) - 12)]

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[8] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
dubai.pred.log.return <- ts.dubai.monthly.absolute[length(ts.dubai.monthly.absolute) - 12] * cumprod(exp(forecast(fit, h = 12)$mean))
arima.results$mae[8] <- mae(ts.test, dubai.pred.log.return)
arima.results$mape[8] <- mape(ts.test, dubai.pred.log.return)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### NatGas first difference
# Split data
ts.train <- ts(ts.natgas.us.monthly.difference[1:(length(ts.natgas.us.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.natgas.us.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.natgas.us.monthly.difference)[(length(ts.natgas.us.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.natgas.us.monthly.absolute[(length(ts.natgas.us.monthly.absolute) - 11):length(ts.natgas.us.monthly.absolute)],
              start = conv_to_ym(time(ts.natgas.us.monthly.absolute)[(length(ts.natgas.us.monthly.absolute) - 11)] - 0.083),
              end = conv_to_ym(time(ts.natgas.us.monthly.absolute)[length(ts.natgas.us.monthly.absolute)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[4] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
natgas.us.pred.diff <- cumsum(c(ts.natgas.us.monthly.absolute[length(ts.natgas.us.monthly.absolute) - 12], forecast(fit, h = 12)$mean))[-1]
arima.results$mae[4] <- mae(ts.test, natgas.us.pred.diff)
arima.results$mape[4] <- mape(ts.test, natgas.us.pred.diff)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### NatGas log returns
# Split data
ts.train <- log(ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)] / lag(ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)]))[2:(length(ts.natgas.us.monthly.absolute) - 12)]

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[9] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
natgas.us.pred.log.return <- ts.natgas.us.monthly.absolute[length(ts.natgas.us.monthly.absolute) - 12] * cumprod(exp(forecast(fit, h = 12)$mean))
arima.results$mae[9] <- mae(ts.test, natgas.us.pred.log.return)
arima.results$mape[9] <- mape(ts.test, natgas.us.pred.log.return)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### WTI first difference
# Split data
ts.train <- ts(ts.wti.monthly.difference[1:(length(ts.wti.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.wti.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.wti.monthly.difference)[(length(ts.wti.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.wti.monthly.absolute[(length(ts.wti.monthly.absolute) - 11):length(ts.wti.monthly.absolute)],
              start = conv_to_ym(time(ts.wti.monthly.absolute)[(length(ts.wti.monthly.absolute) - 11)] - 0.083),
              end = conv_to_ym(time(ts.wti.monthly.absolute)[length(ts.wti.monthly.absolute)] - 0.083),
              frequency = 12)

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[5] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
wti.pred.diff <- cumsum(c(ts.wti.monthly.absolute[length(ts.wti.monthly.absolute) - 12], forecast(fit, h = 12)$mean))[-1]
arima.results$mae[5] <- mae(ts.test, wti.pred.diff)
arima.results$mape[5] <- mape(ts.test, wti.pred.diff)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### WTI log returns
# Split data
ts.train <- log(ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)] / lag(ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)]))[2:(length(ts.wti.monthly.absolute) - 12)]

# Fit model
fit <- auto.arima(ts.train)
arima.results$model[10] <- paste0("(", fit$arma[1], ", ", fit$arma[6], ", ", fit$arma[2], ")")

# Evaluate
wti.pred.log.return <- ts.wti.monthly.absolute[length(ts.wti.monthly.absolute) - 12] * cumprod(exp(forecast(fit, h = 12)$mean))
arima.results$mae[10] <- mae(ts.test, wti.pred.log.return)
arima.results$mape[10] <- mape(ts.test, wti.pred.log.return)

# Plot
checkresiduals(fit)
autoplot(forecast(fit, h = 12))


### Save the results
save(arima.results, file = "results/econometric models/ARIMA.RData")
openxlsx::write.xlsx(arima.results, file = "results/econometric models/ARIMA.xlsx")

arima.pred <- data.frame("apsp.diff" = apsp.pred.diff, "apsp.log.return" = apsp.pred.log.return,
                         "brent.diff" = brent.pred.diff, "brent.log.return" = brent.pred.log.return,
                         "dubai.diff" = dubai.pred.diff, "dubai.log.return" = dubai.pred.log.return,
                         "natgas.us.diff" = natgas.us.pred.diff, "natgas.us.log.return" = natgas.us.pred.log.return,
                         "wti.diff" = wti.pred.diff, "wti.log.return" = wti.pred.log.return)
save(arima.pred, file = "results/econometric models/ARIMA pred.RData")
