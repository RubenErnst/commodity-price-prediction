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
ses.results <- data.frame("commodity" = rep(c("APSP", "Brent", "Dubai", "US NatGas", "WTI"), 2),
                          "series" = c(rep("difference", 5), rep("log returns", 5)),
                          "alpha" = rep(NA, 10),
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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[1] <- unname(fit$model$par[1])

# Evaluate
apsp.pred.diff <- cumsum(c(ts.apsp.monthly.absolute[length(ts.apsp.monthly.absolute) - 12], fit$mean))[-1]
ses.results$mae[1] <- mae(ts.test, apsp.pred.diff)
ses.results$mape[1] <- mape(ts.test, apsp.pred.diff)


### APSP log returns
# Split data
ts.train <- log(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)] / lag(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)]))[2:(length(ts.apsp.monthly.absolute) - 12)]

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[6] <- unname(fit$model$par[1])

# Evaluate
apsp.pred.log.return <- ts.apsp.monthly.absolute[length(ts.apsp.monthly.absolute) - 12] * cumprod(exp(fit$mean))
ses.results$mae[6] <- mae(ts.test, apsp.pred.log.return)
ses.results$mape[6] <- mape(ts.test, apsp.pred.log.return)


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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[2] <- unname(fit$model$par[1])

# Evaluate
brent.pred.diff <- cumsum(c(ts.brent.monthly.absolute[length(ts.brent.monthly.absolute) - 12], fit$mean))[-1]
ses.results$mae[2] <- mae(ts.test, brent.pred.diff)
ses.results$mape[2] <- mape(ts.test, brent.pred.diff)


### Brent log returns
# Split data
ts.train <- log(ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)] / lag(ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)]))[2:(length(ts.brent.monthly.absolute) - 12)]

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[7] <- unname(fit$model$par[1])

# Evaluate
brent.pred.log.return <- ts.brent.monthly.absolute[length(ts.brent.monthly.absolute) - 12] * cumprod(exp(fit$mean))
ses.results$mae[7] <- mae(ts.test, brent.pred.log.return)
ses.results$mape[7] <- mape(ts.test, brent.pred.log.return)


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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[3] <- unname(fit$model$par[1])

# Evaluate
dubai.pred.diff <- cumsum(c(ts.dubai.monthly.absolute[length(ts.dubai.monthly.absolute) - 12], fit$mean))[-1]
ses.results$mae[3] <- mae(ts.test, dubai.pred.diff)
ses.results$mape[3] <- mape(ts.test, dubai.pred.diff)


### Dubai log returns
# Split data
ts.train <- log(ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)] / lag(ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)]))[2:(length(ts.dubai.monthly.absolute) - 12)]

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[8] <- unname(fit$model$par[1])

# Evaluate
dubai.pred.log.return <- ts.dubai.monthly.absolute[length(ts.dubai.monthly.absolute) - 12] * cumprod(exp(fit$mean))
ses.results$mae[8] <- mae(ts.test, dubai.pred.log.return)
ses.results$mape[8] <- mape(ts.test, dubai.pred.log.return)


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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[4] <- unname(fit$model$par[1])

# Evaluate
natgas.us.pred.diff <- cumsum(c(ts.natgas.us.monthly.absolute[length(ts.natgas.us.monthly.absolute) - 12], fit$mean))[-1]
ses.results$mae[4] <- mae(ts.test, natgas.us.pred.diff)
ses.results$mape[4] <- mape(ts.test, natgas.us.pred.diff)


### NatGas log returns
# Split data
ts.train <- log(ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)] / lag(ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)]))[2:(length(ts.natgas.us.monthly.absolute) - 12)]

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[9] <- unname(fit$model$par[1])

# Evaluate
natgas.us.pred.log.return<- ts.natgas.us.monthly.absolute[length(ts.natgas.us.monthly.absolute) - 12] * cumprod(exp(fit$mean))
ses.results$mae[9] <- mae(ts.test, natgas.us.pred.log.return)
ses.results$mape[9] <- mape(ts.test, natgas.us.pred.log.return)


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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[5] <- unname(fit$model$par[1])

# Evaluate
wti.pred.diff <- cumsum(c(ts.wti.monthly.absolute[length(ts.wti.monthly.absolute) - 12], fit$mean))[-1]
ses.results$mae[5] <- mae(ts.test, wti.pred.diff)
ses.results$mape[5] <- mape(ts.test, wti.pred.diff)


### WTI log returns
# Split data
ts.train <- log(ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)] / lag(ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)]))[2:(length(ts.wti.monthly.absolute) - 12)]

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[10] <- unname(fit$model$par[1])

# Evaluate
wti.pred.log.return <- ts.wti.monthly.absolute[length(ts.wti.monthly.absolute) - 12] * cumprod(exp(fit$mean))
ses.results$mae[10] <- mae(ts.test, wti.pred.log.return)
ses.results$mape[10] <- mape(ts.test, wti.pred.log.return)


### Save the results
save(ses.results, file = "results/econometric models/SES.RData")

ses.pred <- data.frame("apsp.diff" = apsp.pred.diff, "apsp.log.return" = apsp.pred.log.return,
                       "brent.diff" = brent.pred.diff, "brent.log.return" = brent.pred.log.return,
                       "dubai.diff" = dubai.pred.diff, "dubai.log.return" = dubai.pred.log.return,
                       "natgas.us.diff" = natgas.us.pred.diff, "natgas.us.log.return" = natgas.us.pred.log.return,
                       "wti.diff" = wti.pred.diff, "wti.log.return" = wti.pred.log.return)
save(ses.pred, file = "results/econometric models/SES pred.Rdata")
