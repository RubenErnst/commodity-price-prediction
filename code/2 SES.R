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
ts.test <- ts(ts.apsp.monthly.difference[(length(ts.apsp.monthly.difference) - 11):length(ts.apsp.monthly.difference)],
              start = conv_to_ym(time(ts.apsp.monthly.difference)[(length(ts.apsp.monthly.difference) - 11)] - 0.083),
              end = conv_to_ym(time(ts.apsp.monthly.difference)[length(ts.apsp.monthly.difference)] - 0.083),
              frequency = 12)

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[1] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[1] <- mae(ts.test, fit$mean)
ses.results$mape[1] <- mape(ts.test, fit$mean)


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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[6] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[6] <- mae(ts.test, fit$mean)
ses.results$mape[6] <- mape(ts.test, fit$mean)


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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[2] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[2] <- mae(ts.test, fit$mean)
ses.results$mape[2] <- mape(ts.test, fit$mean)


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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[7] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[7] <- mae(ts.test, fit$mean)
ses.results$mape[7] <- mape(ts.test, fit$mean)


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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[3] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[3] <- mae(ts.test, fit$mean)
ses.results$mape[3] <- mape(ts.test, fit$mean)


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

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[8] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[8] <- mae(ts.test, fit$mean)
ses.results$mape[8] <- mape(ts.test, fit$mean)


### NatGas first difference
# Split data
ts.train <- ts(ts.natgas.us.monthly.difference[1:(length(ts.natgas.us.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.natgas.us.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.natgas.us.monthly.difference)[(length(ts.natgas.us.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.natgas.us.monthly.difference[(length(ts.natgas.us.monthly.difference) - 11):length(ts.natgas.us.monthly.difference)],
              start = conv_to_ym(time(ts.natgas.us.monthly.difference)[(length(ts.natgas.us.monthly.difference) - 11)] - 0.083),
              end = conv_to_ym(time(ts.natgas.us.monthly.difference)[length(ts.natgas.us.monthly.difference)] - 0.083),
              frequency = 12)

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[4] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[4] <- mae(ts.test, fit$mean)
ses.results$mape[4] <- mape(ts.test, fit$mean)


### NatGas log returns
# Split data
ts.train <- ts(ts.natgas.us.monhtly.log.returns[1:(length(ts.natgas.us.monhtly.log.returns) - 12)],
               start = conv_to_ym(time(ts.natgas.us.monhtly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.natgas.us.monhtly.log.returns)[(length(ts.natgas.us.monhtly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.natgas.us.monhtly.log.returns[(length(ts.natgas.us.monhtly.log.returns) - 11):length(ts.natgas.us.monhtly.log.returns)],
              start = conv_to_ym(time(ts.natgas.us.monhtly.log.returns)[(length(ts.natgas.us.monhtly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.natgas.us.monhtly.log.returns)[length(ts.natgas.us.monhtly.log.returns)] - 0.083),
              frequency = 12)

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[9] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[9] <- mae(ts.test, fit$mean)
ses.results$mape[9] <- mape(ts.test, fit$mean)


### WTI first difference
# Split data
ts.train <- ts(ts.wti.monthly.difference[1:(length(ts.wti.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.wti.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.wti.monthly.difference)[(length(ts.wti.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.wti.monthly.difference[(length(ts.wti.monthly.difference) - 11):length(ts.wti.monthly.difference)],
              start = conv_to_ym(time(ts.wti.monthly.difference)[(length(ts.wti.monthly.difference) - 11)] - 0.083),
              end = conv_to_ym(time(ts.wti.monthly.difference)[length(ts.wti.monthly.difference)] - 0.083),
              frequency = 12)

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[5] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[5] <- mae(ts.test, fit$mean)
ses.results$mape[5] <- mape(ts.test, fit$mean)


### NatGas log returns
# Split data
ts.train <- ts(ts.wti.monthly.log.returns[1:(length(ts.wti.monthly.log.returns) - 12)],
               start = conv_to_ym(time(ts.wti.monthly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.wti.monthly.log.returns)[(length(ts.wti.monthly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.wti.monthly.log.returns[(length(ts.wti.monthly.log.returns) - 11):length(ts.wti.monthly.log.returns)],
              start = conv_to_ym(time(ts.wti.monthly.log.returns)[(length(ts.wti.monthly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.wti.monthly.log.returns)[length(ts.wti.monthly.log.returns)] - 0.083),
              frequency = 12)

# Fit SES model
fit <- ses(ts.train, h = 12) # h determines the number of periods to be forecast
ses.results$alpha[10] <- unname(fit$model$par[1])

# Evaluate
ses.results$mae[10] <- mae(ts.test, fit$mean)
ses.results$mape[10] <- mape(ts.test, fit$mean)

### Save the results
save(ses.results, file = "results/econometric models/SES.RData")
