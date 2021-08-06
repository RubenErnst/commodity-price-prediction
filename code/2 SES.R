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

# Resulting data frame
result <- data.frame("commodity" = rep(c("APSP", "Brent", "Dubai", "US NatGas", "WTI"), 2),
                     "series" = c(rep("difference", 5), rep("log returns", 5)),
                     "mae" = rep(NA, 10),
                     "mape" = rep(NA, 10))

### APSP
ses.apsp.monthly.difference <- forecast::ses(ts.apsp.monthly.difference, h = 12) # h determines the number of periods to be forecast
ses.apsp.monthly.log.returns <- forecast::ses(ts.apsp.monthly.log.returns, h = 12)

result$mae[1] <- mae(ts.apsp.monthly.difference[(length(ts.apsp.monthly.difference) - 11): length(ts.apsp.monthly.difference)],
                     as.data.frame(ses.apsp.monthly.difference)$`Point Forecast`)
result$mae[6] <- mae(ts.apsp.monthly.log.returns[(length(ts.apsp.monthly.log.returns) - 11): length(ts.apsp.monthly.log.returns)],
                     as.data.frame(ses.apsp.monthly.log.returns)$`Point Forecast`)
result$mape[1] <- mape(ts.apsp.monthly.difference[(length(ts.apsp.monthly.difference) - 11): length(ts.apsp.monthly.difference)],
                      as.data.frame(ses.apsp.monthly.difference)$`Point Forecast`)
result$mape[6] <- mape(ts.apsp.monthly.log.returns[(length(ts.apsp.monthly.log.returns) - 11): length(ts.apsp.monthly.log.returns)],
                      as.data.frame(ses.apsp.monthly.log.returns)$`Point Forecast`)

### Brent
ses.brent.monthly.difference <- forecast::ses(ts.brent.monthly.difference, h = 12) # h determines the number of periods to be forecast
ses.brent.monthly.log.returns <- forecast::ses(ts.brent.monthly.log.returns, h = 12)

result$mae[2] <- mae(ts.brent.monthly.difference[(length(ts.brent.monthly.difference) - 11): length(ts.brent.monthly.difference)],
                     as.data.frame(ses.brent.monthly.difference)$`Point Forecast`)
result$mae[7] <- mae(ts.brent.monthly.log.returns[(length(ts.brent.monthly.log.returns) - 11): length(ts.brent.monthly.log.returns)],
                     as.data.frame(ses.brent.monthly.log.returns)$`Point Forecast`)
result$mape[2] <- mape(ts.brent.monthly.difference[(length(ts.brent.monthly.difference) - 11): length(ts.brent.monthly.difference)],
                       as.data.frame(ses.brent.monthly.difference)$`Point Forecast`)
result$mape[7] <- mape(ts.brent.monthly.log.returns[(length(ts.brent.monthly.log.returns) - 11): length(ts.brent.monthly.log.returns)],
                       as.data.frame(ses.brent.monthly.log.returns)$`Point Forecast`)

