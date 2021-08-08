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
ses.results <- data.frame("commodity" = rep(c("APSP", "Brent", "Dubai", "US NatGas", "WTI"), 2),
                     "series" = c(rep("difference", 5), rep("log returns", 5)),
                     "mae" = rep(NA, 10),
                     "mape" = rep(NA, 10))

### APSP
ses.apsp.monthly.difference <- forecast::ses(ts.apsp.monthly.difference[1 : (length(ts.apsp.monthly.difference) - 11)], h = 12) # h determines the number of periods to be forecast
ses.apsp.monthly.log.returns <- forecast::ses(ts.apsp.monthly.log.returns[1 : (length(ts.apsp.monthly.log.returns) - 11)], h = 12)

ses.results$mae[1] <- mae(ts.apsp.monthly.difference[(length(ts.apsp.monthly.difference) - 11): length(ts.apsp.monthly.difference)],
                     as.data.frame(ses.apsp.monthly.difference)$`Point Forecast`)
ses.results$mae[6] <- mae(ts.apsp.monthly.log.returns[(length(ts.apsp.monthly.log.returns) - 11): length(ts.apsp.monthly.log.returns)],
                     as.data.frame(ses.apsp.monthly.log.returns)$`Point Forecast`)
ses.results$mape[1] <- mape(ts.apsp.monthly.difference[(length(ts.apsp.monthly.difference) - 11): length(ts.apsp.monthly.difference)],
                      as.data.frame(ses.apsp.monthly.difference)$`Point Forecast`)
ses.results$mape[6] <- mape(ts.apsp.monthly.log.returns[(length(ts.apsp.monthly.log.returns) - 11): length(ts.apsp.monthly.log.returns)],
                      as.data.frame(ses.apsp.monthly.log.returns)$`Point Forecast`)

### Brent
ses.brent.monthly.difference <- forecast::ses(ts.brent.monthly.difference[1: (length(ts.brent.monthly.difference) - 11)], h = 12)
ses.brent.monthly.log.returns <- forecast::ses(ts.brent.monthly.log.returns[1: (length(ts.brent.monthly.log.returns) - 11)], h = 12)

ses.results$mae[2] <- mae(ts.brent.monthly.difference[(length(ts.brent.monthly.difference) - 11): length(ts.brent.monthly.difference)],
                     as.data.frame(ses.brent.monthly.difference)$`Point Forecast`)
ses.results$mae[7] <- mae(ts.brent.monthly.log.returns[(length(ts.brent.monthly.log.returns) - 11): length(ts.brent.monthly.log.returns)],
                     as.data.frame(ses.brent.monthly.log.returns)$`Point Forecast`)
ses.results$mape[2] <- mape(ts.brent.monthly.difference[(length(ts.brent.monthly.difference) - 11): length(ts.brent.monthly.difference)],
                       as.data.frame(ses.brent.monthly.difference)$`Point Forecast`)
ses.results$mape[7] <- mape(ts.brent.monthly.log.returns[(length(ts.brent.monthly.log.returns) - 11): length(ts.brent.monthly.log.returns)],
                       as.data.frame(ses.brent.monthly.log.returns)$`Point Forecast`)

### Dubai
ses.dubai.monthly.difference <- forecast::ses(ts.dubai.monthly.difference[1: (length(ts.dubai.monthly.difference) - 11)], h = 12)
ses.dubai.monthly.log.returns <- forecast::ses(ts.dubai.monthly.log.returns[1: (length(ts.dubai.monthly.log.returns) - 11)], h = 12)

ses.results$mae[3] <- mae(ts.dubai.monthly.difference[(length(ts.dubai.monthly.difference) - 11): length(ts.dubai.monthly.difference)],
                     as.data.frame(ses.dubai.monthly.difference)$`Point Forecast`)
ses.results$mae[8] <- mae(ts.dubai.monthly.log.returns[(length(ts.dubai.monthly.log.returns) - 11): length(ts.dubai.monthly.log.returns)],
                     as.data.frame(ses.dubai.monthly.log.returns)$`Point Forecast`)
ses.results$mape[3] <- mape(ts.dubai.monthly.difference[(length(ts.dubai.monthly.difference) - 11): length(ts.dubai.monthly.difference)],
                       as.data.frame(ses.dubai.monthly.difference)$`Point Forecast`)
ses.results$mape[8] <- mape(ts.dubai.monthly.log.returns[(length(ts.dubai.monthly.log.returns) - 11): length(ts.dubai.monthly.log.returns)],
                       as.data.frame(ses.dubai.monthly.log.returns)$`Point Forecast`)

### NatGas
ses.natgas.us.monthly.difference <- forecast::ses(ts.natgas.us.monthly.difference[1: (length(ts.natgas.us.monthly.difference) - 11)], h = 12)
ses.natgas.us.monthly.log.returns <- forecast::ses(ts.natgas.us.monthly.log.returns[1: (length(ts.natgas.us.monthly.log.returns) - 11)], h = 12)

ses.results$mae[4] <- mae(ts.natgas.us.monthly.difference[(length(ts.natgas.us.monthly.difference) - 11): length(ts.natgas.us.monthly.difference)],
                     as.data.frame(ses.natgas.us.monthly.difference)$`Point Forecast`)
ses.results$mae[9] <- mae(ts.natgas.us.monthly.log.returns[(length(ts.natgas.us.monthly.log.returns) - 11): length(ts.natgas.us.monthly.log.returns)],
                     as.data.frame(ses.natgas.us.monthly.log.returns)$`Point Forecast`)
ses.results$mape[4] <- mape(ts.natgas.us.monthly.difference[(length(ts.natgas.us.monthly.difference) - 11): length(ts.natgas.us.monthly.difference)],
                       as.data.frame(ses.natgas.us.monthly.difference)$`Point Forecast`)
ses.results$mape[9] <- mape(ts.natgas.us.monthly.log.returns[(length(ts.natgas.us.monthly.log.returns) - 11): length(ts.natgas.us.monthly.log.returns)],
                       as.data.frame(ses.natgas.us.monthly.log.returns)$`Point Forecast`)

### WTI
ses.wti.monthly.difference <- forecast::ses(ts.wti.monthly.difference[1: (length(ts.wti.monthly.difference) - 11)], h = 12)
ses.wti.monthly.log.returns <- forecast::ses(ts.wti.monthly.log.returns[1: (length(ts.wti.monthly.log.returns) - 11)], h = 12)

ses.results$mae[5] <- mae(ts.wti.monthly.difference[(length(ts.wti.monthly.difference) - 11): length(ts.wti.monthly.difference)],
                     as.data.frame(ses.wti.monthly.difference)$`Point Forecast`)
ses.results$mae[10] <- mae(ts.wti.monthly.log.returns[(length(ts.wti.monthly.log.returns) - 11): length(ts.wti.monthly.log.returns)],
                     as.data.frame(ses.wti.monthly.log.returns)$`Point Forecast`)
ses.results$mape[5] <- mape(ts.wti.monthly.difference[(length(ts.wti.monthly.difference) - 11): length(ts.wti.monthly.difference)],
                       as.data.frame(ses.wti.monthly.difference)$`Point Forecast`)
ses.results$mape[10] <- mape(ts.wti.monthly.log.returns[(length(ts.wti.monthly.log.returns) - 11): length(ts.wti.monthly.log.returns)],
                       as.data.frame(ses.wti.monthly.log.returns)$`Point Forecast`)


### Save the results
save(ses.results, file = "results/econometric models/SES.RData")
