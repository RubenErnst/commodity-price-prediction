rm(list = ls())

library(tidyverse)
library(tseries)


### Load data
load("clean data/timeseries.RData")

### ADF tests
adf.results <- data.frame("commodity" = rep(c("APSP", "Brent", "Dubai", "US NatGas", "WTI"), 3),
                          "series" = c(rep("value", 5), rep("first difference", 5), rep("log-return", 5)),
                          "test" = rep("ADF", 15),
                          "statistic" = rep(NA, 15),
                          "p" = rep(NA, 15))
## ADF statistic
adf.results$statistic <- c(adf.test(ts.apsp.monthly.absolute, "stationary", k = 0)$statistic, # k is the number of delta (=phi_1 - 1) lags, here, k = 0 is used to decrease complexity
                           adf.test(ts.brent.monthly.absolute, "stationary", k = 0)$statistic,
                           adf.test(ts.dubai.monthly.absolute, "stationary", k = 0)$statistic,
                           adf.test(ts.natgas.us.monthly.absolute, "stationary", k = 0)$statistic,
                           adf.test(ts.wti.monthly.absolute, "stationary", k = 0)$statistic,
                           adf.test(ts.apsp.monthly.difference, "stationary", k = 0)$statistic,
                           adf.test(ts.brent.monthly.difference, "stationary", k = 0)$statistic,
                           adf.test(ts.dubai.monthly.difference, "stationary", k = 0)$statistic,
                           adf.test(ts.natgas.us.monthly.difference, "stationary", k = 0)$statistic,
                           adf.test(ts.wti.monthly.difference, "stationary", k = 0)$statistic,
                           adf.test(ts.apsp.monthly.log.returns, "stationary", k = 0)$statistic,
                           adf.test(ts.brent.monthly.log.returns, "stationary", k = 0)$statistic,
                           adf.test(ts.dubai.monthly.log.returns, "stationary", k = 0)$statistic,
                           adf.test(ts.natgas.us.monthly.log.returns, "stationary", k = 0)$statistic,
                           adf.test(ts.wti.monthly.log.returns, "stationary", k = 0)$statistic)

## p-value
adf.results$p <- c(adf.test(ts.apsp.monthly.absolute, "stationary", k = 0)$p.value,
                   adf.test(ts.brent.monthly.absolute, "stationary", k = 0)$p.value,
                   adf.test(ts.dubai.monthly.absolute, "stationary", k = 0)$p.value,
                   adf.test(ts.natgas.us.monthly.absolute, "stationary", k = 0)$p.value,
                   adf.test(ts.wti.monthly.absolute, "stationary", k = 0)$p.value,
                   adf.test(ts.apsp.monthly.difference, "stationary", k = 0)$p.value,
                   adf.test(ts.brent.monthly.difference, "stationary", k = 0)$p.value,
                   adf.test(ts.dubai.monthly.difference, "stationary", k = 0)$p.value,
                   adf.test(ts.natgas.us.monthly.difference, "stationary", k = 0)$p.value,
                   adf.test(ts.wti.monthly.difference, "stationary", k = 0)$p.value,
                   adf.test(ts.apsp.monthly.log.returns, "stationary", k = 0)$p.value,
                   adf.test(ts.brent.monthly.log.returns, "stationary", k = 0)$p.value,
                   adf.test(ts.dubai.monthly.log.returns, "stationary", k = 0)$p.value,
                   adf.test(ts.natgas.us.monthly.log.returns, "stationary", k = 0)$p.value,
                   adf.test(ts.wti.monthly.log.returns, "stationary", k = 0)$p.value)


### Save the results
save(adf.results, file = "results/descriptive stats/ADF.RData")
