rm(list = ls())

library(tidyverse)
library(tseries)


### Load data
load("clean data/timeseries.RData")

### ADF tests
adf.results <- data.frame("commodity" = rep(c("APSP", "Brent", "Dubai", "LNG", "US NatGas", "WTI"), 3),
                          "series" = c(rep("value", 6), rep("first difference", 6), rep("log-return", 6)),
                          "test" = rep("ADF", 18),
                          "statistic" = rep(NA, 18),
                          "p" = rep(NA, 18))
## ADF statistic
adf.results$statistic <- c(adf.test(ts.apsp.monthly.absolute, "stationary")$statistic,
                           adf.test(ts.brent.monthly.absolute, "stationary")$statistic,
                           adf.test(ts.dubai.monthly.absolute, "stationary")$statistic,
                           adf.test(ts.lng.monthly.absolute, "stationary")$statistic,
                           adf.test(ts.natgas.us.monthly.absolute, "stationary")$statistic,
                           adf.test(ts.wti.monthly.absolute, "stationary")$statistic,
                           adf.test(ts.apsp.monthly.difference, "stationary")$statistic,
                           adf.test(ts.brent.monthly.difference, "stationary")$statistic,
                           adf.test(ts.dubai.monthly.difference, "stationary")$statistic,
                           adf.test(ts.lng.monthly.difference, "stationary")$statistic,
                           adf.test(ts.natgas.us.monthly.difference, "stationary")$statistic,
                           adf.test(ts.wti.monthly.difference, "stationary")$statistic,
                           adf.test(ts.apsp.monthly.log.returns, "stationary")$statistic,
                           adf.test(ts.brent.monthly.log.returns, "stationary")$statistic,
                           adf.test(ts.dubai.monthly.log.returns, "stationary")$statistic,
                           adf.test(ts.lng.monthly.log.returns, "stationary")$statistic,
                           adf.test(ts.natgas.us.monthly.log.returns, "stationary")$statistic,
                           adf.test(ts.wti.monthly.log.returns, "stationary")$statistic)

## p-value
adf.results$p <- c(adf.test(ts.apsp.monthly.absolute, "stationary")$p.value,
                   adf.test(ts.brent.monthly.absolute, "stationary")$p.value,
                   adf.test(ts.dubai.monthly.absolute, "stationary")$p.value,
                   adf.test(ts.lng.monthly.absolute, "stationary")$p.value,
                   adf.test(ts.natgas.us.monthly.absolute, "stationary")$p.value,
                   adf.test(ts.wti.monthly.absolute, "stationary")$p.value,
                   adf.test(ts.apsp.monthly.difference, "stationary")$p.value,
                   adf.test(ts.brent.monthly.difference, "stationary")$p.value,
                   adf.test(ts.dubai.monthly.difference, "stationary")$p.value,
                   adf.test(ts.lng.monthly.difference, "stationary")$p.value,
                   adf.test(ts.natgas.us.monthly.difference, "stationary")$p.value,
                   adf.test(ts.wti.monthly.difference, "stationary")$p.value,
                   adf.test(ts.apsp.monthly.log.returns, "stationary")$p.value,
                   adf.test(ts.brent.monthly.log.returns, "stationary")$p.value,
                   adf.test(ts.dubai.monthly.log.returns, "stationary")$p.value,
                   adf.test(ts.lng.monthly.log.returns, "stationary")$p.value,
                   adf.test(ts.natgas.us.monthly.log.returns, "stationary")$p.value,
                   adf.test(ts.wti.monthly.log.returns, "stationary")$p.value)


### Save the results
save(adf.results, file = "results/descriptive stats/ADF.RData")
adf.results$statistic <- ifelse(adf.results$p <= 0.01, paste0(as.character(adf.results$statistic), "***"),
                        ifelse(adf.results$p < 0.05, paste0(as.character(adf.results$statistic), "**"),
                               ifelse(adf.results$p < 0.1, paste0(as.character(adf.results$statistic), "*"),
                                      as.character(adf.results$statistic))))
openxlsx::write.xlsx(adf.results, file = "results/descriptive stats/ADF.xlsx")
