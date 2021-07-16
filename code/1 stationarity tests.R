rm(list = ls())

library(tidyverse)
library(tseries)


### Load data
load("clean data/timeseries.RData")

### ADF tests
## APSP
adf.apsp.stationary <- adf.test(ts.apsp.monthly.absolute, "stationary")
adf.apsp.stationary <- adf.test(ts.apsp.monthly.difference, "stationary", k = 2)
