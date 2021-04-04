rm(list = ls())

### Plot overviews on some of the data

library(tidyverse)
library(ggplot2)
library(rmaf)
library(zoo)
library(itsmr)
library(tseries)


### Load data
load("clean data/EIA short term energy outlook monthly.RData")
load("clean data/APSP monthly.RData")
load("clean data/Brent Crude monthly.RData")
load("clean data/Dubai Fateh monthly.RData")
load("clean data/Nat Gas Henry Hub monthly.RData")
load("clean data/WTI Crude monthly.RData")


### Create time series
## APSP
ts.apsp.monthly.absolute <- ts(apsp.monthly.absolute$value,
                               start = c(min(apsp.monthly.absolute$year), min(apsp.monthly.absolute$month[apsp.monthly.absolute$year == min(apsp.monthly.absolute$year)])),
                               end = c(max(apsp.monthly.absolute$year), max(apsp.monthly.absolute$month[apsp.monthly.absolute$year == max(apsp.monthly.absolute$year)])),
                               frequency = 12)

png("plots/0_APSP monthly prices.png", width = 1200, height = 800)
plot.ts(ts.apsp.monthly.absolute, ylab = "APSP monthly prices")
dev.off()

ts.apsp.monthly.log.return <- ts(log(apsp.monthly.return$value + 1),
                                 start = c(min(apsp.monthly.return$year), min(apsp.monthly.return$month[apsp.monthly.return$year == min(apsp.monthly.return$year)])),
                                 end = c(max(apsp.monthly.return$year), max(apsp.monthly.return$month[apsp.monthly.return$year == max(apsp.monthly.return$year)])),
                                 frequency = 12)

png("plots/0_APSP monthly log returns.png", width = 1200, height = 800)
plot.ts(ts.apsp.monthly.log.return, ylab = "APSP monthly log returns")
dev.off()


## Brent
ts.brent.monthly.absolute <- ts(brent.monthly.absolute$value,
                                start = c(min(brent.monthly.absolute$year), min(brent.monthly.absolute$month[brent.monthly.absolute$year == min(brent.monthly.absolute$year)])),
                                end = c(max(brent.monthly.absolute$year), max(brent.monthly.absolute$month[brent.monthly.absolute$year == max(brent.monthly.absolute$year)])),
                                frequency = 12)

png("plots/0_Brent monthly prices.png", width = 1200, height = 800)
plot.ts(ts.brent.monthly.absolute, ylab = "Brent monthly prices")
dev.off()

ts.brent.monthly.log.return <- ts(log(brent.monthly.return$value + 1),
                                  start = c(min(brent.monthly.return$year), min(brent.monthly.return$month[brent.monthly.return$year == min(brent.monthly.return$year)])),
                                  end = c(max(brent.monthly.return$year), max(brent.monthly.return$month[brent.monthly.return$year == max(brent.monthly.return$year)])),
                                  frequency = 12)

png("plots/0_Brent monthly log returns.png", width = 1200, height = 800)
plot.ts(ts.brent.monthly.log.return, ylab = "Brent monthly log returns")
dev.off()


## Dubai Fateh
ts.dubai.monthly.absolute <- ts(dubai.monthly.absolute$value,
                                start = c(min(dubai.monthly.absolute$year), min(dubai.monthly.absolute$month[dubai.monthly.absolute$year == min(dubai.monthly.absolute$year)])),
                                end = c(max(dubai.monthly.absolute$year), max(dubai.monthly.absolute$month[dubai.monthly.absolute$year == max(dubai.monthly.absolute$year)])),
                                frequency = 12)

png("plots/0_Dubai monthly prices.png", width = 1200, height = 800)
plot.ts(ts.dubai.monthly.absolute, ylab = "Dubai Fateh monthly prices")
dev.off()

ts.dubai.monthly.log.returns <- ts(log(dubai.monthly.return$value + 1),
                                   start = c(min(dubai.monthly.return$year), min(dubai.monthly.return$month[dubai.monthly.return$year == min(dubai.monthly.return$year)])),
                                   end = c(max(dubai.monthly.return$year), max(dubai.monthly.return$month[dubai.monthly.return$year == max(dubai.monthly.return$year)])),
                                   frequency = 12)

png("plots/0_Dubai monthly log returns.png", width = 1200, height = 800)
plot.ts(ts.dubai.monthly.log.returns, ylab = "Dubai Fateh monthly log returns")
dev.off()


## Nat Gas Henry Hub
ts.natgas.us.monthly.absolute <- ts(natgas.us.monthly.absolute$value,
                                    start = c(min(natgas.us.monthly.absolute$year), min(natgas.us.monthly.absolute$month[natgas.us.monthly.absolute$year == min(natgas.us.monthly.absolute$year)])),
                                    end = c(max(natgas.us.monthly.absolute$year), max(natgas.us.monthly.absolute$month[natgas.us.monthly.absolute$year == max(natgas.us.monthly.absolute$year)])),
                                    frequency = 12)

png("plots/0_NatGas Henry Hub monthly prices.png", width = 1200, height = 800)
plot.ts(ts.natgas.us.monthly.absolute, ylab = "Nat Gas Henry Hub monthly prices (USD/MMBtu)")
dev.off()

ts.natgas.us.monthly.log.returns <- ts(log(natgas.us.monthly.return$value + 1),
                                       start = c(min(natgas.us.monthly.return$year), min(natgas.us.monthly.return$month[natgas.us.monthly.return$year == min(natgas.us.monthly.return$year)])),
                                       end = c(max(natgas.us.monthly.return$year), max(natgas.us.monthly.return$month[natgas.us.monthly.return$year == max(natgas.us.monthly.return$year)])),
                                       frequency = 12)

png("plots/0_NatGas Henry Hub monthly log returns.png", width = 1200, height = 800)
plot.ts(ts.natgas.us.monthly.log.returns, ylab = "Nat Gas Henry Hub monthly log returns")
dev.off()


## WTI
ts.wti.monthly.absolute <- ts(wti.monthly.absolute$value,
                              start = c(min(wti.monthly.absolute$year), min(wti.monthly.absolute$month[wti.monthly.absolute$year == min(wti.monthly.absolute$year)])),
                              end = c(max(wti.monthly.absolute$year), max(wti.monthly.absolute$month[wti.monthly.absolute$year == max(wti.monthly.absolute$year)])),
                              frequency = 12)

png("plots/0_WTI monthly prices.png", width = 1200, height = 800)
plot.ts(ts.wti.monthly.absolute, ylab = "WTI monthly prices")
dev.off()

ts.wti.monthly.log.returns <- ts(log(wti.monthly.return$value + 1),
                                 start = c(min(wti.monthly.return$year), min(wti.monthly.return$month[wti.monthly.return$year == min(wti.monthly.return$year)])),
                                 end = c(max(wti.monthly.return$year), max(wti.monthly.return$month[wti.monthly.return$year == max(wti.monthly.return$year)])),
                                 frequency = 12)

png("plots/0_WTI monthly log returns.png", width = 1200, height = 800)
plot.ts(ts.wti.monthly.log.returns, ylab = "WTI monthly log returns")
dev.off()


### Save timeseries
save(ts.apsp.monthly.absolute, ts.apsp.monthly.log.return, ts.brent.monthly.absolute, ts.brent.monthly.log.return, ts.dubai.monthly.absolute,
     ts.dubai.monthly.log.returns, ts.natgas.us.monthly.absolute, ts.natgas.us.monthly.log.returns, ts.wti.monthly.absolute,
     ts.wti.monthly.log.returns, file = "clean data/timeseries.RData")
