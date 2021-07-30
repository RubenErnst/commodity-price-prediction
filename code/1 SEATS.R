rm(list = ls())

library(seasonal)
library(tidyverse)

### Load data
load("clean data/timeseries.RData")


### SEATS
apsp.seas <- ts.apsp.monthly.absolute %>% seas()
brent.seas <- ts.brent.monthly.absolute %>% seas()
dubai.seas <- ts.dubai.monthly.absolute %>% seas()
natgas.seas <- ts.natgas.us.monthly.absolute %>% seas()
wti.seas <- ts.wti.monthly.absolute %>% seas()


### Plot decompositions
ts.time = attributes(ts.apsp.monthly.absolute)[[1]]
ts.time = seq(ts.time[1], ts.time[2], length.out = (((ts.time[2] - ts.time[1]) * ts.time[3]) + 1))
df = cbind(ts.time, with(decompose(ts.apsp.monthly.absolute), data.frame("observed" = x, "trend" = trend, "seasonal" = seasonal, "random" = random)))
df = pivot_longer(df, cols = c("observed", "trend", "seasonal", "random"), names_to = "component", values_to = "value") %>% arrange(ts.time)
df$component = factor(df$component, levels = c("observed", "trend", "seasonal", "random"))

cairo_pdf("plots/1_APSP SEATS decomposition.pdf", width = 8, height = 5.5)

ggplot(df, aes(ts.time, value)) +
  facet_grid(component ~ ., scales = "free_y") +
  geom_line() +
  theme_bw() +
  labs(y = "USD/bbl", x = "Year") +
  ggtitle("Decomposition of additive APSP time series") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

ts.time = attributes(ts.brent.monthly.absolute)[[1]]
ts.time = seq(ts.time[1], ts.time[2], length.out = (((ts.time[2] - ts.time[1]) * ts.time[3]) + 1))
df = cbind(ts.time, with(decompose(ts.brent.monthly.absolute), data.frame("observed" = x, "trend" = trend, "seasonal" = seasonal, "random" = random)))
df = pivot_longer(df, cols = c("observed", "trend", "seasonal", "random"), names_to = "component", values_to = "value") %>% arrange(ts.time)
df$component = factor(df$component, levels = c("observed", "trend", "seasonal", "random"))

cairo_pdf("plots/1_Brent SEATS decomposition.pdf", width = 8, height = 5.5)

ggplot(df, aes(ts.time, value)) +
  facet_grid(component ~ ., scales = "free_y") +
  geom_line() +
  theme_bw() +
  labs(y = "USD/bbl", x = "Year") +
  ggtitle("Decomposition of additive Brent time series") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

ts.time = attributes(ts.dubai.monthly.absolute)[[1]]
ts.time = seq(ts.time[1], ts.time[2], length.out = (((ts.time[2] - ts.time[1]) * ts.time[3]) + 1))
df = cbind(ts.time, with(decompose(ts.dubai.monthly.absolute), data.frame("observed" = x, "trend" = trend, "seasonal" = seasonal, "random" = random)))
df = pivot_longer(df, cols = c("observed", "trend", "seasonal", "random"), names_to = "component", values_to = "value") %>% arrange(ts.time)
df$component = factor(df$component, levels = c("observed", "trend", "seasonal", "random"))

cairo_pdf("plots/1_Dubai SEATS decomposition.pdf", width = 8, height = 5.5)

ggplot(df, aes(ts.time, value)) +
  facet_grid(component ~ ., scales = "free_y") +
  geom_line() +
  theme_bw() +
  labs(y = "USD/bbl", x = "Year") +
  ggtitle("Decomposition of additive Dubai time series") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

ts.time = attributes(ts.natgas.us.monthly.absolute)[[1]]
ts.time = seq(ts.time[1], ts.time[2], length.out = (((ts.time[2] - ts.time[1]) * ts.time[3]) + 1))
df = cbind(ts.time, with(decompose(ts.natgas.us.monthly.absolute), data.frame("observed" = x, "trend" = trend, "seasonal" = seasonal, "random" = random)))
df = pivot_longer(df, cols = c("observed", "trend", "seasonal", "random"), names_to = "component", values_to = "value") %>% arrange(ts.time)
df$component = factor(df$component, levels = c("observed", "trend", "seasonal", "random"))

cairo_pdf("plots/1_NatGas Henry Hub SEATS decomposition.pdf", width = 8, height = 5.5)

ggplot(df, aes(ts.time, value)) +
  facet_grid(component ~ ., scales = "free_y") +
  geom_line() +
  theme_bw() +
  labs(y = "USD/mBtu", x = "Year") +
  ggtitle("Decomposition of additive US NatGas time series") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

ts.time = attributes(ts.wti.monthly.absolute)[[1]]
ts.time = seq(ts.time[1], ts.time[2], length.out = (((ts.time[2] - ts.time[1]) * ts.time[3]) + 1))
df = cbind(ts.time, with(decompose(ts.wti.monthly.absolute), data.frame("observed" = x, "trend" = trend, "seasonal" = seasonal, "random" = random)))
df = pivot_longer(df, cols = c("observed", "trend", "seasonal", "random"), names_to = "component", values_to = "value") %>% arrange(ts.time)
df$component = factor(df$component, levels = c("observed", "trend", "seasonal", "random"))

cairo_pdf("plots/1_WTI SEATS decomposition.pdf", width = 8, height = 5.5)

ggplot(df, aes(ts.time, value)) +
  facet_grid(component ~ ., scales = "free_y") +
  geom_line() +
  theme_bw() +
  labs(y = "USD/bbl", x = "Year") +
  ggtitle("Decomposition of additive WTI time series") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()
