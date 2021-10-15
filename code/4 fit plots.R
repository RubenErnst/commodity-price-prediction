rm(list = ls())

library(tidyverse)
library(forecast)
library(ggforce)
library(viridis)
library(ggpubr)


load("clean data/timeseries.RData")
load("results/econometric models/SES pred.RData")
load("results/econometric models/ARIMA pred.RData")
load("results/ml models/ANN pred.RData")
load("results/ml models/RF pred.RData")

load("aux files/color_palettes.RData")

### APSP
# Prepare data for plotting
apsp.plot.data <- data.frame("time" = zoo::yearmon(time(ts.apsp.monthly.absolute)),
                             "price" = ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)],
                             "log.return" = log(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)] / lag(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)])),
                             "diff" = c(NA, diff(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)], 1)))

apsp.plot.data$no.change <- c(rep(NA, length(ts.apsp.monthly.absolute) - 12), rep(ts.apsp.monthly.absolute[length(ts.apsp.monthly.absolute) - 12], 12))
apsp.plot.data$ses <- c(rep(NA, length(ts.apsp.monthly.absolute) - 12), ses.pred$apsp.log.return)
apsp.plot.data$arima <- c(rep(NA, length(ts.apsp.monthly.absolute) - 12), arima.pred$apsp.log.return)
apsp.plot.data$ann <- c(rep(NA, length(ts.apsp.monthly.absolute) - 12), ann.pred$apsp.log.return)
apsp.plot.data$rf <- c(rep(NA, length(ts.apsp.monthly.absolute) - 12), rf.pred$apsp.log.return)

apsp.plot.data <- pivot_longer(apsp.plot.data, cols = c("price", "no.change", "ses", "arima", "ann", "rf"), names_to = "series", values_to = "value")
apsp.plot.data$series <- factor(apsp.plot.data$series, levels = c("price", "no.change", "ses", "arima", "ann", "rf"))

# Plot
apsp.fit.plot <- ggplot(data = apsp.plot.data, aes(x = time, y = value, color = series)) +
  geom_line() +
  scale_color_manual(values = c("black", "red", "orange", "green", "blue", "purple"), name = "Series", labels = c("price", "no change", "SES", "ARIMA", "ANN", "RF"), guide = FALSE) +
  labs(title = "APSP", x = "Time", y = "USD/bbl") +
  facet_zoom(x = series == "price", xlim = c(2019, 2021), ylim = c(20, 70), zoom.size = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.background = element_blank())

apsp.fit.plot

# Save the plot
# ggsave(filename = "plots/4 APSP fit plot.pdf", plot = apsp.fit.plot, device = cairo_pdf, width = 12, height = 7)


### Brent
# Prepare data for plotting
brent.plot.data <- data.frame("time" = zoo::yearmon(time(ts.brent.monthly.absolute)),
                              "price" = ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)],
                              "log.return" = log(ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)] / lag(ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)])),
                              "diff" = c(NA, diff(ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)], 1)))

brent.plot.data$no.change <- c(rep(NA, length(ts.brent.monthly.absolute) - 12), rep(ts.brent.monthly.absolute[length(ts.brent.monthly.absolute) - 12], 12))
brent.plot.data$ses <- c(rep(NA, length(ts.brent.monthly.absolute) - 12), ses.pred$brent.log.return)
brent.plot.data$arima <- c(rep(NA, length(ts.brent.monthly.absolute) - 12), arima.pred$brent.log.return)
brent.plot.data$ann <- c(rep(NA, length(ts.brent.monthly.absolute) - 12), ann.pred$brent.log.return)
brent.plot.data$rf <- c(rep(NA, length(ts.brent.monthly.absolute) - 12), rf.pred$brent.log.return)

brent.plot.data <- pivot_longer(brent.plot.data, cols = c("price", "no.change", "ses", "arima", "ann", "rf"), names_to = "series", values_to = "value")
brent.plot.data$series <- factor(brent.plot.data$series, levels = c("price", "no.change", "ses", "arima", "ann", "rf"))

# Plot
brent.fit.plot <- ggplot(data = brent.plot.data, aes(x = time, y = value, color = series)) +
  geom_line() +
  scale_color_manual(values = c("black", "red", "orange", "green", "blue", "purple"), name = "Series", labels = c("price", "no change", "SES", "ARIMA", "ANN", "RF"), guide = FALSE) +
  labs(title = "Brent", x = "Time", y = "USD/bbl") +
  facet_zoom(x = series == "price", xlim = c(2019, 2021), ylim = c(20, 70), zoom.size = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

brent.fit.plot

# Save the plot
# ggsave(filename = "plots/4 Brent fit plot.pdf", plot = brent.fit.plot, device = cairo_pdf, width = 12, height = 7)


### Dubai
# Prepare data for plotting
dubai.plot.data <- data.frame("time" = zoo::yearmon(time(ts.dubai.monthly.absolute)),
                              "price" = ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)],
                              "log.return" = log(ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)] / lag(ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)])),
                              "diff" = c(NA, diff(ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)], 1)))

dubai.plot.data$no.change <- c(rep(NA, length(ts.dubai.monthly.absolute) - 12), rep(ts.dubai.monthly.absolute[length(ts.dubai.monthly.absolute) - 12], 12))
dubai.plot.data$ses <- c(rep(NA, length(ts.dubai.monthly.absolute) - 12), ses.pred$dubai.log.return)
dubai.plot.data$arima <- c(rep(NA, length(ts.dubai.monthly.absolute) - 12), arima.pred$dubai.log.return)
dubai.plot.data$ann <- c(rep(NA, length(ts.dubai.monthly.absolute) - 12), ann.pred$dubai.log.return)
dubai.plot.data$rf <- c(rep(NA, length(ts.dubai.monthly.absolute) - 12), rf.pred$dubai.log.return)

dubai.plot.data <- pivot_longer(dubai.plot.data, cols = c("price", "no.change", "ses", "arima", "ann", "rf"), names_to = "series", values_to = "value")
dubai.plot.data$series <- factor(dubai.plot.data$series, levels = c("price", "no.change", "ses", "arima", "ann", "rf"))

# Plot
dubai.fit.plot <- ggplot(data = dubai.plot.data, aes(x = time, y = value, color = series)) +
  geom_line() +
  scale_color_manual(values = c("black", "red", "orange", "green", "blue", "purple"), name = "Series", labels = c("price", "no change", "SES", "ARIMA", "ANN", "RF"), guide = FALSE) +
  labs(title = "Dubai Fateh", x = "Time", y = "USD/bbl") +
  facet_zoom(x = series == "price", xlim = c(2019, 2021), ylim = c(20, 70), zoom.size = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

dubai.fit.plot

# Save the plot
# ggsave(filename = "plots/4 Dubai fit plot.pdf", plot = dubai.fit.plot, device = cairo_pdf, width = 12, height = 7)


### NatGas
# Prepare data for plotting
natgas.us.plot.data <- data.frame("time" = zoo::yearmon(time(ts.natgas.us.monthly.absolute)),
                                  "price" = ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)],
                                  "log.return" = log(ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)] / lag(ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)])),
                                  "diff" = c(NA, diff(ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)], 1)))

natgas.us.plot.data$no.change <- c(rep(NA, length(ts.natgas.us.monthly.absolute) - 12), rep(ts.natgas.us.monthly.absolute[length(ts.natgas.us.monthly.absolute) - 12], 12))
natgas.us.plot.data$ses <- c(rep(NA, length(ts.natgas.us.monthly.absolute) - 12), ses.pred$natgas.us.log.return)
natgas.us.plot.data$arima <- c(rep(NA, length(ts.natgas.us.monthly.absolute) - 12), arima.pred$natgas.us.log.return)
natgas.us.plot.data$ann <- c(rep(NA, length(ts.natgas.us.monthly.absolute) - 12), ann.pred$natgas.us.log.return)
natgas.us.plot.data$rf <- c(rep(NA, length(ts.natgas.us.monthly.absolute) - 12), rf.pred$natgas.us.log.return)

natgas.us.plot.data <- pivot_longer(natgas.us.plot.data, cols = c("price", "no.change", "ses", "arima", "ann", "rf"), names_to = "series", values_to = "value")
natgas.us.plot.data$series <- factor(natgas.us.plot.data$series, levels = c("price", "no.change", "ses", "arima", "ann", "rf"))

# Plot
natgas.us.fit.plot <- ggplot(data = natgas.us.plot.data, aes(x = time, y = value, color = series)) +
  geom_line() +
  scale_color_manual(values = c("black", "red", "orange", "green", "blue", "purple"), name = "Series", labels = c("price", "no change", "SES", "ARIMA", "ANN", "RF"), guide = FALSE) +
  labs(title = "Natural Gas Henry Hub", x = "Time", y = "USD/mmBtu") +
  facet_zoom(x = series == "price", xlim = c(2019, 2021), ylim = c(0, 6), zoom.size = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

natgas.us.fit.plot

# Save the plot
# ggsave(filename = "plots/4 NatGas fit plot.pdf", plot = natgas.us.fit.plot, device = cairo_pdf, width = 12, height = 7)


### WTI
# Prepare data for plotting
wti.plot.data <- data.frame("time" = zoo::yearmon(time(ts.wti.monthly.absolute)),
                            "price" = ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)],
                            "log.return" = log(ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)] / lag(ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)])),
                            "diff" = c(NA, diff(ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)], 1)))

wti.plot.data$no.change <- c(rep(NA, length(ts.wti.monthly.absolute) - 12), rep(ts.wti.monthly.absolute[length(ts.wti.monthly.absolute) - 12], 12))
wti.plot.data$ses <- c(rep(NA, length(ts.wti.monthly.absolute) - 12), ses.pred$wti.log.return)
wti.plot.data$arima <- c(rep(NA, length(ts.wti.monthly.absolute) - 12), arima.pred$wti.log.return)
wti.plot.data$ann <- c(rep(NA, length(ts.wti.monthly.absolute) - 12), ann.pred$wti.log.return)
wti.plot.data$rf <- c(rep(NA, length(ts.wti.monthly.absolute) - 12), rf.pred$wti.log.return)

wti.plot.data <- pivot_longer(wti.plot.data, cols = c("price", "no.change", "ses", "arima", "ann", "rf"), names_to = "series", values_to = "value")
wti.plot.data$series <- factor(wti.plot.data$series, levels = c("price", "no.change", "ses", "arima", "ann", "rf"))

# Plot
wti.fit.plot <- ggplot(data = wti.plot.data, aes(x = time, y = value, color = series)) +
  geom_line() +
  scale_color_manual(values = c("black", "red", "orange", "green", "blue", "purple"), name = "Series", labels = c("price", "no change", "SES", "ARIMA", "ANN", "RF")) +
  labs(title = "WTI", x = "Time", y = "USD/bbl") +
  facet_zoom(x = series == "price", xlim = c(2019, 2021), ylim = c(15, 70), zoom.size = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = c(1.2,0.5))

wti.fit.plot

# Save the plot
# ggsave(filename = "plots/4 WTI fit plot.pdf", plot = wti.fit.plot, device = cairo_pdf, width = 12, height = 7)


### Clustered version
fit.plots <- ggarrange(apsp.fit.plot, brent.fit.plot, dubai.fit.plot, natgas.us.fit.plot, wti.fit.plot,
                       ncol = 2, nrow = 3)

fit.plots

ggsave(filename = "plots/4 All fit plots.pdf", plot = fit.plots, device = cairo_pdf, width = 12, height = 12)
