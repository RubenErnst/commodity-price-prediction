rm(list = ls())

library(tidyverse)
library(viridis)

load("results/ml models/APSP_log_returns_forked.RData")
load("results/ml models/Brent_log_returns_forked.RData")
load("results/ml models/Dubai_log_returns_forked.RData")
load("results/ml models/NatGas_log_returns_forked.RData")
load("results/ml models/WTI_log_returns_forked.RData")


### APSP
# Prepare data to plot
apsp.plot.data <- select(tuning.results.apsp.log.returns.forked, -mae)
apsp.plot.data$n_hl <- sapply(apsp.plot.data$hidden_config, function(x){length(unlist(str_split(x, ",")))})
apsp.plot.data$n_nodes <- sapply(apsp.plot.data$hidden_config, function(x){sum(as.numeric(unlist(str_split(x, ","))))})
apsp.plot.data$nodes_category[apsp.plot.data$n_nodes < 5] <- "<5"
apsp.plot.data$nodes_category[apsp.plot.data$n_nodes >= 5 & apsp.plot.data$n_nodes <= 10] <- "5-10"
apsp.plot.data$nodes_category[apsp.plot.data$n_nodes > 10 & apsp.plot.data$n_nodes <= 20] <- "11-20"
apsp.plot.data$nodes_category[apsp.plot.data$n_nodes > 20 & apsp.plot.data$n_nodes <= 30] <- "21-30"
apsp.plot.data$nodes_category[apsp.plot.data$n_nodes > 30 & apsp.plot.data$n_nodes <= 40] <- "31-40"
apsp.plot.data$nodes_category[apsp.plot.data$n_nodes > 40 & apsp.plot.data$n_nodes <= 50] <- "41-50"
apsp.plot.data$nodes_category[apsp.plot.data$n_nodes > 50 & apsp.plot.data$n_nodes <= 60] <- "51-60"
apsp.plot.data$hl_category <- paste(apsp.plot.data$n_hl, " layer(s), ", apsp.plot.data$nodes_category, " nodes")

apsp.plot.data <- aggregate(mape ~ n_lags + hl_category, apsp.plot.data, mean)
apsp.plot.data$hl_category <- factor(apsp.plot.data$hl_category, levels = c("1  layer(s),  <5  nodes", "1  layer(s),  5-10  nodes", "1  layer(s),  11-20  nodes", "2  layer(s),  <5  nodes", "2  layer(s),  5-10  nodes",
                                                                            "2  layer(s),  11-20  nodes", "2  layer(s),  21-30  nodes", "2  layer(s),  31-40  nodes", "3  layer(s),  <5  nodes", "3  layer(s),  5-10  nodes",
                                                                            "3  layer(s),  11-20  nodes", "3  layer(s),  21-30  nodes", "3  layer(s),  31-40  nodes", "3  layer(s),  41-50  nodes", "3  layer(s),  51-60  nodes"))

# Plot
apsp.mape.heatmap <- ggplot(data = apsp.plot.data, aes(x = hl_category, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "MAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "ANN error heatmap", subtitle = "APSP log returns", xlab = "hidden layer configuration", ylab = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

apsp.mape.heatmap

ggsave(filename = "plots/3 APSP error heatmap.pdf", plot = apsp.mape.heatmap, device = cairo_pdf)


### Brent
# Prepare data to plot
brent.plot.data <- select(tuning.results.brent.log.returns.forked, -mae)
brent.plot.data$n_hl <- sapply(brent.plot.data$hidden_config, function(x){length(unlist(str_split(x, ",")))})
brent.plot.data$n_nodes <- sapply(brent.plot.data$hidden_config, function(x){sum(as.numeric(unlist(str_split(x, ","))))})
brent.plot.data$nodes_category[brent.plot.data$n_nodes < 5] <- "<5"
brent.plot.data$nodes_category[brent.plot.data$n_nodes >= 5 & brent.plot.data$n_nodes <= 10] <- "5-10"
brent.plot.data$nodes_category[brent.plot.data$n_nodes > 10 & brent.plot.data$n_nodes <= 20] <- "11-20"
brent.plot.data$nodes_category[brent.plot.data$n_nodes > 20 & brent.plot.data$n_nodes <= 30] <- "21-30"
brent.plot.data$nodes_category[brent.plot.data$n_nodes > 30 & brent.plot.data$n_nodes <= 40] <- "31-40"
brent.plot.data$nodes_category[brent.plot.data$n_nodes > 40 & brent.plot.data$n_nodes <= 50] <- "41-50"
brent.plot.data$nodes_category[brent.plot.data$n_nodes > 50 & brent.plot.data$n_nodes <= 60] <- "51-60"
brent.plot.data$hl_category <- paste(brent.plot.data$n_hl, " layer(s), ", brent.plot.data$nodes_category, " nodes")

brent.plot.data <- aggregate(mape ~ n_lags + hl_category, brent.plot.data, mean)
brent.plot.data$hl_category <- factor(brent.plot.data$hl_category, levels = c("1  layer(s),  <5  nodes", "1  layer(s),  5-10  nodes", "1  layer(s),  11-20  nodes", "2  layer(s),  <5  nodes", "2  layer(s),  5-10  nodes",
                                                                              "2  layer(s),  11-20  nodes", "2  layer(s),  21-30  nodes", "2  layer(s),  31-40  nodes", "3  layer(s),  <5  nodes", "3  layer(s),  5-10  nodes",
                                                                              "3  layer(s),  11-20  nodes", "3  layer(s),  21-30  nodes", "3  layer(s),  31-40  nodes", "3  layer(s),  41-50  nodes", "3  layer(s),  51-60  nodes"))

# Plot
brent.mape.heatmap <- ggplot(data = brent.plot.data, aes(x = hl_category, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "MAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "ANN error heatmap", subtitle = "Brent log returns", xlab = "hidden layer configuration", ylab = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

brent.mape.heatmap

ggsave(filename = "plots/3 Brent error heatmap.pdf", plot = brent.mape.heatmap, device = cairo_pdf)


### Dubai
# Prepare data to plot
dubai.plot.data <- select(tuning.results.dubai.log.returns.forked, -mae)
dubai.plot.data$n_hl <- sapply(dubai.plot.data$hidden_config, function(x){length(unlist(str_split(x, ",")))})
dubai.plot.data$n_nodes <- sapply(dubai.plot.data$hidden_config, function(x){sum(as.numeric(unlist(str_split(x, ","))))})
dubai.plot.data$nodes_category[dubai.plot.data$n_nodes < 5] <- "<5"
dubai.plot.data$nodes_category[dubai.plot.data$n_nodes >= 5 & dubai.plot.data$n_nodes <= 10] <- "5-10"
dubai.plot.data$nodes_category[dubai.plot.data$n_nodes > 10 & dubai.plot.data$n_nodes <= 20] <- "11-20"
dubai.plot.data$nodes_category[dubai.plot.data$n_nodes > 20 & dubai.plot.data$n_nodes <= 30] <- "21-30"
dubai.plot.data$nodes_category[dubai.plot.data$n_nodes > 30 & dubai.plot.data$n_nodes <= 40] <- "31-40"
dubai.plot.data$nodes_category[dubai.plot.data$n_nodes > 40 & dubai.plot.data$n_nodes <= 50] <- "41-50"
dubai.plot.data$nodes_category[dubai.plot.data$n_nodes > 50 & dubai.plot.data$n_nodes <= 60] <- "51-60"
dubai.plot.data$hl_category <- paste(dubai.plot.data$n_hl, " layer(s), ", dubai.plot.data$nodes_category, " nodes")

dubai.plot.data <- aggregate(mape ~ n_lags + hl_category, dubai.plot.data, mean)
dubai.plot.data$hl_category <- factor(dubai.plot.data$hl_category, levels = c("1  layer(s),  <5  nodes", "1  layer(s),  5-10  nodes", "1  layer(s),  11-20  nodes", "2  layer(s),  <5  nodes", "2  layer(s),  5-10  nodes",
                                                                              "2  layer(s),  11-20  nodes", "2  layer(s),  21-30  nodes", "2  layer(s),  31-40  nodes", "3  layer(s),  <5  nodes", "3  layer(s),  5-10  nodes",
                                                                              "3  layer(s),  11-20  nodes", "3  layer(s),  21-30  nodes", "3  layer(s),  31-40  nodes", "3  layer(s),  41-50  nodes", "3  layer(s),  51-60  nodes"))

# Plot
dubai.mape.heatmap <- ggplot(data = dubai.plot.data, aes(x = hl_category, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "MAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "ANN error heatmap", subtitle = "Dubai Fateh log returns", xlab = "hidden layer configuration", ylab = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

dubai.mape.heatmap

ggsave(filename = "plots/3 Dubai error heatmap.pdf", plot = dubai.mape.heatmap, device = cairo_pdf)


### NatGas
# Prepare data to plot
natgas.us.plot.data <- select(tuning.results.natgas.us.log.returns.forked, -mae)
natgas.us.plot.data$n_hl <- sapply(natgas.us.plot.data$hidden_config, function(x){length(unlist(str_split(x, ",")))})
natgas.us.plot.data$n_nodes <- sapply(natgas.us.plot.data$hidden_config, function(x){sum(as.numeric(unlist(str_split(x, ","))))})
natgas.us.plot.data$nodes_category[natgas.us.plot.data$n_nodes < 5] <- "<5"
natgas.us.plot.data$nodes_category[natgas.us.plot.data$n_nodes >= 5 & natgas.us.plot.data$n_nodes <= 10] <- "5-10"
natgas.us.plot.data$nodes_category[natgas.us.plot.data$n_nodes > 10 & natgas.us.plot.data$n_nodes <= 20] <- "11-20"
natgas.us.plot.data$nodes_category[natgas.us.plot.data$n_nodes > 20 & natgas.us.plot.data$n_nodes <= 30] <- "21-30"
natgas.us.plot.data$nodes_category[natgas.us.plot.data$n_nodes > 30 & natgas.us.plot.data$n_nodes <= 40] <- "31-40"
natgas.us.plot.data$nodes_category[natgas.us.plot.data$n_nodes > 40 & natgas.us.plot.data$n_nodes <= 50] <- "41-50"
natgas.us.plot.data$nodes_category[natgas.us.plot.data$n_nodes > 50 & natgas.us.plot.data$n_nodes <= 60] <- "51-60"
natgas.us.plot.data$hl_category <- paste(natgas.us.plot.data$n_hl, " layer(s), ", natgas.us.plot.data$nodes_category, " nodes")

natgas.us.plot.data <- aggregate(mape ~ n_lags + hl_category, natgas.us.plot.data, mean)
natgas.us.plot.data$hl_category <- factor(natgas.us.plot.data$hl_category, levels = c("1  layer(s),  <5  nodes", "1  layer(s),  5-10  nodes", "1  layer(s),  11-20  nodes", "2  layer(s),  <5  nodes", "2  layer(s),  5-10  nodes",
                                                                                      "2  layer(s),  11-20  nodes", "2  layer(s),  21-30  nodes", "2  layer(s),  31-40  nodes", "3  layer(s),  <5  nodes", "3  layer(s),  5-10  nodes",
                                                                                      "3  layer(s),  11-20  nodes", "3  layer(s),  21-30  nodes", "3  layer(s),  31-40  nodes", "3  layer(s),  41-50  nodes", "3  layer(s),  51-60  nodes"))

# Plot
natgas.us.mape.heatmap <- ggplot(data = natgas.us.plot.data, aes(x = hl_category, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "MAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "ANN error heatmap", subtitle = "NatGas US log returns", xlab = "hidden layer configuration", ylab = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

natgas.us.mape.heatmap

ggsave(filename = "plots/3 NatGas error heatmap.pdf", plot = natgas.us.mape.heatmap, device = cairo_pdf)


### WTI
# Prepare data to plot
wti.plot.data <- select(tuning.results.wti.log.returns.forked, -mae)
wti.plot.data$n_hl <- sapply(wti.plot.data$hidden_config, function(x){length(unlist(str_split(x, ",")))})
wti.plot.data$n_nodes <- sapply(wti.plot.data$hidden_config, function(x){sum(as.numeric(unlist(str_split(x, ","))))})
wti.plot.data$nodes_category[wti.plot.data$n_nodes < 5] <- "<5"
wti.plot.data$nodes_category[wti.plot.data$n_nodes >= 5 & wti.plot.data$n_nodes <= 10] <- "5-10"
wti.plot.data$nodes_category[wti.plot.data$n_nodes > 10 & wti.plot.data$n_nodes <= 20] <- "11-20"
wti.plot.data$nodes_category[wti.plot.data$n_nodes > 20 & wti.plot.data$n_nodes <= 30] <- "21-30"
wti.plot.data$nodes_category[wti.plot.data$n_nodes > 30 & wti.plot.data$n_nodes <= 40] <- "31-40"
wti.plot.data$nodes_category[wti.plot.data$n_nodes > 40 & wti.plot.data$n_nodes <= 50] <- "41-50"
wti.plot.data$nodes_category[wti.plot.data$n_nodes > 50 & wti.plot.data$n_nodes <= 60] <- "51-60"
wti.plot.data$hl_category <- paste(wti.plot.data$n_hl, " layer(s), ", wti.plot.data$nodes_category, " nodes")

wti.plot.data <- aggregate(mape ~ n_lags + hl_category, wti.plot.data, mean)
wti.plot.data$hl_category <- factor(wti.plot.data$hl_category, levels = c("1  layer(s),  <5  nodes", "1  layer(s),  5-10  nodes", "1  layer(s),  11-20  nodes", "2  layer(s),  <5  nodes", "2  layer(s),  5-10  nodes",
                                                                          "2  layer(s),  11-20  nodes", "2  layer(s),  21-30  nodes", "2  layer(s),  31-40  nodes", "3  layer(s),  <5  nodes", "3  layer(s),  5-10  nodes",
                                                                          "3  layer(s),  11-20  nodes", "3  layer(s),  21-30  nodes", "3  layer(s),  31-40  nodes", "3  layer(s),  41-50  nodes", "3  layer(s),  51-60  nodes"))

# Plot
wti.mape.heatmap <- ggplot(data = wti.plot.data, aes(x = hl_category, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "MAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "ANN error heatmap", subtitle = "WTI log returns", xlab = "hidden layer configuration", ylab = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

wti.mape.heatmap

ggsave(filename = "plots/3 WTI error heatmap.pdf", plot = wti.mape.heatmap, device = cairo_pdf)
