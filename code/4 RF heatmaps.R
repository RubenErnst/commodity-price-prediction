rm(list = ls())

library(tidyverse)
library(viridis)
library(ggpubr)

load("results/ml models/RF_APSP_log_returns_forked.RData")
load("results/ml models/RF_Brent_log_returns_forked.RData")
load("results/ml models/RF_Dubai_log_returns_forked.RData")
load("results/ml models/RF_NatGas_log_returns_forked.RData")
load("results/ml models/RF_WTI_log_returns_forked.RData")


### APSP
# Prepare data to plot
apsp.plot.data <- select(tuning.results.apsp.log.returns.forked, -mae)
apsp.plot.data$n_tree_category[apsp.plot.data$ntree <= 50] <- "1-50"
apsp.plot.data$n_tree_category[apsp.plot.data$ntree > 50 & apsp.plot.data$ntree <= 100] <- "51-100"
apsp.plot.data$n_tree_category[apsp.plot.data$ntree > 100 & apsp.plot.data$ntree <= 150] <- "101-150"
apsp.plot.data$n_tree_category[apsp.plot.data$ntree > 150 & apsp.plot.data$ntree <= 200] <- "151-200"
apsp.plot.data$n_tree_category[apsp.plot.data$ntree > 200] <- "201-250"
apsp.plot.data$config <- paste0(apsp.plot.data$n_tree_category, " trees, mtry = ", apsp.plot.data$mtry)

apsp.plot.data <- aggregate(mape ~ config + n_lags, apsp.plot.data, median)
apsp.plot.data$config <- factor(apsp.plot.data$config, levels = c("1-50 trees, mtry = 1", "1-50 trees, mtry = 2", "1-50 trees, mtry = 3", "1-50 trees, mtry = 4", "1-50 trees, mtry = 5", "51-100 trees, mtry = 1",
                                                                  "51-100 trees, mtry = 2", "51-100 trees, mtry = 3", "51-100 trees, mtry = 4", "51-100 trees, mtry = 5", "101-150 trees, mtry = 1", "101-150 trees, mtry = 2",
                                                                  "101-150 trees, mtry = 3", "101-150 trees, mtry = 4", "101-150 trees, mtry = 5", "151-200 trees, mtry = 1", "151-200 trees, mtry = 2",
                                                                  "151-200 trees, mtry = 3", "151-200 trees, mtry = 4", "151-200 trees, mtry = 5", "201-250 trees, mtry = 1", "201-250 trees, mtry = 2",
                                                                  "201-250 trees, mtry = 3", "201-250 trees, mtry = 4", "201-250 trees, mtry = 5"))

# Plot
apsp.mape.heatmap <- ggplot(data = apsp.plot.data, aes(x = config, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "median\nMAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "APSP", x = "hyperparameter configuration", y = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.05, 0, 0, 0.35), "in"))

apsp.mape.heatmap

# ggsave(filename = "plots/3 RF APSP error heatmap.pdf", plot = apsp.mape.heatmap, device = cairo_pdf)


### Brent
# Prepare data to plot
brent.plot.data <- select(tuning.results.brent.log.returns.forked, -mae)
brent.plot.data$n_tree_category[brent.plot.data$ntree <= 50] <- "1-50"
brent.plot.data$n_tree_category[brent.plot.data$ntree > 50 & brent.plot.data$ntree <= 100] <- "51-100"
brent.plot.data$n_tree_category[brent.plot.data$ntree > 100 & brent.plot.data$ntree <= 150] <- "101-150"
brent.plot.data$n_tree_category[brent.plot.data$ntree > 150 & brent.plot.data$ntree <= 200] <- "151-200"
brent.plot.data$n_tree_category[brent.plot.data$ntree > 200] <- "201-250"
brent.plot.data$config <- paste0(brent.plot.data$n_tree_category, " trees, mtry = ", brent.plot.data$mtry)

brent.plot.data <- aggregate(mape ~ config + n_lags, brent.plot.data, median)
brent.plot.data$config <- factor(brent.plot.data$config, levels = c("1-50 trees, mtry = 1", "1-50 trees, mtry = 2", "1-50 trees, mtry = 3", "1-50 trees, mtry = 4", "1-50 trees, mtry = 5", "51-100 trees, mtry = 1",
                                                                    "51-100 trees, mtry = 2", "51-100 trees, mtry = 3", "51-100 trees, mtry = 4", "51-100 trees, mtry = 5", "101-150 trees, mtry = 1", "101-150 trees, mtry = 2",
                                                                    "101-150 trees, mtry = 3", "101-150 trees, mtry = 4", "101-150 trees, mtry = 5", "151-200 trees, mtry = 1", "151-200 trees, mtry = 2",
                                                                    "151-200 trees, mtry = 3", "151-200 trees, mtry = 4", "151-200 trees, mtry = 5", "201-250 trees, mtry = 1", "201-250 trees, mtry = 2",
                                                                    "201-250 trees, mtry = 3", "201-250 trees, mtry = 4", "201-250 trees, mtry = 5"))

# Plot
brent.mape.heatmap <- ggplot(data = brent.plot.data, aes(x = config, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "median\nMAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "Brent", x = "hyperparameter configuration", y = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.05, 0, 0, 0.35), "in"))

brent.mape.heatmap

# ggsave(filename = "plots/3 RF Brent error heatmap.pdf", plot = brent.mape.heatmap, device = cairo_pdf)


### Dubai
# Prepare data to plot
dubai.plot.data <- select(tuning.results.dubai.log.returns.forked, -mae)
dubai.plot.data$n_tree_category[dubai.plot.data$ntree <= 50] <- "1-50"
dubai.plot.data$n_tree_category[dubai.plot.data$ntree > 50 & dubai.plot.data$ntree <= 100] <- "51-100"
dubai.plot.data$n_tree_category[dubai.plot.data$ntree > 100 & dubai.plot.data$ntree <= 150] <- "101-150"
dubai.plot.data$n_tree_category[dubai.plot.data$ntree > 150 & dubai.plot.data$ntree <= 200] <- "151-200"
dubai.plot.data$n_tree_category[dubai.plot.data$ntree > 200] <- "201-250"
dubai.plot.data$config <- paste0(dubai.plot.data$n_tree_category, " trees, mtry = ", dubai.plot.data$mtry)

dubai.plot.data <- aggregate(mape ~ config + n_lags, dubai.plot.data, median)
dubai.plot.data$config <- factor(dubai.plot.data$config, levels = c("1-50 trees, mtry = 1", "1-50 trees, mtry = 2", "1-50 trees, mtry = 3", "1-50 trees, mtry = 4", "1-50 trees, mtry = 5", "51-100 trees, mtry = 1",
                                                                    "51-100 trees, mtry = 2", "51-100 trees, mtry = 3", "51-100 trees, mtry = 4", "51-100 trees, mtry = 5", "101-150 trees, mtry = 1", "101-150 trees, mtry = 2",
                                                                    "101-150 trees, mtry = 3", "101-150 trees, mtry = 4", "101-150 trees, mtry = 5", "151-200 trees, mtry = 1", "151-200 trees, mtry = 2",
                                                                    "151-200 trees, mtry = 3", "151-200 trees, mtry = 4", "151-200 trees, mtry = 5", "201-250 trees, mtry = 1", "201-250 trees, mtry = 2",
                                                                    "201-250 trees, mtry = 3", "201-250 trees, mtry = 4", "201-250 trees, mtry = 5"))

# Plot
dubai.mape.heatmap <- ggplot(data = dubai.plot.data, aes(x = config, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "median\nMAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "Dubai Fateh", x = "hyperparameter configuration", y = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.05, 0, 0, 0.35), "in"))

dubai.mape.heatmap

# ggsave(filename = "plots/3 RF Dubai error heatmap.pdf", plot = dubai.mape.heatmap, device = cairo_pdf)


### LNG
# Prepare data to plot
lng.plot.data <- select(tuning.results.lng.log.returns.forked, -mae)
lng.plot.data$n_tree_category[lng.plot.data$ntree <= 50] <- "1-50"
lng.plot.data$n_tree_category[lng.plot.data$ntree > 50 & lng.plot.data$ntree <= 100] <- "51-100"
lng.plot.data$n_tree_category[lng.plot.data$ntree > 100 & lng.plot.data$ntree <= 150] <- "101-150"
lng.plot.data$n_tree_category[lng.plot.data$ntree > 150 & lng.plot.data$ntree <= 200] <- "151-200"
lng.plot.data$n_tree_category[lng.plot.data$ntree > 200] <- "201-250"
lng.plot.data$config <- paste0(lng.plot.data$n_tree_category, " trees, mtry = ", lng.plot.data$mtry)

lng.plot.data <- aggregate(mape ~ config + n_lags, lng.plot.data, median)
lng.plot.data$config <- factor(lng.plot.data$config, levels = c("1-50 trees, mtry = 1", "1-50 trees, mtry = 2", "1-50 trees, mtry = 3", "1-50 trees, mtry = 4", "1-50 trees, mtry = 5", "51-100 trees, mtry = 1",
                                                                    "51-100 trees, mtry = 2", "51-100 trees, mtry = 3", "51-100 trees, mtry = 4", "51-100 trees, mtry = 5", "101-150 trees, mtry = 1", "101-150 trees, mtry = 2",
                                                                    "101-150 trees, mtry = 3", "101-150 trees, mtry = 4", "101-150 trees, mtry = 5", "151-200 trees, mtry = 1", "151-200 trees, mtry = 2",
                                                                    "151-200 trees, mtry = 3", "151-200 trees, mtry = 4", "151-200 trees, mtry = 5", "201-250 trees, mtry = 1", "201-250 trees, mtry = 2",
                                                                    "201-250 trees, mtry = 3", "201-250 trees, mtry = 4", "201-250 trees, mtry = 5"))

# Plot
lng.mape.heatmap <- ggplot(data = lng.plot.data, aes(x = config, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "median\nMAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "LNG Asia", x = "hyperparameter configuration", y = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.05, 0, 0, 0.35), "in"))

lng.mape.heatmap

ggsave(filename = "plots/3 RF LNG error heatmap.pdf", plot = lng.mape.heatmap, device = cairo_pdf)


### NatGas
# Prepare data to plot
natgas.us.plot.data <- select(tuning.results.natgas.us.log.returns.forked, -mae)
natgas.us.plot.data$n_tree_category[natgas.us.plot.data$ntree <= 50] <- "1-50"
natgas.us.plot.data$n_tree_category[natgas.us.plot.data$ntree > 50 & natgas.us.plot.data$ntree <= 100] <- "51-100"
natgas.us.plot.data$n_tree_category[natgas.us.plot.data$ntree > 100 & natgas.us.plot.data$ntree <= 150] <- "101-150"
natgas.us.plot.data$n_tree_category[natgas.us.plot.data$ntree > 150 & natgas.us.plot.data$ntree <= 200] <- "151-200"
natgas.us.plot.data$n_tree_category[natgas.us.plot.data$ntree > 200] <- "201-250"
natgas.us.plot.data$config <- paste0(natgas.us.plot.data$n_tree_category, " trees, mtry = ", natgas.us.plot.data$mtry)

natgas.us.plot.data <- aggregate(mape ~ config + n_lags, natgas.us.plot.data, median)
natgas.us.plot.data$config <- factor(natgas.us.plot.data$config, levels = c("1-50 trees, mtry = 1", "1-50 trees, mtry = 2", "1-50 trees, mtry = 3", "1-50 trees, mtry = 4", "1-50 trees, mtry = 5", "51-100 trees, mtry = 1",
                                                                            "51-100 trees, mtry = 2", "51-100 trees, mtry = 3", "51-100 trees, mtry = 4", "51-100 trees, mtry = 5", "101-150 trees, mtry = 1", "101-150 trees, mtry = 2",
                                                                            "101-150 trees, mtry = 3", "101-150 trees, mtry = 4", "101-150 trees, mtry = 5", "151-200 trees, mtry = 1", "151-200 trees, mtry = 2",
                                                                            "151-200 trees, mtry = 3", "151-200 trees, mtry = 4", "151-200 trees, mtry = 5", "201-250 trees, mtry = 1", "201-250 trees, mtry = 2",
                                                                            "201-250 trees, mtry = 3", "201-250 trees, mtry = 4", "201-250 trees, mtry = 5"))

# Plot
natgas.us.mape.heatmap <- ggplot(data = natgas.us.plot.data, aes(x = config, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "median\nMAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "Natural Gas Henry Hub", x = "hyperparameter configuration", y = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.05, 0, 0, 0.35), "in"))

natgas.us.mape.heatmap

# ggsave(filename = "plots/3 RF NatGas error heatmap.pdf", plot = natgas.us.mape.heatmap, device = cairo_pdf)


### WTI
# Prepare data to plot
wti.plot.data <- select(tuning.results.wti.log.returns.forked, -mae)
wti.plot.data$n_tree_category[wti.plot.data$ntree <= 50] <- "1-50"
wti.plot.data$n_tree_category[wti.plot.data$ntree > 50 & wti.plot.data$ntree <= 100] <- "51-100"
wti.plot.data$n_tree_category[wti.plot.data$ntree > 100 & wti.plot.data$ntree <= 150] <- "101-150"
wti.plot.data$n_tree_category[wti.plot.data$ntree > 150 & wti.plot.data$ntree <= 200] <- "151-200"
wti.plot.data$n_tree_category[wti.plot.data$ntree > 200] <- "201-250"
wti.plot.data$config <- paste0(wti.plot.data$n_tree_category, " trees, mtry = ", wti.plot.data$mtry)

wti.plot.data <- aggregate(mape ~ config + n_lags, wti.plot.data, median)
wti.plot.data$config <- factor(wti.plot.data$config, levels = c("1-50 trees, mtry = 1", "1-50 trees, mtry = 2", "1-50 trees, mtry = 3", "1-50 trees, mtry = 4", "1-50 trees, mtry = 5", "51-100 trees, mtry = 1",
                                                                "51-100 trees, mtry = 2", "51-100 trees, mtry = 3", "51-100 trees, mtry = 4", "51-100 trees, mtry = 5", "101-150 trees, mtry = 1", "101-150 trees, mtry = 2",
                                                                "101-150 trees, mtry = 3", "101-150 trees, mtry = 4", "101-150 trees, mtry = 5", "151-200 trees, mtry = 1", "151-200 trees, mtry = 2",
                                                                "151-200 trees, mtry = 3", "151-200 trees, mtry = 4", "151-200 trees, mtry = 5", "201-250 trees, mtry = 1", "201-250 trees, mtry = 2",
                                                                "201-250 trees, mtry = 3", "201-250 trees, mtry = 4", "201-250 trees, mtry = 5"))

# Plot
wti.mape.heatmap <- ggplot(data = wti.plot.data, aes(x = config, y = n_lags, fill = mape)) + 
  geom_tile() +
  scale_fill_viridis(name = "median\nMAPE", option = "magma", discrete = FALSE, direction = -1) +
  labs(title = "WTI", x = "hyperparameter configuration", y = "nr. of lags used") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", color = "#000000", angle = 45, hjust = 1, vjust = 1.2),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.05, 0, 0, 0.35), "in"))

wti.mape.heatmap

# ggsave(filename = "plots/3 RF WTI error heatmap.pdf", plot = wti.mape.heatmap, device = cairo_pdf)


### Clustered version
rf.heatmaps <- ggarrange(apsp.mape.heatmap, brent.mape.heatmap, dubai.mape.heatmap, lng.mape.heatmap, natgas.us.mape.heatmap, wti.mape.heatmap,
                         ncol = 2, nrow = 3)

rf.heatmaps

ggsave(filename = "plots/3 RF combined error heatmaps.pdf", plot = rf.heatmaps, device = cairo_pdf, width = 9, height = 12)
