rm(list = ls())

library(tidyverse)
library(neuralnet)

load("results/ml models/ANN pred.RData")


# Plot one neural net per commodity
best.ann.apsp$model.list[["variables"]] <- paste0("lag", 1:length(best.ann.apsp$model.list[["variables"]]), sep = "")
cairo_pdf("plots/5_APSP_ANN_plot.pdf")
plot(best.ann.apsp, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
dev.off()

best.ann.brent$model.list[["variables"]] <- paste0("lag", 1:length(best.ann.brent$model.list[["variables"]]), sep = "")
cairo_pdf("plots/5_Brent_ANN_plot.pdf")
plot(best.ann.brent, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
dev.off()

best.ann.dubai$model.list[["variables"]] <- paste0("lag", 1:length(best.ann.dubai$model.list[["variables"]]), sep = "")
cairo_pdf("plots/5_Dubai_ANN_plot.pdf")
plot(best.ann.dubai, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
dev.off()

best.ann.lng$model.list[["variables"]] <- paste0("lag", 1:length(best.ann.lng$model.list[["variables"]]), sep = "")
cairo_pdf("plots/5_LNG_ANN_plot.pdf")
plot(best.ann.lng, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
dev.off()

best.ann.natgas.us$model.list[["variables"]] <- paste0("lag", 1:length(best.ann.natgas.us$model.list[["variables"]]), sep = "")
cairo_pdf("plots/5_NatGas_ANN_plot.pdf")
plot(best.ann.natgas.us, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
dev.off()

best.ann.wti$model.list[["variables"]] <- paste0("lag", 1:length(best.ann.wti$model.list[["variables"]]), sep = "")
cairo_pdf("plots/5_WTI_ANN_plot.pdf")
plot(best.ann.wti, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
dev.off()


# Plot all of them together
cairo_pdf("plots/999_test.pdf", width = 12, height = 8)
layout(mat = matrix(data = 1:12, nrow = 4, ncol = 3, byrow = TRUE), heights = c(1, 5, 1, 5))
par(mar=c(0, 0, 0, 0))
plot.new()
text(0.5, 0.8, "APSP", cex = 1.5, font = 2)
plot.new()
text(0.5, 0.8, "Brent", cex = 1.5, font = 2)
plot.new()
text(0.5, 0.8, "Dubai Fateh", cex = 1.5, font = 2)
plot(best.ann.apsp, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
plot(best.ann.brent, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
plot(best.ann.dubai, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
plot.new()
text(0.5, 0, "LNG Asia", cex = 1.5, font = 2)
plot.new()
text(0.5, 0, "NatGas US", cex = 1.5, font = 2)
plot.new()
text(0.5, 0, "WTI", cex = 1.5, font = 2)
plot(best.ann.lng, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
plot(best.ann.natgas.us, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
plot(best.ann.wti, show.weights = FALSE, rep = "best", arrow.length = 0.16, dimension = 8)
dev.off()
