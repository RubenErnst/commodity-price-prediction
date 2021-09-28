rm(list = ls())

library(tidyverse)
library(neuralnet)

load("clean data/timeseries.RData")

apsp.log.returns <- data.frame("y" = log(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)] / lag(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)])))
for (i in 1:12){
  eval(parse(text = paste0("apsp.log.returns$l", i, " = lag(apsp.log.returns$y[1:nrow(apsp.log.returns)], ", i, ")")))
}
set.seed(42)
ann <- neuralnet(formula = y ~ l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 + l10 + l11 + l12,
                 data = apsp.log.returns[14:362,],
                 hidden = c(15, 10, 5),
                 threshold = 0.01,
                 lifesign = "minimal")

ann.plot <- plot(ann, plots = "net", show.weights = FALSE, custon = TRUE, file = "test ann plot.pdf", device = cairo_pdf)
