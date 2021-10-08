rm(list = ls())

library(devtools)
library(tidyverse)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

load("results/ml models/ANN pred.RData")


### Plot
plot.nnet(best.ann.apsp, x.lab = c("lag 1", "lag 2", "lag 3", "lag 4", "lag 5", "lag 6"), circle.col = list("black", "gray", "white", "red"))
