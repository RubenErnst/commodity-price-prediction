rm(list = ls())

library(tidyverse)
library(neuralnet)
library(parallel)
library(doParallel)
library(doSNOW)


# Load data
load("/Users/rubenernst/OneDrive - Universität St.Gallen/BA/commodities-pricing/clean data/timeseries.RData")

# Target functions
mape <- function(y, pred){
  out <- mean(abs((y - pred) / y))
  return(out)
}

mae <- function(y, pred){
  out <- mean(abs(y - pred))
  return(out)
}

# Dealing with ts times
conv_to_ym <- function(ts_date){
  year <- floor(ts_date)
  month <- ts_date - year
  return(c(year, round(month * 12) + 1))
}

# Recursive ANN prediction
ann_recursive_pred <- function(model, h, lagged.data){
  # Prepare newdata
  newdata <- as.data.frame(matrix(data = NA, nrow = h, ncol = ncol(lagged.data)))
  names(newdata) <- names(lagged.data)
  
  for (i in 1:(nrow(newdata))){
    if (ncol(newdata) - i + 1 > 0){
      newdata[i,] <- c(rep(NA, c(ncol(newdata), i - 1)[which.min(c(ncol(newdata), i - 1))]), as.numeric(lagged.data[1:(ncol(newdata) - i + 1)]))
    }
  }
  
  pred <- c()
  for (i in 1:nrow(newdata)){
    pred <- c(predict(object = model, newdata = newdata[i,])[[1]], pred)
    newdata[(i + 1), 1:c(ncol(newdata), i)[which.min(c(ncol(newdata), i))]] <- pred[1:c(ncol(newdata), i)[which.min(c(ncol(newdata), i))]]
  }
  return(rev(pred))
}

ann_tune <- function(data_ts, hidden_layers, target_threshold, h, n_lags){
  # Build lagged dataset
  lagged <- data.frame("y" = data_ts[1:length(data_ts)])
  for (i in 1:n_lags){
    eval(parse(text = paste0("lagged$l", i, " = lag(data_ts[1:length(data_ts)], ", i, ")")))
  }
  
  # Train ANN
  set.seed(42)
  eval(parse(text = paste0("ann <- neuralnet(formula = y ~ ", paste(names(lagged)[-1], collapse = " + "), ", data = lagged[", n_lags + 1, ":(nrow(lagged) - ", n_lags, "),], hidden = ", paste0("c(", paste(hidden_layers, collapse = ", "), ")"), ", threshold = ", target_threshold, ", lifesign = 'full')")))
  
  # Recursively predict using the ANN
  pred <- ann_recursive_pred(model = ann, h = h, lagged.data = lagged[(nrow(lagged) - h + 1), -1])
  # eval(parse(text = paste0("pred <- predict(object = ann, newdata = lagged[", nrow(lagged) - h + 1, ":", nrow(lagged), ",])")))
  
  # Evaluate results
  eval(parse(text = paste0("mae <- mae(lagged$y[", nrow(lagged) - h + 1, ":", nrow(lagged), "], pred)")))
  eval(parse(text = paste0("mape <- mape(lagged$y[", nrow(lagged) - h + 1, ":", nrow(lagged), "], pred)")))
  
  return(list("mae" = mae, "mape" = mape))
}


# ##### Cluster -----
# cl <- makeSOCKcluster(8)
# registerDoSNOW(cl)
# opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/120000"))})
# 
# hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
# my_fun <- function(l, hl){
#   res <- ann_tune(data_ts = ts.apsp.monthly.log.returns, hidden_layers = hl, 0.01, 12, l)
#   print(paste0("Finished l: ", l, ", hl: ", paste(hl, collapse = ", ")))
#   return(data.frame("n_lags" = l, "hidden_config" = paste0(hl, collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
# }
# 
# # registerDoParallel(8)  # use multicore, set to the number of our cores
# tuning.results.natgas.us.log.returns <- foreach (l = 5:20, .combine = rbind) %:%
#   foreach(hl = 1:20, .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
#     my_fun(l, hl_combinations[hl,])
#   }
# 
# stopImplicitCluster()




##### Forking -----
library(lme4)
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
my_fun <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.brent.monthly.log.returns, hidden_layers = c(h1, h2, h3), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.forked <- mcmapply(FUN = function(l, h1, h2, h3){return(my_fun(l, h1, h2, h3))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2, h3 = fork.params$h3)

save(tuning.results.forked, file = "results/ml models/Brent_log_returns_forked.RData")