rm(list = ls())

library(tidyverse)
library(neuralnet)
library(parallel)
library(doParallel)

# Load data
load("clean data/timeseries.RData")

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
  eval(parse(text = paste0("ann <- neuralnet(formula = y ~ ", paste(names(lagged)[-1], collapse = " + "), ", data = lagged[", n_lags + 1, ":(nrow(lagged) - ", n_lags, "),], hidden = ", paste0("c(", paste(hidden_layers, collapse = ", "), ")"), ", threshold = ", target_threshold, ")")))
  
  # Recursively predict using the ANN
  pred <- ann_recursive_pred(model = ann, h = h, lagged.data = lagged[(nrow(lagged) - h + 1), -1])
  # eval(parse(text = paste0("pred <- predict(object = ann, newdata = lagged[", nrow(lagged) - h + 1, ":", nrow(lagged), ",])")))
  
  # Evaluate results
  eval(parse(text = paste0("mae <- mae(lagged$y[", nrow(lagged) - h + 1, ":", nrow(lagged), "], pred)")))
  eval(parse(text = paste0("mape <- mape(lagged$y[", nrow(lagged) - h + 1, ":", nrow(lagged), "], pred)")))
  
  return(list("mae" = mae, "mape" = mape))
}


fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.dubai.monthly.log.returns, hidden_layers = c(h1, h2, h3), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.dubai.log.returns.forked <- mcmapply(FUN = function(l, h1, h2, h3){return(tuning_forked(l, h1, h2, h3))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2, h3 = fork.params$h3)

tuning.results.dubai.log.returns.forked <- as.data.frame(t(tuning.results.dubai.log.returns.forked))
tuning.results.dubai.log.returns.forked$n_lags <- unlist(tuning.results.dubai.log.returns.forked$n_lags)
tuning.results.dubai.log.returns.forked$hidden_config <- unlist(tuning.results.dubai.log.returns.forked$hidden_config)
tuning.results.dubai.log.returns.forked$mae <- unlist(tuning.results.dubai.log.returns.forked$mae)
tuning.results.dubai.log.returns.forked$mape <- unlist(tuning.results.dubai.log.returns.forked$mape)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20)
tuning_forked <- function(l, h1, h2){
  res <- ann_tune(data_ts = ts.dubai.monthly.log.returns, hidden_layers = c(h1, h2), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1, h2){return(tuning_forked(l, h1, h2))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.dubai.log.returns.forked <- rbind(tuning.results.dubai.log.returns.forked, temp)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20)
tuning_forked <- function(l, h1){
  res <- ann_tune(data_ts = ts.dubai.monthly.log.returns, hidden_layers = c(h1), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1){return(tuning_forked(l, h1))}, l = fork.params$l, h1 = fork.params$h1)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.dubai.log.returns.forked <- rbind(tuning.results.dubai.log.returns.forked, temp); rm(temp)

save(tuning.results.dubai.log.returns.forked, file = "results/ml models/Dubai_log_returns_forked.RData")


### US NatGas
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.natgas.us.monthly.log.returns, hidden_layers = c(h1, h2, h3), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.natgas.us.log.returns.forked <- mcmapply(FUN = function(l, h1, h2, h3){return(tuning_forked(l, h1, h2, h3))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2, h3 = fork.params$h3)

tuning.results.natgas.us.log.returns.forked <- as.data.frame(t(tuning.results.natgas.us.log.returns.forked))
tuning.results.natgas.us.log.returns.forked$n_lags <- unlist(tuning.results.natgas.us.log.returns.forked$n_lags)
tuning.results.natgas.us.log.returns.forked$hidden_config <- unlist(tuning.results.natgas.us.log.returns.forked$hidden_config)
tuning.results.natgas.us.log.returns.forked$mae <- unlist(tuning.results.natgas.us.log.returns.forked$mae)
tuning.results.natgas.us.log.returns.forked$mape <- unlist(tuning.results.natgas.us.log.returns.forked$mape)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20)
tuning_forked <- function(l, h1, h2){
  res <- ann_tune(data_ts = ts.natgas.us.monthly.log.returns, hidden_layers = c(h1, h2), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1, h2){return(tuning_forked(l, h1, h2))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.natgas.us.log.returns.forked <- rbind(tuning.results.natgas.us.log.returns.forked, temp)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20)
tuning_forked <- function(l, h1){
  res <- ann_tune(data_ts = ts.natgas.us.monthly.log.returns, hidden_layers = c(h1), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1){return(tuning_forked(l, h1))}, l = fork.params$l, h1 = fork.params$h1)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.natgas.us.log.returns.forked <- rbind(tuning.results.natgas.us.log.returns.forked, temp); rm(temp)

save(tuning.results.natgas.us.log.returns.forked, file = "results/ml models/NatGas_log_returns_forked.RData")


### WTI
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.wti.monthly.log.returns, hidden_layers = c(h1, h2, h3), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.wti.log.returns.forked <- mcmapply(FUN = function(l, h1, h2, h3){return(tuning_forked(l, h1, h2, h3))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2, h3 = fork.params$h3)

tuning.results.wti.log.returns.forked <- as.data.frame(t(tuning.results.wti.log.returns.forked))
tuning.results.wti.log.returns.forked$n_lags <- unlist(tuning.results.wti.log.returns.forked$n_lags)
tuning.results.wti.log.returns.forked$hidden_config <- unlist(tuning.results.wti.log.returns.forked$hidden_config)
tuning.results.wti.log.returns.forked$mae <- unlist(tuning.results.wti.log.returns.forked$mae)
tuning.results.wti.log.returns.forked$mape <- unlist(tuning.results.wti.log.returns.forked$mape)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20)
tuning_forked <- function(l, h1, h2){
  res <- ann_tune(data_ts = ts.wti.monthly.log.returns, hidden_layers = c(h1, h2), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1, h2){return(tuning_forked(l, h1, h2))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.wti.log.returns.forked <- rbind(tuning.results.wti.log.returns.forked, temp)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20)
tuning_forked <- function(l, h1){
  res <- ann_tune(data_ts = ts.wti.monthly.log.returns, hidden_layers = c(h1), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1){return(tuning_forked(l, h1))}, l = fork.params$l, h1 = fork.params$h1)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.wti.log.returns.forked <- rbind(tuning.results.wti.log.returns.forked, temp); rm(temp)

save(tuning.results.wti.log.returns.forked, file = "results/ml models/WTI_log_returns_forked.RData")