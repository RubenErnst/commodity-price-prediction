rm(list = ls())

library(tidyverse)
library(tsibble)
library(forecast)
library(randomForest)
library(parallel)
library(doParallel)


# Load data
load("clean data/timeseries.RData")

# Dealing with ts times
conv_to_ym <- function(ts_date){
  year <- floor(ts_date)
  month <- ts_date - year
  return(c(year, round(month * 12) + 1))
}

# Target functions
mape <- function(y, pred){
  out <- mean(abs((y - pred) / y))
  return(out)
}

mae <- function(y, pred){
  out <- mean(abs(y - pred))
  return(out)
}

# Recursive RF prediction
rf_recursive_pred <- function(model, h, lagged.data){
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

rf_tune <- function(data_ts, ntree, mtry, h, n_lags, series = "price"){
  # Build lagged dataset
  if (series == "price"){
    lagged <- data.frame("y" = data_ts[1:length(data_ts)])
    for (i in 1:n_lags){
      eval(parse(text = paste0("lagged$l", i, " = lag(data_ts[1:length(data_ts)], ", i, ")")))
    }
  } else if (series == "diff"){
    lagged <- data.frame("y" = diff(data_ts[1:length(data_ts)], 1))
    for (i in 1:n_lags){
      eval(parse(text = paste0("lagged$l", i, " = lag(lagged$y[1:nrow(lagged)], ", i, ")")))
    }
  } else if (series == "log.return"){
    lagged <- data.frame("y" = log(data_ts[1:length(data_ts)] / lag(data_ts[1:length(data_ts)]))[-1])
    for (i in 1:n_lags){
      eval(parse(text = paste0("lagged$l", i, " = lag(lagged$y[1:nrow(lagged)], ", i, ")")))
    }
  }
  
  
  # Train RF
  set.seed(42)
  rf <- randomForest(x = lagged[(n_lags + 1):(nrow(lagged) - n_lags),-1], y = lagged$y[(n_lags + 1):(nrow(lagged) - n_lags)], ntree = ntree, mtry = mtry)
  
  # Recursively predict using the RF
  pred <- rf_recursive_pred(model = rf, h = h, lagged.data = lagged[(nrow(lagged) - h + 1), -1])
  
  # Evaluate results
  if (series == "price"){
    eval(parse(text = paste0("mae <- mae(lagged$y[", nrow(lagged) - h + 1, ":", nrow(lagged), "], pred)")))
    eval(parse(text = paste0("mape <- mape(lagged$y[", nrow(lagged) - h + 1, ":", nrow(lagged), "], pred)")))
  } else if (series == "diff"){
    mae <- mae(data_ts[(nrow(lagged) - h + 2):(nrow(lagged) + 1)], cumsum(c(data_ts[(nrow(lagged) - h + 1)], pred))[-1])
    mape <- mape(data_ts[(nrow(lagged) - h + 2):(nrow(lagged) + 1)], cumsum(c(data_ts[(nrow(lagged) - h + 1)], pred))[-1])
  } else if (series == "log.return"){
    mae <- mae(data_ts[(nrow(lagged) - h + 2):(nrow(lagged) + 1)], data_ts[(nrow(lagged) - h + 1)] * cumprod(exp(pred)))
    mape <- mape(data_ts[(nrow(lagged) - h + 2):(nrow(lagged) + 1)], data_ts[(nrow(lagged) - h + 1)] * cumprod(exp(pred)))
  } else {
    mae <- NA
    mape <- NA
  }
  
  return(list("mae" = mae, "mape" = mape))
}

### APSP
fork.params <- expand.grid("l" = 2:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.apsp.monthly.absolute, ntree = ntree, mtry = mtry, h = 12, n_lags = l, series = "log.return")
  print(paste0("Finished l: ", l, ", ntree: ", ntree, ", mtry: ", mtry))
  return(data.frame("n_lags" = l, "ntree" = ntree, "mtry" = mtry, "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.apsp.log.returns.forked <- mcmapply(FUN = function(l, ntree, mtry){return(tuning_forked(l, ntree, mtry))}, l = fork.params$l, ntree = fork.params$ntree, mtry = fork.params$mtry)

tuning.results.apsp.log.returns.forked <- as.data.frame(t(tuning.results.apsp.log.returns.forked))
tuning.results.apsp.log.returns.forked$n_lags <- unlist(tuning.results.apsp.log.returns.forked$n_lags)
tuning.results.apsp.log.returns.forked$ntree <- unlist(tuning.results.apsp.log.returns.forked$ntree)
tuning.results.apsp.log.returns.forked$mtry <- unlist(tuning.results.apsp.log.returns.forked$mtry)
tuning.results.apsp.log.returns.forked$mae <- unlist(tuning.results.apsp.log.returns.forked$mae)
tuning.results.apsp.log.returns.forked$mape <- unlist(tuning.results.apsp.log.returns.forked$mape)


save(tuning.results.apsp.log.returns.forked, file = "results/ml models/RF_APSP_log_returns_forked.RData")

### Brent
fork.params <- expand.grid("l" = 2:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.brent.monthly.absolute, ntree = ntree, mtry = mtry, h = 12, n_lags = l, series = "log.return")
  print(paste0("Finished l: ", l, ", ntree: ", ntree, ", mtry: ", mtry))
  return(data.frame("n_lags" = l, "ntree" = ntree, "mtry" = mtry, "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.brent.log.returns.forked <- mcmapply(FUN = function(l, ntree, mtry){return(tuning_forked(l, ntree, mtry))}, l = fork.params$l, ntree = fork.params$ntree, mtry = fork.params$mtry)

tuning.results.brent.log.returns.forked <- as.data.frame(t(tuning.results.brent.log.returns.forked))
tuning.results.brent.log.returns.forked$n_lags <- unlist(tuning.results.brent.log.returns.forked$n_lags)
tuning.results.brent.log.returns.forked$ntree <- unlist(tuning.results.brent.log.returns.forked$ntree)
tuning.results.brent.log.returns.forked$mtry <- unlist(tuning.results.brent.log.returns.forked$mtry)
tuning.results.brent.log.returns.forked$mae <- unlist(tuning.results.brent.log.returns.forked$mae)
tuning.results.brent.log.returns.forked$mape <- unlist(tuning.results.brent.log.returns.forked$mape)


save(tuning.results.brent.log.returns.forked, file = "results/ml models/RF_Brent_log_returns_forked.RData")

### Dubai
fork.params <- expand.grid("l" = 2:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.dubai.monthly.absolute, ntree = ntree, mtry = mtry, h = 12, n_lags = l, series = "log.return")
  print(paste0("Finished l: ", l, ", ntree: ", ntree, ", mtry: ", mtry))
  return(data.frame("n_lags" = l, "ntree" = ntree, "mtry" = mtry, "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.dubai.log.returns.forked <- mcmapply(FUN = function(l, ntree, mtry){return(tuning_forked(l, ntree, mtry))}, l = fork.params$l, ntree = fork.params$ntree, mtry = fork.params$mtry)

tuning.results.dubai.log.returns.forked <- as.data.frame(t(tuning.results.dubai.log.returns.forked))
tuning.results.dubai.log.returns.forked$n_lags <- unlist(tuning.results.dubai.log.returns.forked$n_lags)
tuning.results.dubai.log.returns.forked$ntree <- unlist(tuning.results.dubai.log.returns.forked$ntree)
tuning.results.dubai.log.returns.forked$mtry <- unlist(tuning.results.dubai.log.returns.forked$mtry)
tuning.results.dubai.log.returns.forked$mae <- unlist(tuning.results.dubai.log.returns.forked$mae)
tuning.results.dubai.log.returns.forked$mape <- unlist(tuning.results.dubai.log.returns.forked$mape)


save(tuning.results.dubai.log.returns.forked, file = "results/ml models/RF_Dubai_log_returns_forked.RData")

### LNG
fork.params <- expand.grid("l" = 2:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.lng.monthly.absolute, ntree = ntree, mtry = mtry, h = 12, n_lags = l, series = "log.return")
  print(paste0("Finished l: ", l, ", ntree: ", ntree, ", mtry: ", mtry))
  return(data.frame("n_lags" = l, "ntree" = ntree, "mtry" = mtry, "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.lng.log.returns.forked <- mcmapply(FUN = function(l, ntree, mtry){return(tuning_forked(l, ntree, mtry))}, l = fork.params$l, ntree = fork.params$ntree, mtry = fork.params$mtry)

tuning.results.lng.log.returns.forked <- as.data.frame(t(tuning.results.lng.log.returns.forked))
tuning.results.lng.log.returns.forked$n_lags <- unlist(tuning.results.lng.log.returns.forked$n_lags)
tuning.results.lng.log.returns.forked$ntree <- unlist(tuning.results.lng.log.returns.forked$ntree)
tuning.results.lng.log.returns.forked$mtry <- unlist(tuning.results.lng.log.returns.forked$mtry)
tuning.results.lng.log.returns.forked$mae <- unlist(tuning.results.lng.log.returns.forked$mae)
tuning.results.lng.log.returns.forked$mape <- unlist(tuning.results.lng.log.returns.forked$mape)


save(tuning.results.lng.log.returns.forked, file = "results/ml models/RF_LNG_log_returns_forked.RData")

### NatGas
fork.params <- expand.grid("l" = 2:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.natgas.us.monthly.absolute, ntree = ntree, mtry = mtry, h = 12, n_lags = l, series = "log.return")
  print(paste0("Finished l: ", l, ", ntree: ", ntree, ", mtry: ", mtry))
  return(data.frame("n_lags" = l, "ntree" = ntree, "mtry" = mtry, "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.natgas.us.log.returns.forked <- mcmapply(FUN = function(l, ntree, mtry){return(tuning_forked(l, ntree, mtry))}, l = fork.params$l, ntree = fork.params$ntree, mtry = fork.params$mtry)

tuning.results.natgas.us.log.returns.forked <- as.data.frame(t(tuning.results.natgas.us.log.returns.forked))
tuning.results.natgas.us.log.returns.forked$n_lags <- unlist(tuning.results.natgas.us.log.returns.forked$n_lags)
tuning.results.natgas.us.log.returns.forked$ntree <- unlist(tuning.results.natgas.us.log.returns.forked$ntree)
tuning.results.natgas.us.log.returns.forked$mtry <- unlist(tuning.results.natgas.us.log.returns.forked$mtry)
tuning.results.natgas.us.log.returns.forked$mae <- unlist(tuning.results.natgas.us.log.returns.forked$mae)
tuning.results.natgas.us.log.returns.forked$mape <- unlist(tuning.results.natgas.us.log.returns.forked$mape)


save(tuning.results.natgas.us.log.returns.forked, file = "results/ml models/RF_NatGas_log_returns_forked.RData")

### WTI
fork.params <- expand.grid("l" = 2:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.wti.monthly.absolute, ntree = ntree, mtry = mtry, h = 12, n_lags = l, series = "log.return")
  print(paste0("Finished l: ", l, ", ntree: ", ntree, ", mtry: ", mtry))
  return(data.frame("n_lags" = l, "ntree" = ntree, "mtry" = mtry, "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.wti.log.returns.forked <- mcmapply(FUN = function(l, ntree, mtry){return(tuning_forked(l, ntree, mtry))}, l = fork.params$l, ntree = fork.params$ntree, mtry = fork.params$mtry)

tuning.results.wti.log.returns.forked <- as.data.frame(t(tuning.results.wti.log.returns.forked))
tuning.results.wti.log.returns.forked$n_lags <- unlist(tuning.results.wti.log.returns.forked$n_lags)
tuning.results.wti.log.returns.forked$ntree <- unlist(tuning.results.wti.log.returns.forked$ntree)
tuning.results.wti.log.returns.forked$mtry <- unlist(tuning.results.wti.log.returns.forked$mtry)
tuning.results.wti.log.returns.forked$mae <- unlist(tuning.results.wti.log.returns.forked$mae)
tuning.results.wti.log.returns.forked$mape <- unlist(tuning.results.wti.log.returns.forked$mape)


save(tuning.results.wti.log.returns.forked, file = "results/ml models/RF_WTI_log_returns_forked.RData")

