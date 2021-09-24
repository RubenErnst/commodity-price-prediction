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

rf_tune <- function(data_ts, ntree, mtry, h, n_lags){
  # Build lagged dataset
  lagged <- data.frame("y" = data_ts[1:length(data_ts)])
  for (i in 1:n_lags){
    eval(parse(text = paste0("lagged$l", i, " = lag(data_ts[1:length(data_ts)], ", i, ")")))
  }
  
  # Train ANN
  set.seed(42)
  eval(parse(text = paste0("rf <- randomForest(x = lagged[", n_lags + 1, ":(nrow(lagged) - ", n_lags, "),-1], y = lagged$y[", n_lags + 1, ":(nrow(lagged) - ", n_lags, ")], ntree = ", ntree, ", mtry = ", mtry, ")")))
  
  # Recursively predict using the ANN
  pred <- rf_recursive_pred(model = rf, h = h, lagged.data = lagged[(nrow(lagged) - h + 1), -1])
  # eval(parse(text = paste0("pred <- predict(object = ann, newdata = lagged[", nrow(lagged) - h + 1, ":", nrow(lagged), ",])")))
  
  # Evaluate results
  eval(parse(text = paste0("mae <- mae(lagged$y[", nrow(lagged) - h + 1, ":", nrow(lagged), "], pred)")))
  eval(parse(text = paste0("mape <- mape(lagged$y[", nrow(lagged) - h + 1, ":", nrow(lagged), "], pred)")))
  
  return(list("mae" = mae, "mape" = mape))
}


### SANDBOX -------------------------------------------------------------------------------------------------------------------------------
ts.train <- ts(ts.apsp.monthly.log.returns[1:(length(ts.apsp.monthly.log.returns) - 12)],
               start = conv_to_ym(time(ts.apsp.monthly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.apsp.monthly.log.returns)[(length(ts.apsp.monthly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.apsp.monthly.log.returns[(length(ts.apsp.monthly.log.returns) - 11):length(ts.apsp.monthly.log.returns)],
              start = conv_to_ym(time(ts.apsp.monthly.log.returns)[(length(ts.apsp.monthly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.apsp.monthly.log.returns)[length(ts.apsp.monthly.log.returns)] - 0.083),
              frequency = 12)

apsp.log.returns.lagged <- data.frame("y" = ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)],
                                      "l1" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 1),
                                      "l2" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 2),
                                      "l3" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 3),
                                      "l4" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 4),
                                      "l5" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 5),
                                      "l6" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 6),
                                      "l7" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 7),
                                      "l8" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 8),
                                      "l9" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 9),
                                      "l10" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 10),
                                      "l11" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 11),
                                      "l12" = lag(ts.apsp.monthly.log.returns[1:length(ts.apsp.monthly.log.returns)], 12))


## Recursive multi-step forecast
set.seed(42)
fit <- randomForest(x = apsp.log.returns.lagged[c(-1:-12, -363:-374), 2:13], y = apsp.log.returns.lagged[c(-1:-12, -363:-374), 1])

pred <- c()
for (i in 1:12){
  pred[i] <- predict(fit, apsp.log.returns.lagged[(362 + i), 2:13])
}

mape(ts.apsp.monthly.log.returns[363:374], pred)

## Direct multi-step forecast
set.seed(42)

pred <- c()
for(i in 1:12){
  fit <- randomForest(x = apsp.log.returns.lagged[(25 - i):(363 - i), 2:13], y = apsp.log.returns.lagged[24:362, 1])
  pred[i] <- predict(fit, apsp.log.returns.lagged[362, 2:13])
}

mape(ts.apsp.monthly.log.returns[363:374], pred)



### Tune RF with forking
### APSP
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.apsp.monthly.log.returns, ntree = ntree, mtry = mtry, h = 12, n_lags = l)
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
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.brent.monthly.log.returns, ntree = ntree, mtry = mtry, h = 12, n_lags = l)
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
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.dubai.monthly.log.returns, ntree = ntree, mtry = mtry, h = 12, n_lags = l)
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

### NatGas
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.natgas.us.monthly.log.returns, ntree = ntree, mtry = mtry, h = 12, n_lags = l)
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
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
tuning_forked <- function(l, ntree, mtry){
  res <- rf_tune(data_ts = ts.wti.monthly.log.returns, ntree = ntree, mtry = mtry, h = 12, n_lags = l)
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





### Convert log returns back to prices before evaluating ---------------------------------------------------------------
ts.apsp <- data.frame("price" = ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)], "return" = lag(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)], 1))
ts.apsp$return <- ts.apsp$price / ts.apsp$return
ts.apsp$log.return <- log(ts.apsp$return)

apsp.log.returns.lagged <- data.frame("y" = ts.apsp$log.return[1:nrow(ts.apsp)],
                                      "l1" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 1),
                                      "l2" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 2),
                                      "l3" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 3),
                                      "l4" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 4),
                                      "l5" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 5),
                                      "l6" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 6),
                                      "l7" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 7),
                                      "l8" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 8),
                                      "l9" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 9),
                                      "l10" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 10),
                                      "l11" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 11),
                                      "l12" = lag(ts.apsp$log.return[1:nrow(ts.apsp)], 12))

## Recursive multi-step forecast
set.seed(42)
fit <- randomForest(x = apsp.log.returns.lagged[c(-1:-13, -363:-374), 2:13], y = apsp.log.returns.lagged[c(-1:-13, -363:-374), 1])

pred <- rf_recursive_pred(fit, 12, apsp.log.returns.lagged[363, -1])

mape(ts.apsp$log.return[363:374], pred)
mape(ts.apsp.monthly.absolute[363:374], ts.apsp.monthly.absolute[362] * cumprod(exp(pred)))

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



### Tune RF with forking -----------------------------------------------------------------------------------------------
### APSP
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
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
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
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
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
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

### NatGas
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
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
fork.params <- expand.grid("l" = 5:20, "ntree" = seq(1, 250, 1), "mtry" = 1:5)
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


### Save predictions of best configurations
n_lags <- tuning.results.apsp.log.returns.forked$n_lags[which.min(tuning.results.apsp.log.returns.forked$mape)]
ntree <- tuning.results.apsp.log.returns.forked$ntree[which.min(tuning.results.apsp.log.returns.forked$mape)]
mtry <- tuning.results.apsp.log.returns.forked$mtry[which.min(tuning.results.apsp.log.returns.forked$mape)]
lagged <- data.frame("y" = log(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)] / lag(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)]))[-1])
for (i in 1:n_lags){
  eval(parse(text = paste0("lagged$l", i, " = lag(lagged$y[1:nrow(lagged)], ", i, ")")))
}

set.seed(42)
apsp.pred.log.return <- ts.apsp.monthly.absolute[(nrow(lagged) - 12 + 1)] * cumprod(exp(rf_recursive_pred(model = randomForest(x = lagged[(n_lags + 1):(nrow(lagged) - n_lags),-1], y = lagged$y[(n_lags + 1):(nrow(lagged) - n_lags)], ntree = ntree, mtry = mtry), h = 12, lagged.data = lagged[(nrow(lagged) - 12 + 1), -1])))


n_lags <- tuning.results.brent.log.returns.forked$n_lags[which.min(tuning.results.brent.log.returns.forked$mape)]
ntree <- tuning.results.brent.log.returns.forked$ntree[which.min(tuning.results.brent.log.returns.forked$mape)]
mtry <- tuning.results.brent.log.returns.forked$mtry[which.min(tuning.results.brent.log.returns.forked$mape)]
lagged <- data.frame("y" = log(ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)] / lag(ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)]))[-1])
for (i in 1:n_lags){
  eval(parse(text = paste0("lagged$l", i, " = lag(lagged$y[1:nrow(lagged)], ", i, ")")))
}

set.seed(42)
brent.pred.log.return <- ts.brent.monthly.absolute[(nrow(lagged) - 12 + 1)] * cumprod(exp(rf_recursive_pred(model = randomForest(x = lagged[(n_lags + 1):(nrow(lagged) - n_lags),-1], y = lagged$y[(n_lags + 1):(nrow(lagged) - n_lags)], ntree = ntree, mtry = mtry), h = 12, lagged.data = lagged[(nrow(lagged) - 12 + 1), -1])))


n_lags <- tuning.results.dubai.log.returns.forked$n_lags[which.min(tuning.results.dubai.log.returns.forked$mape)]
ntree <- tuning.results.dubai.log.returns.forked$ntree[which.min(tuning.results.dubai.log.returns.forked$mape)]
mtry <- tuning.results.dubai.log.returns.forked$mtry[which.min(tuning.results.dubai.log.returns.forked$mape)]
lagged <- data.frame("y" = log(ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)] / lag(ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)]))[-1])
for (i in 1:n_lags){
  eval(parse(text = paste0("lagged$l", i, " = lag(lagged$y[1:nrow(lagged)], ", i, ")")))
}

set.seed(42)
dubai.pred.log.return <- ts.dubai.monthly.absolute[(nrow(lagged) - 12 + 1)] * cumprod(exp(rf_recursive_pred(model = randomForest(x = lagged[(n_lags + 1):(nrow(lagged) - n_lags),-1], y = lagged$y[(n_lags + 1):(nrow(lagged) - n_lags)], ntree = ntree, mtry = mtry), h = 12, lagged.data = lagged[(nrow(lagged) - 12 + 1), -1])))


n_lags <- tuning.results.natgas.us.log.returns.forked$n_lags[which.min(tuning.results.natgas.us.log.returns.forked$mape)]
ntree <- tuning.results.natgas.us.log.returns.forked$ntree[which.min(tuning.results.natgas.us.log.returns.forked$mape)]
mtry <- tuning.results.natgas.us.log.returns.forked$mtry[which.min(tuning.results.natgas.us.log.returns.forked$mape)]
lagged <- data.frame("y" = log(ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)] / lag(ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)]))[-1])
for (i in 1:n_lags){
  eval(parse(text = paste0("lagged$l", i, " = lag(lagged$y[1:nrow(lagged)], ", i, ")")))
}

set.seed(42)
natgas.us.pred.log.return <- ts.natgas.us.monthly.absolute[(nrow(lagged) - 12 + 1)] * cumprod(exp(rf_recursive_pred(model = randomForest(x = lagged[(n_lags + 1):(nrow(lagged) - n_lags),-1], y = lagged$y[(n_lags + 1):(nrow(lagged) - n_lags)], ntree = ntree, mtry = mtry), h = 12, lagged.data = lagged[(nrow(lagged) - 12 + 1), -1])))


n_lags <- tuning.results.wti.log.returns.forked$n_lags[which.min(tuning.results.wti.log.returns.forked$mape)]
ntree <- tuning.results.wti.log.returns.forked$ntree[which.min(tuning.results.wti.log.returns.forked$mape)]
mtry <- tuning.results.wti.log.returns.forked$mtry[which.min(tuning.results.wti.log.returns.forked$mape)]
lagged <- data.frame("y" = log(ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)] / lag(ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)]))[-1])
for (i in 1:n_lags){
  eval(parse(text = paste0("lagged$l", i, " = lag(lagged$y[1:nrow(lagged)], ", i, ")")))
}

set.seed(42)
wti.pred.log.return <- ts.wti.monthly.absolute[(nrow(lagged) - 12 + 1)] * cumprod(exp(rf_recursive_pred(model = randomForest(x = lagged[(n_lags + 1):(nrow(lagged) - n_lags),-1], y = lagged$y[(n_lags + 1):(nrow(lagged) - n_lags)], ntree = ntree, mtry = mtry), h = 12, lagged.data = lagged[(nrow(lagged) - 12 + 1), -1])))

rf.pred <- data.frame("apsp.log.return" = apsp.pred.log.return,
                      "brent.log.return" = brent.pred.log.return,
                      "dubai.log.return" = dubai.pred.log.return,
                      "natgas.us.log.return" = natgas.us.pred.log.return,
                      "wti.log.return" = wti.pred.log.return)
save(rf.pred, file = "results/ml models/RF pred.RData")
