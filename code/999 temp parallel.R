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

ann_tune <- function(data_ts, hidden_layers, target_threshold, h, n_lags, series = "price"){
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
  
  
  # Train ANN
  set.seed(42)
  eval(parse(text = paste0("ann <- neuralnet(formula = y ~ ", paste(names(lagged)[-1], collapse = " + "), ", data = lagged[", n_lags + 1, ":(nrow(lagged) - ", n_lags, "),], hidden = ", paste0("c(", paste(hidden_layers, collapse = ", "), ")"), ", threshold = ", target_threshold, ")")))
  
  # Recursively predict using the ANN
  pred <- ann_recursive_pred(model = ann, h = h, lagged.data = lagged[(nrow(lagged) - h + 1), -1])
  
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


### LNG
fork.params <- expand.grid("l" = 2:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  out <- tryCatch(expr = {
    res <- ann_tune(data_ts = ts.lng.monthly.absolute, hidden_layers = c(h1, h2, h3), 0.01, 12, l, series = "log.return")
    print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", ")))
    return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
  }, error = function(cond){
    res <- ann_tune(data_ts = ts.lng.monthly.absolute, hidden_layers = c(h1, h2, h3), 0.015, 12, l, series = "log.return")
    print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", "), " (with Error)"))
    return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
  })
  return(out)
}

tuning.results.lng.log.returns.forked <- mcmapply(FUN = function(l, h1, h2, h3){return(tuning_forked(l, h1, h2, h3))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2, h3 = fork.params$h3)

tuning.results.lng.log.returns.forked <- as.data.frame(t(tuning.results.lng.log.returns.forked))
tuning.results.lng.log.returns.forked$n_lags <- unlist(tuning.results.lng.log.returns.forked$n_lags)
tuning.results.lng.log.returns.forked$hidden_config <- unlist(tuning.results.lng.log.returns.forked$hidden_config)
tuning.results.lng.log.returns.forked$mae <- unlist(tuning.results.lng.log.returns.forked$mae)
tuning.results.lng.log.returns.forked$mape <- unlist(tuning.results.lng.log.returns.forked$mape)

fork.params <- expand.grid("l" = 2:20, "h1" = 1:20, "h2" = 1:20)
tuning_forked <- function(l, h1, h2){
  out <- tryCatch(expr = {
    res <- ann_tune(data_ts = ts.lng.monthly.absolute, hidden_layers = c(h1, h2), 0.01, 12, l, series = "log.return")
    print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, collapse = ", ")))
    return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
  }, error = function(cond){
    res <- ann_tune(data_ts = ts.lng.monthly.absolute, hidden_layers = c(h1, h2), 0.015, 12, l, series = "log.return")
    print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, collapse = ", "), " (with Error)"))
    return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
  })
  return(out)
}

temp <- mcmapply(FUN = function(l, h1, h2){return(tuning_forked(l, h1, h2))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.lng.log.returns.forked <- rbind(tuning.results.lng.log.returns.forked, temp)

fork.params <- expand.grid("l" = 2:20, "h1" = 1:20)
tuning_forked <- function(l, h1){
  out <- tryCatch(expr = {
    res <- ann_tune(data_ts = ts.lng.monthly.absolute, hidden_layers = c(h1), 0.01, 12, l, series = "log.return")
    print(paste0("Finished l: ", l, ", hl: ", paste(h1, collapse = ", ")))
    return(data.frame("n_lags" = l, "hidden_config" = paste(h1, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
  }, error = function(cond){
    res <- ann_tune(data_ts = ts.lng.monthly.absolute, hidden_layers = c(h1), 0.015, 12, l, series = "log.return")
    print(paste0("Finished l: ", l, ", hl: ", paste(h1, collapse = ", "), " (with Error)"))
    return(data.frame("n_lags" = l, "hidden_config" = paste(h1, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
  })
  return(out)
}

temp <- mcmapply(FUN = function(l, h1){return(tuning_forked(l, h1))}, l = fork.params$l, h1 = fork.params$h1)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.lng.log.returns.forked <- rbind(tuning.results.lng.log.returns.forked, temp); rm(temp)

save(tuning.results.lng.log.returns.forked, file = "results/ml models/LNG_log_returns_forked_2.RData")
