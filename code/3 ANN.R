rm(list = ls())

library(tidyverse)
library(forecast)
library(tsfgrnn)
library(neuralnet)
library(parallel)
library(doParallel)
library(doSNOW)

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



# Resulting data frame
ann.results <- data.frame("commodity" = rep(c("APSP", "Brent", "Dubai", "US NatGas", "WTI"), 2),
                          "series" = c(rep("difference", 5), rep("log returns", 5)),
                          "model.dim" = rep(NA, 10),
                          "mae" = rep(NA, 10),
                          "mape" = rep(NA, 10))


### APSP first difference
# Split data
ts.train <- ts(ts.apsp.monthly.difference[1:(length(ts.apsp.monthly.difference) - 12)],
               start = conv_to_ym(time(ts.apsp.monthly.difference)[1] - 0.083),
               end = conv_to_ym(time(ts.apsp.monthly.difference)[(length(ts.apsp.monthly.difference) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.apsp.monthly.difference[(length(ts.apsp.monthly.difference) - 11):length(ts.apsp.monthly.difference)],
              start = conv_to_ym(time(ts.apsp.monthly.difference)[(length(ts.apsp.monthly.difference) - 11)] - 0.083),
              end = conv_to_ym(time(ts.apsp.monthly.difference)[length(ts.apsp.monthly.difference)] - 0.083),
              frequency = 12)

# Train model
pred <- grnn_forecasting(ts.train, h = 12)

# Evaluate
ann.results$mae[1] <- mae(ts.test, pred$prediction)
ann.results$mape[1] <- mape(ts.test, pred$prediction)


### APSP log returns
# Split data
ts.train <- ts(ts.apsp.monthly.log.returns[1:(length(ts.apsp.monthly.log.returns) - 12)],
               start = conv_to_ym(time(ts.apsp.monthly.log.returns)[1] - 0.083),
               end = conv_to_ym(time(ts.apsp.monthly.log.returns)[(length(ts.apsp.monthly.log.returns) - 12)] - 0.083),
               frequency = 12)
ts.test <- ts(ts.apsp.monthly.log.returns[(length(ts.apsp.monthly.log.returns) - 11):length(ts.apsp.monthly.log.returns)],
              start = conv_to_ym(time(ts.apsp.monthly.log.returns)[(length(ts.apsp.monthly.log.returns) - 11)] - 0.083),
              end = conv_to_ym(time(ts.apsp.monthly.log.returns)[length(ts.apsp.monthly.log.returns)] - 0.083),
              frequency = 12)

# Train model
pred <- grnn_forecasting(ts.train, h = 12)

# Evaluate
ann.results$mae[6] <- mae(ts.test, pred$prediction)
ann.results$mape[6] <- mape(ts.test, pred$prediction)



### SANDBOX ###########################################################
ts.train <- ts(ts.brent.monthly.log.returns[1:(length(ts.brent.monthly.log.returns) - 24)],
               start = conv_to_ym(time(ts.brent.monthly.log.returns)[1] - 0.166),
               end = conv_to_ym(time(ts.brent.monthly.log.returns)[(length(ts.brent.monthly.log.returns) - 24)] - 0.166),
               frequency = 12)
ts.test <- ts(ts.brent.monthly.log.returns[(length(ts.brent.monthly.log.returns) - 23):length(ts.brent.monthly.log.returns)],
              start = conv_to_ym(time(ts.brent.monthly.log.returns)[(length(ts.brent.monthly.log.returns) - 23)] - 0.166),
              end = conv_to_ym(time(ts.brent.monthly.log.returns)[length(ts.brent.monthly.log.returns)] - 0.166),
              frequency = 12)

pred <- grnn_forecasting(timeS = ts.train, h = 24, lags = c(1:50), msas = "recursive")
mae(ts.test, pred$prediction)
mape(ts.test, pred$prediction)

plot(ts.brent.monthly.log.returns)
lines(pred$prediction, col = "red")

rolling <- rolling_origin(pred, h = 12)
mae(ts.test, pred$prediction)
mape(ts.test, pred$prediction)



# Make dataframe with lags
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

ann <- neuralnet(formula = y ~ l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 + l10 + l11 + l12, data = apsp.log.returns.lagged[13:(nrow(apsp.log.returns.lagged) - 12),], hidden = c(15, 10, 5), threshold = 0.01, lifesign = "full")
result <- cbind(apsp.log.returns.lagged[13:362,], ann$net.result[[1]])

pred <- predict(object = ann, newdata = apsp.log.returns.lagged[363:374, ])
mae(apsp.log.returns.lagged$y[363:374], pred)
mape(apsp.log.returns.lagged$y[363:374], pred)


########################################################################################################################################################################


##### ANN Tuning -----

### Tune APSP log returns
tuning.results.apsp.log.returns <- data.frame("n_lags" = NULL, "hidden_config" = NULL, "mae" = NULL, "mape" = NULL, stringsAsFactors = FALSE)
# Generate 3 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.apsp.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.apsp.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 2 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.apsp.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.apsp.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 1 hidden layer
hl_combinations <- data.frame("h1" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.apsp.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.apsp.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

save(tuning.results.apsp.log.returns, file = "results/ml models/APSP_log_returns.RData")


### Tune Brent log returns
tuning.results.brent.log.returns <- data.frame("n_lags" = NULL, "hidden_config" = NULL, "mae" = NULL, "mape" = NULL, stringsAsFactors = FALSE)
# Generate 3 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.brent.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.brent.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 2 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.brent.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.brent.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 1 hidden layer
hl_combinations <- data.frame("h1" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.brent.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.brent.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

save(tuning.results.brent.log.returns, file = "results/ml models/Brent_log_returns.RData")


### Tune Dubai log returns
tuning.results.dubai.log.returns <- data.frame("n_lags" = NULL, "hidden_config" = NULL, "mae" = NULL, "mape" = NULL, stringsAsFactors = FALSE)
# Generate 3 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.dubai.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.dubai.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 2 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.dubai.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.dubai.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 1 hidden layer
hl_combinations <- data.frame("h1" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.dubai.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.dubai.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

save(tuning.results.dubai.log.returns, file = "results/ml models/Dubai_log_returns.RData")


### Tune US NatGas log returns
tuning.results.natgas.us.log.returns <- data.frame("n_lags" = NULL, "hidden_config" = NULL, "mae" = NULL, "mape" = NULL, stringsAsFactors = FALSE)
# Generate 3 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.us.natgas.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.us.natgas.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 2 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.us.natgas.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.us.natgas.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 1 hidden layer
hl_combinations <- data.frame("h1" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.us.natgas.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.us.natgas.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

save(tuning.results.us.natgas.log.returns, file = "results/ml models/NatGas_log_returns.RData")


### Tune WTI log returns
tuning.results.wti.log.returns <- data.frame("n_lags" = NULL, "hidden_config" = NULL, "mae" = NULL, "mape" = NULL, stringsAsFactors = FALSE)
# Generate 3 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.wti.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.wti.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 2 hidden layer combinations
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.wti.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.wti.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

# 1 hidden layer
hl_combinations <- data.frame("h1" = 1:20)
for (l in 5:20){
  for (hl in 1:nrow(hl_combinations)){
    res <- ann_tune(data_ts = ts.wti.monthly.log.returns, hidden_layers = hl_combinations[hl,], 0.01, 12, l)
    tuning.results.wti.log.returns <- rbind(tuning.results.apsp.log.returns, data.frame("n_lags" = l, "hidden_config" = paste0(hl_combinations[hl,], collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
    print(paste0("Finished l: ", l, ", hl: ", paste(hl_combinations[hl,], collapse = ", "))) 
  }
}

save(tuning.results.wti.log.returns, file = "results/ml models/WTI_log_returns.RData")




##### Parallel processing
cl <- makeSOCKcluster(4)
registerDoSNOW(cl)

my_fun <- function(data_ts, l, hl){
  res <- ann_tune(data_ts = data_ts, hidden_layers = hl, 0.01, 12, l)
  return(data.frame("n_lags" = l, "hidden_config" = paste0(hl, collapse = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}


### APSP
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/128000"))})

tuning.results.apsp.log.returns <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.apsp.monthly.log.returns, l, hl_combinations[hl,])
  }

hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/6400"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.apsp.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.apsp.log.returns <- rbind(tuning.results.apsp.log.returns, temp); rm(temp)

hl_combinations <- data.frame("h1" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/320"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.apsp.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.apsp.log.returns <- rbind(tuning.results.apsp.log.returns, temp); rm(temp)

save(tuning.results.apsp.log.returns, file = "results/ml models/APSP_log_returns.RData")


### Brent
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/128000"))})

tuning.results.brent.log.returns <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.brent.monthly.log.returns, l, hl_combinations[hl,])
  }

hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/6400"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.brent.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.brent.log.returns <- rbind(tuning.results.brent.log.returns, temp); rm(temp)

hl_combinations <- data.frame("h1" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/320"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.brent.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.brent.log.returns <- rbind(tuning.results.brent.log.returns, temp); rm(temp)

save(tuning.results.brent.log.returns, file = "results/ml models/Brent_log_returns.RData")

### Dubai
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/128000"))})

tuning.results.dubai.log.returns <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.dubai.monthly.log.returns, l, hl_combinations[hl,])
  }

hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/6400"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.dubai.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.dubai.log.returns <- rbind(tuning.results.dubai.log.returns, temp); rm(temp)

hl_combinations <- data.frame("h1" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/320"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.dubai.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.dubai.log.returns <- rbind(tuning.results.dubai.log.returns, temp); rm(temp)

save(tuning.results.dubai.log.returns, file = "results/ml models/Dubai_log_returns.RData")

### US NatGas
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/128000"))})

tuning.results.natgas.us.log.returns <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.natgas.us.monthly.log.returns, l, hl_combinations[hl,])
  }

hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/6400"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.natgas.us.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.natgas.us.log.returns <- rbind(tuning.results.natgas.us.log.returns, temp); rm(temp)

hl_combinations <- data.frame("h1" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/320"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.natgas.us.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.natgas.us.log.returns <- rbind(tuning.results.natgas.us.log.returns, temp); rm(temp)

save(tuning.results.natgas.us.log.returns, file = "results/ml models/NatGas_log_returns.RData")

### WTI
hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/128000"))})

tuning.results.wti.log.returns <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.wti.monthly.log.returns, l, hl_combinations[hl,])
  }

hl_combinations <- expand.grid("h1" = 1:20, "h2" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/6400"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.wti.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.wti.log.returns <- rbind(tuning.results.wti.log.returns, temp); rm(temp)

hl_combinations <- data.frame("h1" = 1:20)
opts <- list(progress=function(t){print(paste0("Finished task: ", t, "/320"))})

temp <- foreach (l = 5:20, .combine = rbind) %:%
  foreach(hl = 1:nrow(hl_combinations), .combine = rbind, .options.snow = opts, .packages = "neuralnet") %dopar% {
    my_fun(ts.wti.monthly.log.returns, l, hl_combinations[hl,])
  }

tuning.results.wti.log.returns <- rbind(tuning.results.wti.log.returns, temp); rm(temp)

save(tuning.results.wti.log.returns, file = "results/ml models/WTI_log_returns.RData")


snow::stopCluster(cl); rm(cl)



##### Forking -----
### APSP
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.apsp.monthly.log.returns, hidden_layers = c(h1, h2, h3), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.apsp.log.returns.forked <- mcmapply(FUN = function(l, h1, h2, h3){return(tuning_forked(l, h1, h2, h3))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2, h3 = fork.params$h3)

tuning.results.apsp.log.returns.forked <- as.data.frame(t(tuning.results.apsp.log.returns.forked))
tuning.results.apsp.log.returns.forked$n_lags <- unlist(tuning.results.apsp.log.returns.forked$n_lags)
tuning.results.apsp.log.returns.forked$hidden_config <- unlist(tuning.results.apsp.log.returns.forked$hidden_config)
tuning.results.apsp.log.returns.forked$mae <- unlist(tuning.results.apsp.log.returns.forked$mae)
tuning.results.apsp.log.returns.forked$mape <- unlist(tuning.results.apsp.log.returns.forked$mape)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20)
tuning_forked <- function(l, h1, h2){
  res <- ann_tune(data_ts = ts.apsp.monthly.log.returns, hidden_layers = c(h1, h2), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1, h2){return(tuning_forked(l, h1, h2))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.apsp.log.returns.forked <- rbind(tuning.results.apsp.log.returns.forked, temp)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20)
tuning_forked <- function(l, h1){
  res <- ann_tune(data_ts = ts.apsp.monthly.log.returns, hidden_layers = c(h1), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1){return(tuning_forked(l, h1))}, l = fork.params$l, h1 = fork.params$h1)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.apsp.log.returns.forked <- rbind(tuning.results.apsp.log.returns.forked, temp); rm(temp)

save(tuning.results.apsp.log.returns.forked, file = "results/ml models/APSP_log_returns_forked.RData")


### Brent
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.brent.monthly.log.returns, hidden_layers = c(h1, h2, h3), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.brent.log.returns.forked <- mcmapply(FUN = function(l, h1, h2, h3){return(tuning_forked(l, h1, h2, h3))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2, h3 = fork.params$h3)

tuning.results.brent.log.returns.forked <- as.data.frame(t(tuning.results.brent.log.returns.forked))
tuning.results.brent.log.returns.forked$n_lags <- unlist(tuning.results.brent.log.returns.forked$n_lags)
tuning.results.brent.log.returns.forked$hidden_config <- unlist(tuning.results.brent.log.returns.forked$hidden_config)
tuning.results.brent.log.returns.forked$mae <- unlist(tuning.results.brent.log.returns.forked$mae)
tuning.results.brent.log.returns.forked$mape <- unlist(tuning.results.brent.log.returns.forked$mape)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20)
tuning_forked <- function(l, h1, h2){
  res <- ann_tune(data_ts = ts.brent.monthly.log.returns, hidden_layers = c(h1, h2), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1, h2){return(tuning_forked(l, h1, h2))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.brent.log.returns.forked <- rbind(tuning.results.brent.log.returns.forked, temp)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20)
tuning_forked <- function(l, h1){
  res <- ann_tune(data_ts = ts.brent.monthly.log.returns, hidden_layers = c(h1), 0.01, 12, l)
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1){return(tuning_forked(l, h1))}, l = fork.params$l, h1 = fork.params$h1)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.brent.log.returns.forked <- rbind(tuning.results.brent.log.returns.forked, temp); rm(temp)

save(tuning.results.brent.log.returns.forked, file = "results/ml models/Brent_log_returns_forked.RData")


### Dubai
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


### Forking with retransformation of data -----
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


### APSP
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.apsp.monthly.absolute, hidden_layers = c(h1, h2, h3), 0.01, 12, l, series = "log.return")
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.apsp.log.returns.forked <- mcmapply(FUN = function(l, h1, h2, h3){return(tuning_forked(l, h1, h2, h3))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2, h3 = fork.params$h3)

tuning.results.apsp.log.returns.forked <- as.data.frame(t(tuning.results.apsp.log.returns.forked))
tuning.results.apsp.log.returns.forked$n_lags <- unlist(tuning.results.apsp.log.returns.forked$n_lags)
tuning.results.apsp.log.returns.forked$hidden_config <- unlist(tuning.results.apsp.log.returns.forked$hidden_config)
tuning.results.apsp.log.returns.forked$mae <- unlist(tuning.results.apsp.log.returns.forked$mae)
tuning.results.apsp.log.returns.forked$mape <- unlist(tuning.results.apsp.log.returns.forked$mape)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20)
tuning_forked <- function(l, h1, h2){
  res <- ann_tune(data_ts = ts.apsp.monthly.absolute, hidden_layers = c(h1, h2), 0.01, 12, l, series = "log.return")
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1, h2){return(tuning_forked(l, h1, h2))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.apsp.log.returns.forked <- rbind(tuning.results.apsp.log.returns.forked, temp)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20)
tuning_forked <- function(l, h1){
  res <- ann_tune(data_ts = ts.apsp.monthly.absolute, hidden_layers = c(h1), 0.01, 12, l, series = "log.return")
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1){return(tuning_forked(l, h1))}, l = fork.params$l, h1 = fork.params$h1)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.apsp.log.returns.forked <- rbind(tuning.results.apsp.log.returns.forked, temp); rm(temp)

save(tuning.results.apsp.log.returns.forked, file = "results/ml models/APSP_log_returns_forked_2.RData")


### Brent
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.brent.monthly.absolute, hidden_layers = c(h1, h2, h3), 0.01, 12, l, series = "log.return")
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, h3, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, h3, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

tuning.results.brent.log.returns.forked <- mcmapply(FUN = function(l, h1, h2, h3){return(tuning_forked(l, h1, h2, h3))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2, h3 = fork.params$h3)

tuning.results.brent.log.returns.forked <- as.data.frame(t(tuning.results.brent.log.returns.forked))
tuning.results.brent.log.returns.forked$n_lags <- unlist(tuning.results.brent.log.returns.forked$n_lags)
tuning.results.brent.log.returns.forked$hidden_config <- unlist(tuning.results.brent.log.returns.forked$hidden_config)
tuning.results.brent.log.returns.forked$mae <- unlist(tuning.results.brent.log.returns.forked$mae)
tuning.results.brent.log.returns.forked$mape <- unlist(tuning.results.brent.log.returns.forked$mape)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20)
tuning_forked <- function(l, h1, h2){
  res <- ann_tune(data_ts = ts.brent.monthly.absolute, hidden_layers = c(h1, h2), 0.01, 12, l, series = "log.return")
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, h2, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, h2, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1, h2){return(tuning_forked(l, h1, h2))}, l = fork.params$l, h1 = fork.params$h1, h2 = fork.params$h2)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.brent.log.returns.forked <- rbind(tuning.results.brent.log.returns.forked, temp)

fork.params <- expand.grid("l" = 5:20, "h1" = 1:20)
tuning_forked <- function(l, h1){
  res <- ann_tune(data_ts = ts.brent.monthly.absolute, hidden_layers = c(h1), 0.01, 12, l, series = "log.return")
  print(paste0("Finished l: ", l, ", hl: ", paste(h1, collapse = ", ")))
  return(data.frame("n_lags" = l, "hidden_config" = paste(h1, sep = ", "), "mae" = res[[1]], "mape" = res[[2]]))
}

temp <- mcmapply(FUN = function(l, h1){return(tuning_forked(l, h1))}, l = fork.params$l, h1 = fork.params$h1)

temp <- as.data.frame(t(temp))
temp$n_lags <- unlist(temp$n_lags)
temp$hidden_config <- unlist(temp$hidden_config)
temp$mae <- unlist(temp$mae)
temp$mape <- unlist(temp$mape)

tuning.results.brent.log.returns.forked <- rbind(tuning.results.brent.log.returns.forked, temp); rm(temp)

save(tuning.results.brent.log.returns.forked, file = "results/ml models/Brent_log_returns_forked_2.RData")


### Dubai
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.dubai.monthly.absolute, hidden_layers = c(h1, h2, h3), 0.01, 12, l, series = "log.return")
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
  res <- ann_tune(data_ts = ts.dubai.monthly.absolute, hidden_layers = c(h1, h2), 0.01, 12, l, series = "log.return")
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
  res <- ann_tune(data_ts = ts.dubai.monthly.absolute, hidden_layers = c(h1), 0.01, 12, l, series = "log.return")
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

save(tuning.results.dubai.log.returns.forked, file = "results/ml models/Dubai_log_returns_forked_2.RData")


### US NatGas
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.natgas.us.monthly.absolute, hidden_layers = c(h1, h2, h3), 0.01, 12, l, series = "log.return")
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
  res <- ann_tune(data_ts = ts.natgas.us.monthly.absolute, hidden_layers = c(h1, h2), 0.01, 12, l, series = "log.return")
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
  res <- ann_tune(data_ts = ts.natgas.us.monthly.absolute, hidden_layers = c(h1), 0.01, 12, l, series = "log.return")
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

save(tuning.results.natgas.us.log.returns.forked, file = "results/ml models/NatGas_log_returns_forked_2.RData")


### WTI
fork.params <- expand.grid("l" = 5:20, "h1" = 1:20, "h2" = 1:20, "h3" = 1:20)
tuning_forked <- function(l, h1, h2, h3){
  res <- ann_tune(data_ts = ts.wti.monthly.absolute, hidden_layers = c(h1, h2, h3), 0.01, 12, l, series = "log.return")
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
  res <- ann_tune(data_ts = ts.wti.monthly.absolute, hidden_layers = c(h1, h2), 0.01, 12, l, series = "log.return")
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
  res <- ann_tune(data_ts = ts.wti.monthly.absolute, hidden_layers = c(h1), 0.01, 12, l, series = "log.return")
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

save(tuning.results.wti.log.returns.forked, file = "results/ml models/WTI_log_returns_forked_2.RData")