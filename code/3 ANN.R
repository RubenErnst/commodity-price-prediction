rm(list = ls())

library(tidyverse)
library(forecast)
library(tsfgrnn)
library(neuralnet)

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
    newdata[i,] <- c(rep(NA, i - 1), as.numeric(lagged.data[1:(h - i + 1)]))
  }
  
  pred <- c()
  for (i in 1:nrow(newdata)){
    pred <- c(predict(object = model, newdata = newdata[i,])[[1]], pred)
    newdata[(i + 1), 1:i] <- pred
  }
  return(pred)
}

ann_tune <- function(data_ts, hidden_layers, target_threshold, h, n_lags){
  # Build lagged dataset
  lagged <- data.frame("y" = data_ts[1:length(data_ts)])
  for (i in 1:n_lags){
    eval(parse(text = paste0("lagged$l", i, " = lag(data_ts[1:length(data_ts)], ", i, ")")))
  }
  
  # Train ANN
  eval(parse(text = paste0("ann <- neuralnet(formula = y ~ ", paste(names(lagged)[-1], collapse = " + "), ", data = lagged[", n_lags + 1, ":(nrow(lagged) - ", n_lags, "),], hidden = ", paste0("c(", paste(hidden_layers, collapse = ", "), ")"), ", threshold = ", target_threshold, ", lifesign = 'full')")))
  
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
