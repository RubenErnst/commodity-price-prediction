rm(list = ls())

library(tidyverse)
library(forecast)
library(rugarch)


model <- ugarchspec(mean.model = list(armaOrder=c(0, 0), include.mean = FALSE), distribution = "std")
fit <- ugarchfit(spec = model, ts.apsp.monthly.log.returns)
coef(fit)
plot(sqrt(252) * fit@fit$sigma, type='l')

spec <- getspec(fit)
setfixed(spec) <- as.list(coef(fit))
forecast <- ugarchforecast(spec, n.ahead = 12, n.roll = 362, data = ts.apsp.monthly.log.returns[1:374,drop=FALSE], out.sample = 362)
