rm(list = ls())

library(tidyverse)
library(moments)


load("clean data/timeseries.RData")

### APSP
# Assemble table of descriptive stats
apsp.desc <- data.frame("price" = ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)],
                        "year" = lubridate::year(zoo::yearmon(time(ts.apsp.monthly.absolute))),
                        "month" = lubridate::month(zoo::yearmon(time(ts.apsp.monthly.absolute))))

apsp.desc <- merge(select(aggregate(price ~ year, apsp.desc, quantile, 0), "quantile.0" = price, year),
                   select(aggregate(price ~ year, apsp.desc, quantile, 0.25), "quantile.25" = price, year),
                   by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, quantile, 0.5),
                  "quantile.50" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, quantile, 0.75),
                  "quantile.75" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, quantile, 1),
                  "quantile.100" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, mean),
                  "mean" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, median),
                  "median" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, function(x){unique(x)[which.max(tabulate(match(x, unique(x))))]}),
                  "mode" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, var),
                  "variance" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, sd),
                  "sd" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, function(x){sd(x) / mean(x)}),
                  "sd.perc" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, skewness),
                  "skewness" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, apsp.desc, kurtosis),
                  "kurtosis" = price, year),
        by = "year", all.x = TRUE)

apsp.desc$commodity <- "APSP"


### Brent
# Assemble table of descriptive stats
brent.desc <- data.frame("price" = ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)],
                         "year" = lubridate::year(zoo::yearmon(time(ts.brent.monthly.absolute))),
                         "month" = lubridate::month(zoo::yearmon(time(ts.brent.monthly.absolute))))

brent.desc <- merge(select(aggregate(price ~ year, brent.desc, quantile, 0), "quantile.0" = price, year),
                    select(aggregate(price ~ year, brent.desc, quantile, 0.25), "quantile.25" = price, year),
                    by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, quantile, 0.5),
                  "quantile.50" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, quantile, 0.75),
                  "quantile.75" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, quantile, 1),
                  "quantile.100" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, mean),
                  "mean" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, median),
                  "median" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, function(x){unique(x)[which.max(tabulate(match(x, unique(x))))]}),
                  "mode" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, var),
                  "variance" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, sd),
                  "sd" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, function(x){sd(x) / mean(x)}),
                  "sd.perc" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, skewness),
                  "skewness" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, brent.desc, kurtosis),
                  "kurtosis" = price, year),
        by = "year", all.x = TRUE)

brent.desc$commodity <- "Brent"


### Dubai
# Assemble table of descriptive stats
dubai.desc <- data.frame("price" = ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)],
                         "year" = lubridate::year(zoo::yearmon(time(ts.dubai.monthly.absolute))),
                         "month" = lubridate::month(zoo::yearmon(time(ts.dubai.monthly.absolute))))

dubai.desc <- merge(select(aggregate(price ~ year, dubai.desc, quantile, 0), "quantile.0" = price, year),
                    select(aggregate(price ~ year, dubai.desc, quantile, 0.25), "quantile.25" = price, year),
                    by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, quantile, 0.5),
                  "quantile.50" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, quantile, 0.75),
                  "quantile.75" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, quantile, 1),
                  "quantile.100" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, mean),
                  "mean" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, median),
                  "median" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, function(x){unique(x)[which.max(tabulate(match(x, unique(x))))]}),
                  "mode" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, var),
                  "variance" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, sd),
                  "sd" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, function(x){sd(x) / mean(x)}),
                  "sd.perc" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, skewness),
                  "skewness" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, dubai.desc, kurtosis),
                  "kurtosis" = price, year),
        by = "year", all.x = TRUE)

dubai.desc$commodity <- "Dubai"


### LNG
# Assemble table of descriptive stats
lng.desc <- data.frame("price" = ts.lng.monthly.absolute[1:length(ts.lng.monthly.absolute)],
                         "year" = lubridate::year(zoo::yearmon(time(ts.lng.monthly.absolute))),
                         "month" = lubridate::month(zoo::yearmon(time(ts.lng.monthly.absolute))))

lng.desc <- merge(select(aggregate(price ~ year, lng.desc, quantile, 0), "quantile.0" = price, year),
                    select(aggregate(price ~ year, lng.desc, quantile, 0.25), "quantile.25" = price, year),
                    by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, quantile, 0.5),
                  "quantile.50" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, quantile, 0.75),
                  "quantile.75" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, quantile, 1),
                  "quantile.100" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, mean),
                  "mean" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, median),
                  "median" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, function(x){unique(x)[which.max(tabulate(match(x, unique(x))))]}),
                  "mode" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, var),
                  "variance" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, sd),
                  "sd" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, function(x){sd(x) / mean(x)}),
                  "sd.perc" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, skewness),
                  "skewness" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, lng.desc, kurtosis),
                  "kurtosis" = price, year),
        by = "year", all.x = TRUE)

lng.desc$commodity <- "LNG Asia"


### NatGas
# Assemble table of descriptive stats
natgas.us.desc <- data.frame("price" = ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)],
                             "year" = lubridate::year(zoo::yearmon(time(ts.natgas.us.monthly.absolute))),
                             "month" = lubridate::month(zoo::yearmon(time(ts.natgas.us.monthly.absolute))))

natgas.us.desc <- merge(select(aggregate(price ~ year, natgas.us.desc, quantile, 0), "quantile.0" = price, year),
                        select(aggregate(price ~ year, natgas.us.desc, quantile, 0.25), "quantile.25" = price, year),
                        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, quantile, 0.5),
                  "quantile.50" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, quantile, 0.75),
                  "quantile.75" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, quantile, 1),
                  "quantile.100" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, mean),
                  "mean" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, median),
                  "median" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, function(x){unique(x)[which.max(tabulate(match(x, unique(x))))]}),
                  "mode" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, var),
                  "variance" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, sd),
                  "sd" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, function(x){sd(x) / mean(x)}),
                  "sd.perc" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, skewness),
                  "skewness" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, natgas.us.desc, kurtosis),
                  "kurtosis" = price, year),
        by = "year", all.x = TRUE)

natgas.us.desc$commodity <- "NatGas"


### WTI
# Assemble table of descriptive stats
wti.desc <- data.frame("price" = ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)],
                       "year" = lubridate::year(zoo::yearmon(time(ts.wti.monthly.absolute))),
                       "month" = lubridate::month(zoo::yearmon(time(ts.wti.monthly.absolute))))

wti.desc <- merge(select(aggregate(price ~ year, wti.desc, quantile, 0), "quantile.0" = price, year),
                  select(aggregate(price ~ year, wti.desc, quantile, 0.25), "quantile.25" = price, year),
                  by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, quantile, 0.5),
                  "quantile.50" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, quantile, 0.75),
                  "quantile.75" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, quantile, 1),
                  "quantile.100" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, mean),
                  "mean" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, median),
                  "median" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, function(x){unique(x)[which.max(tabulate(match(x, unique(x))))]}),
                  "mode" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, var),
                  "variance" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, sd),
                  "sd" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, function(x){sd(x) / mean(x)}),
                  "sd.perc" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, skewness),
                  "skewness" = price, year),
        by = "year", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ year, wti.desc, kurtosis),
                  "kurtosis" = price, year),
        by = "year", all.x = TRUE)

wti.desc$commodity <- "WTI"


# Save results
descriptives <- rbind(apsp.desc, brent.desc, dubai.desc, lng.desc, natgas.us.desc, wti.desc)
save(descriptives, file = "results/descriptive stats/Descriptives.RData")
openxlsx::write.xlsx(descriptives, file = "results/descriptive stats/Descriptives.xlsx")



### Short version (without yearly resolution) ----
# Assemble table of descriptive stats
descriptives.short <- rbind(data.frame("price" = ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)],
                                       "year" = lubridate::year(zoo::yearmon(time(ts.apsp.monthly.absolute))),
                                       "month" = lubridate::month(zoo::yearmon(time(ts.apsp.monthly.absolute))),
                                       "commodity" = "APSP"),
                            data.frame("price" = ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)],
                                       "year" = lubridate::year(zoo::yearmon(time(ts.brent.monthly.absolute))),
                                       "month" = lubridate::month(zoo::yearmon(time(ts.brent.monthly.absolute))),
                                       "commodity" = "Brent"),
                            data.frame("price" = ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)],
                                       "year" = lubridate::year(zoo::yearmon(time(ts.dubai.monthly.absolute))),
                                       "month" = lubridate::month(zoo::yearmon(time(ts.dubai.monthly.absolute))),
                                       "commodity" = "Dubai"),
                            data.frame("price" = ts.lng.monthly.absolute[1:length(ts.lng.monthly.absolute)],
                                       "year" = lubridate::year(zoo::yearmon(time(ts.lng.monthly.absolute))),
                                       "month" = lubridate::month(zoo::yearmon(time(ts.lng.monthly.absolute))),
                                       "commodity" = "LNG"),
                            data.frame("price" = ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)],
                                       "year" = lubridate::year(zoo::yearmon(time(ts.natgas.us.monthly.absolute))),
                                       "month" = lubridate::month(zoo::yearmon(time(ts.natgas.us.monthly.absolute))),
                                       "commodity" = "NatGas"),
                            data.frame("price" = ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)],
                                       "year" = lubridate::year(zoo::yearmon(time(ts.wti.monthly.absolute))),
                                       "month" = lubridate::month(zoo::yearmon(time(ts.wti.monthly.absolute))),
                                       "commodity" = "WTI"))

descriptives.short <- merge(select(aggregate(price ~ commodity, descriptives.short, quantile, 0), "quantile.0" = price, commodity),
                   select(aggregate(price ~ commodity, descriptives.short, quantile, 0.25), "quantile.25" = price, commodity),
                   by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, quantile, 0.5),
                  "quantile.50" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, quantile, 0.75),
                  "quantile.75" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, quantile, 1),
                  "quantile.100" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, mean),
                  "mean" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, median),
                  "median" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, function(x){unique(x)[which.max(tabulate(match(x, unique(x))))]}),
                  "mode" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, var),
                  "variance" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, sd),
                  "sd" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, function(x){sd(x) / mean(x)}),
                  "sd.perc" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, skewness),
                  "skewness" = price, commodity),
        by = "commodity", all.x = TRUE) %>%
  merge(., select(aggregate(price ~ commodity, descriptives.short, kurtosis),
                  "kurtosis" = price, commodity),
        by = "commodity", all.x = TRUE)



# Save results
save(descriptives.short, file = "results/descriptive stats/Descriptives_short.RData")
openxlsx::write.xlsx(descriptives.short, file = "results/descriptive stats/Descriptives_short.xlsx")




##### Graphs ----
### Boxplots
boxplot.data <- data.frame("price" = c(ts.apsp.monthly.absolute[1:length(ts.apsp.monthly.absolute)],
                                       ts.brent.monthly.absolute[1:length(ts.brent.monthly.absolute)],
                                       ts.dubai.monthly.absolute[1:length(ts.dubai.monthly.absolute)],
                                       ts.lng.monthly.absolute[1:length(ts.lng.monthly.absolute)],
                                       ts.natgas.us.monthly.absolute[1:length(ts.natgas.us.monthly.absolute)],
                                       ts.wti.monthly.absolute[1:length(ts.wti.monthly.absolute)]),
                           "year" = c(lubridate::year(zoo::yearmon(time(ts.apsp.monthly.absolute))),
                                      lubridate::year(zoo::yearmon(time(ts.brent.monthly.absolute))),
                                      lubridate::year(zoo::yearmon(time(ts.dubai.monthly.absolute))),
                                      lubridate::year(zoo::yearmon(time(ts.lng.monthly.absolute))),
                                      lubridate::year(zoo::yearmon(time(ts.natgas.us.monthly.absolute))),
                                      lubridate::year(zoo::yearmon(time(ts.wti.monthly.absolute)))),
                           "commodity" = c(rep("APSP", length(ts.apsp.monthly.absolute)),
                                           rep("Brent", length(ts.brent.monthly.absolute)),
                                           rep("Dubai", length(ts.dubai.monthly.absolute)),
                                           rep("LNG", length(ts.lng.monthly.absolute)),
                                           rep("NatGas", length(ts.natgas.us.monthly.absolute)),
                                           rep("WTI", length(ts.wti.monthly.absolute))))

boxplot.data$year <- as.factor(boxplot.data$year)

dispersion.plot <- ggplot(data = boxplot.data, aes(x = year, y = price)) +
  geom_boxplot() +
  labs(x = "Year", y = "Price (USD)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap( ~ commodity, scales = "free",
              labeller = as_labeller(c("APSP" = "APSP", "Brent" = "Brent", "Dubai" = "Dubai", "NatGas" = "Natural Gas Henry Hub", "WTI" = "WTI", "LNG" = "LNG Asia")))

dispersion.plot

ggsave("plots/1_Price dispersion plot.pdf", dispersion.plot, device = cairo_pdf, width = 12, height = 9)
