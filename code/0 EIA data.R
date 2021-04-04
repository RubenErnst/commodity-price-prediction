rm(list = ls())

### Load EIA data and clean it

library(tidyverse)
library(IMFData)
library(stringr)
library(zoo)


### EIA data -----
eia.summary <- read_delim("data/EIA/1._U.S._Energy_Markets_Summary.csv", delim = ",", skip = 4)
eia.energy.prices <- read_delim("data/EIA/2._Energy_Prices.csv", delim = ",", skip = 4)
eia.intl.petroleum <- read_delim("data/EIA/3a._International_Petroleum_and_Other_Liquids_Production_Consumption_and_Inventories.csv", delim = ",", skip = 4)
eia.non.opec <- read_delim("data/EIA/3b._Non-OPEC_Petroleum_and_Other_Liquids_Production.csv", delim = ",", skip = 4)
eia.opec.crude <- read_delim("data/EIA/3c._OPEC_Crude_Oil_(excluding_condensates)_Production.csv", delim = ",", skip = 4)
eia.world.petroleum <- read_delim("data/EIA/3d._World_Petroleum_and_Other_Liquids_Consumption.csv", delim = ",", skip = 4)
eia.us.petroleum <- read_delim("data/EIA/4a._U.S._Petroleum_and_Other_Liquids_Supply_Consumption_and_Inventories.csv", delim = ",", skip = 4)
eia.us.hgl <- read_delim("data/EIA/4b._U.S._Hydrocarbon_Gas_Liquids_(HGL)_and_Petroleum_Refinery_Balances.csv", delim = ",", skip = 4)
eia.us.gasoline <- read_delim("data/EIA/4c._U.S._Regional_Motor_Gasoline_Prices_and_Inventories.csv", delim = ",", skip = 4)
eia.us.natgas.supply <- read_delim("data/EIA/5a._U.S._Natural_Gas_Supply_Consumption_and_Inventories.csv", delim = ",", skip = 4)
eia.us.natgas.prices <- read_delim("data/EIA/5b._U.S._Regional_Natural_Gas_Prices.csv", delim = ",", skip = 4)
eia.us.coal <- read_delim("data/EIA/6._U.S._Coal_Supply_Consumption_and_Inventories.csv", delim = ",", skip = 4)



### Clean and transform to long format

## Summary
# Rename and drop unnecessary columns
eia.summary <- select(eia.summary, -map, -linechart)
names(eia.summary) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.summary)[5:ncol(eia.summary)])

# Reformat from nested chapters to subchapter structure
eia.summary[3:5,1] <- "Energy Supply"
eia.summary[7:12,1] <- "Energy Consumption"
eia.summary[14:16,1] <- "Energy Prices"
eia.summary[18:25,1] <- "Macroeconomic"
eia.summary[27:28,1] <- "Weather"

# Drop empty rows
eia.summary <- subset(eia.summary, !is.na(sourcekey))

# Replace "--" with NA
eia.summary[eia.summary == "--"] <- NA

# Transpose to long format
eia.summary <- pivot_longer(eia.summary, cols = names(eia.summary)[5:ncol(eia.summary)], names_to = "year", values_to = "value",
                            names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
# Sys.setlocale("LC_TIME", "English")
eia.summary$month <- sapply(eia.summary$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.summary$year <- sapply(eia.summary$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## Energy prices
# Rename and drop unnecessary columns
eia.energy.prices <- select(eia.energy.prices, -map, -linechart)
names(eia.energy.prices) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.energy.prices)[5:ncol(eia.energy.prices)])

# Reformat from nested chapters to subchapter structure
eia.energy.prices[3:6,1] <- "Crude Oil Prices"
eia.energy.prices[9:11,1] <- "U.S. Liquid Fuels - Refiner Prices for Resale"
eia.energy.prices[13:14,1] <- "U.S. Liquid Fuels - Refiner Prices to End Users"
eia.energy.prices[16:19,1] <- "U.S. Liquid Fuels - Retail Prices Including Taxes"
eia.energy.prices[21:22,1] <- "Natural Gas"
eia.energy.prices[24:26,1] <- "U.S. Retail Prices"
eia.energy.prices[29:32,1] <- "U.S. Electricity - Power Generation Fuel Costs"
eia.energy.prices[34:36,1] <- "U.S. Electricity - Retail Prices"

# Drop empty rows
eia.energy.prices <- subset(eia.energy.prices, !is.na(sourcekey))

# Replace "--" with NA
eia.energy.prices[eia.energy.prices == "--"] <- NA

# Transpose to long format
eia.energy.prices <- pivot_longer(eia.energy.prices, cols = names(eia.energy.prices)[5:ncol(eia.energy.prices)], names_to = "year", values_to = "value",
                                  names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.energy.prices$month <- sapply(eia.energy.prices$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.energy.prices$year <- sapply(eia.energy.prices$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## International Petroleum
# Rename and drop unnecessary columns
eia.intl.petroleum <- select(eia.intl.petroleum, -map, -linechart)
names(eia.intl.petroleum) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.intl.petroleum)[5:ncol(eia.intl.petroleum)])

# Reformat from nested chapters to subchapter structure
eia.intl.petroleum[3:17,1] <- "Supply"
eia.intl.petroleum[18:37,1] <- "Consumption"
eia.intl.petroleum[39:40,1] <- "End-of-period Commercial Crude Oil and Other Liquids Inventories"

# Drop empty rows
eia.intl.petroleum <- subset(eia.intl.petroleum, !is.na(sourcekey))

# Replace "--" with NA
eia.intl.petroleum[eia.intl.petroleum == "--"] <- NA

# Transpose to long format
eia.intl.petroleum <- pivot_longer(eia.intl.petroleum, cols = names(eia.intl.petroleum)[5:ncol(eia.intl.petroleum)], names_to = "year", values_to = "value",
                                   names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.intl.petroleum$month <- sapply(eia.intl.petroleum$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.intl.petroleum$year <- sapply(eia.intl.petroleum$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## Non-OPEC
# Rename and drop unnecessary columns
eia.non.opec <- select(eia.non.opec, -map, -linechart)
names(eia.non.opec) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.non.opec)[5:ncol(eia.non.opec)])

# Reformat from nested chapters to subchapter structure
eia.non.opec[2:39,1] <- "Non-OPEC Petroleum and Other Liquids Production"

# Drop empty rows
eia.non.opec <- subset(eia.non.opec, !is.na(sourcekey))

# Replace "--" with NA
eia.non.opec[eia.non.opec == "--"] <- NA

# Transpose to long format
eia.non.opec <- pivot_longer(eia.non.opec, cols = names(eia.non.opec)[5:ncol(eia.non.opec)], names_to = "year", values_to = "value",
                             names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.non.opec$month <- sapply(eia.non.opec$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.non.opec$year <- sapply(eia.non.opec$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## OPEC crude
# Rename and drop unnecessary columns
eia.opec.crude <- select(eia.opec.crude, -map, -linechart)
names(eia.opec.crude) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.opec.crude)[5:ncol(eia.opec.crude)])

# Reformat from nested chapters to subchapter structure
eia.opec.crude[3:18,1] <- "OPEC Production - Crude Oil"
eia.opec.crude[20:22,1] <- "OPEC Production Capacity - Crude Oil"
eia.opec.crude[24:26,1] <- "Surplus Production Capacity - Crude Oil"

# Drop empty rows
eia.opec.crude <- subset(eia.opec.crude, !is.na(sourcekey))

# Replace "--" with NA
eia.opec.crude[eia.opec.crude == "--"] <- NA

# Transpose to long format
eia.opec.crude <- pivot_longer(eia.opec.crude, cols = names(eia.opec.crude)[5:ncol(eia.opec.crude)], names_to = "year", values_to = "value",
                               names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.opec.crude$month <- sapply(eia.opec.crude$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.opec.crude$year <- sapply(eia.opec.crude$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## World Petroleum
# Rename and drop unnecessary columns
eia.world.petroleum <- select(eia.world.petroleum, -map, -linechart)
names(eia.world.petroleum) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.world.petroleum)[5:ncol(eia.world.petroleum)])

# Reformat from nested chapters to subchapter structure
eia.world.petroleum[2:21,1] <- "World Petroleum and Other Liquids Consumption"
eia.world.petroleum[23:31,2] <- c("World Index, 2015 Q1 = 100", "Percent change from prior year", "OECD Index, 2015 = 100", "Percent change from prior year",
                                  "Non-OECD Index, 2015 = 100", "Percent change from prior year", "Real U.S. Dollar Exchange Rate",
                                  "Index, 2015 Q1 = 100", "Percent change from prior year")
eia.world.petroleum[23:31,1] <- c("World Real Gross Domestic Product", "World Real Gross Domestic Product", "OECD Real Gross Domestic Product", "OECD Real Gross Domestic Product",
                                  "Non-OECD Real Gross Domestic Product", "Non-OECD Real Gross Domestic Product", "Real U.S. Dollar Exchange Rate",
                                  "Real Oil-Consumption-Weighted U.S. Dollar Exchange Rate", "Real Oil-Consumption-Weighted U.S. Dollar Exchange Rate")

# Drop empty rows
eia.world.petroleum <- subset(eia.world.petroleum, !is.na(sourcekey))

# Replace "--" with NA
eia.world.petroleum[eia.world.petroleum == "--"] <- NA

# Transpose to long format
eia.world.petroleum <- pivot_longer(eia.world.petroleum, cols = names(eia.world.petroleum)[5:ncol(eia.world.petroleum)], names_to = "year", values_to = "value",
                                    names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.world.petroleum$month <- sapply(eia.world.petroleum$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.world.petroleum$year <- sapply(eia.world.petroleum$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## US Petroleum
# Rename and drop unnecessary columns
eia.us.petroleum <- select(eia.us.petroleum, -map, -linechart)
names(eia.us.petroleum) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.us.petroleum)[5:ncol(eia.us.petroleum)])

# Reformat from nested chapters to subchapter structure
eia.us.petroleum[4:30,1] <- "U.S. Petroleum and Other Liquids - Supply"
eia.us.petroleum[32:40,1] <- "U.S. Petroleum and Other Liquids - Consumption"
eia.us.petroleum[45:57,1] <- "End-of-period Inventories"

# Drop empty rows
eia.us.petroleum <- subset(eia.us.petroleum, !is.na(sourcekey))

# Replace "--" with NA
eia.us.petroleum[eia.us.petroleum == "--"] <- NA

# Transpose to long format
eia.us.petroleum <- pivot_longer(eia.us.petroleum, cols = names(eia.us.petroleum)[5:ncol(eia.us.petroleum)], names_to = "year", values_to = "value",
                                 names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.us.petroleum$month <- sapply(eia.us.petroleum$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.us.petroleum$year <- sapply(eia.us.petroleum$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## US hydrocarbon gas liquids
# Rename and drop unnecessary columns
eia.us.hgl <- select(eia.us.hgl, -map, -linechart)
names(eia.us.hgl) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.us.hgl)[5:ncol(eia.us.hgl)])

# Reformat from nested chapters to subchapter structure
eia.us.hgl[3:14,1] <- "HGL Production"
eia.us.hgl[17:20,1] <- "HGL Net Imports"
eia.us.hgl[23:24,1] <- "HGL Refinery and Blender Net Inputs"
eia.us.hgl[27:31,1] <- "HGL Consumption"
eia.us.hgl[34:38,1] <- "HGL Inventories"
eia.us.hgl[41:47,1] <- "Refinery and Blender Net Inputs"
eia.us.hgl[52:58,1] <- "Refinery and Blender Net Production"

# Drop empty rows
eia.us.hgl <- subset(eia.us.hgl, !is.na(sourcekey))

# Replace "--" with NA
eia.us.hgl[eia.us.hgl == "--"] <- NA

# Transpose to long format
eia.us.hgl <- pivot_longer(eia.us.hgl, cols = names(eia.us.hgl)[5:ncol(eia.us.hgl)], names_to = "year", values_to = "value",
                           names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.us.hgl$month <- sapply(eia.us.hgl$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.us.hgl$year <- sapply(eia.us.hgl$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## US Gasoline
# Rename and drop unnecessary columns
eia.us.gasoline <- select(eia.us.gasoline, -map, -linechart)
names(eia.us.gasoline) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.us.gasoline)[5:ncol(eia.us.gasoline)])

# Reformat from nested chapters to subchapter structure
eia.us.gasoline[3:11,1] <- "Gasoline Prices"
eia.us.gasoline[14:23,1] <- "Gasoline End-of-period Inventories"

# Drop empty rows
eia.us.gasoline <- subset(eia.us.gasoline, !is.na(sourcekey))

# Replace "--" with NA
eia.us.gasoline[eia.us.gasoline == "--"] <- NA

# Transpose to long format
eia.us.gasoline <- pivot_longer(eia.us.gasoline, cols = names(eia.us.gasoline)[5:ncol(eia.us.gasoline)], names_to = "year", values_to = "value",
                                names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.us.gasoline$month <- sapply(eia.us.gasoline$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.us.gasoline$year <- sapply(eia.us.gasoline$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## US natgas supply, consumption, inventories
# Rename and drop unnecessary columns
eia.us.natgas.supply <- select(eia.us.natgas.supply, -map, -linechart)
names(eia.us.natgas.supply) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.us.natgas.supply)[5:ncol(eia.us.natgas.supply)])

# Reformat from nested chapters to subchapter structure
eia.us.natgas.supply[3:16,1] <- "Natgas Supply"
eia.us.natgas.supply[18:25,1] <- "Natgas Consumption"
eia.us.natgas.supply[27:33,1] <- "Natgas End-of-period Inventories"

# Drop empty rows
eia.us.natgas.supply <- subset(eia.us.natgas.supply, !is.na(sourcekey))

# Replace "--" with NA
eia.us.natgas.supply[eia.us.natgas.supply == "--"] <- NA

# Transpose to long format
eia.us.natgas.supply <- pivot_longer(eia.us.natgas.supply, cols = names(eia.us.natgas.supply)[5:ncol(eia.us.natgas.supply)], names_to = "year", values_to = "value",
                                     names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.us.natgas.supply$month <- sapply(eia.us.natgas.supply$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.us.natgas.supply$year <- sapply(eia.us.natgas.supply$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## US natgas prices
# Rename and drop unnecessary columns
eia.us.natgas.prices <- select(eia.us.natgas.prices, -map, -linechart)
names(eia.us.natgas.prices) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.us.natgas.prices)[5:ncol(eia.us.natgas.prices)])

# Reformat from nested chapters to subchapter structure
eia.us.natgas.prices[3:36,1] <- "Natgas Prices"

# Drop empty rows
eia.us.natgas.prices <- subset(eia.us.natgas.prices, !is.na(sourcekey))

# Replace "--" with NA
eia.us.natgas.prices[eia.us.natgas.prices == "--"] <- NA

# Transpose to long format
eia.us.natgas.prices <- pivot_longer(eia.us.natgas.prices, cols = names(eia.us.natgas.prices)[5:ncol(eia.us.natgas.prices)], names_to = "year", values_to = "value",
                                     names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.us.natgas.prices$month <- sapply(eia.us.natgas.prices$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.us.natgas.prices$year <- sapply(eia.us.natgas.prices$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



## US coal
# Rename and drop unnecessary columns
eia.us.coal <- select(eia.us.coal, -map, -linechart)
names(eia.us.coal) <- c("chapter", "subchapter", "units", "sourcekey", names(eia.us.coal)[5:ncol(eia.us.coal)])

# Reformat from nested chapters to subchapter structure
eia.us.coal[3:15,1] <- "Coal Supply"
eia.us.coal[17:23,1] <- "Coal Consumption"
eia.us.coal[25:29,1] <- "Coal End-of-period Inventories"
eia.us.coal[31:33,1] <- "Coal Market Indicators"

# Drop empty rows
eia.us.coal <- subset(eia.us.coal, !is.na(sourcekey))

# Replace "--" with NA
eia.us.coal[eia.us.coal == "--"] <- NA

# Transpose to long format
eia.us.coal <- pivot_longer(eia.us.coal, cols = names(eia.us.coal)[5:ncol(eia.us.coal)], names_to = "year", values_to = "value",
                            names_transform = list(year = as.character), values_transform = list(value = as.double))

# Separate year and month
eia.us.coal$month <- sapply(eia.us.coal$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%m"))})
eia.us.coal$year <- sapply(eia.us.coal$year, function(x){as.numeric(format(as.yearmon(x, "%b %Y"), "%Y"))})



### Save cleaned datasets
save(eia.summary, eia.energy.prices, eia.intl.petroleum, eia.non.opec, eia.opec.crude, eia.world.petroleum, eia.us.petroleum,
     eia.us.hgl, eia.us.gasoline, eia.us.natgas.supply, eia.us.natgas.prices, eia.us.coal, source.url, file = "clean data/EIA short term energy outlook monthly.RData")
