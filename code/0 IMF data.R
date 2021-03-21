rm(list = ls())

### Load IMF data and clean it

library(tidyverse)
library(IMFData)
library(stringr)


# Laod IMF data
# oil.production.index <- CompactDataMethod("AOMPC_IX")
# oil.production.perc.yoy <- CompactDataMethod("AOMPC_PC_CP_A_PT")
# oil.production.perc.previous <- CompactDataMethod("AOMPC_PC_PP_PT")
# 
# commodities.prices.export <- CompactDataMethod("PXP_IX")
# commodities.prices.import <- CompactDataMethod("PMP_IX")
# commodities.prices.producer <- CompactDataMethod("PPPI_IX")
# commodities.prices.industrial.producer <- CompactDataMethod("PPPII_IX")

# Read bulk downloaded PCPS
pcps <- read_delim("data/PCPS_03-08-2021 06-32-19-47_timeSeries.csv", delim = ",")
pcps <- pivot_longer(pcps, names(pcps)[8:ncol(pcps)], names_to = "month", values_to = "value")
names(pcps) <- c("country.name", "country.code", "commodity.name", "commodity.code", "unit.name", "unit.code", "attribute", "month", "value")

# Read aux table
commodities <- openxlsx::read.xlsx("aux files/PCPS commodities.xlsx")

# Merge with PCPS
pcps <- merge(pcps, select(commodities, commodity.code, commodity.category), by = "commodity.code", all.x = T)

# Remove whitespace
pcps$commodity.code <- trimws(pcps$commodity.code, "both")
pcps$country.name <- trimws(pcps$country.name, "both")
pcps$country.code <- trimws(pcps$country.code, "both")
pcps$commodity.name <- trimws(pcps$commodity.name, "both")
pcps$unit.name <- trimws(pcps$unit.name, "both")
pcps$unit.code <- trimws(pcps$unit.code, "both")
pcps$attribute <- trimws(pcps$attribute, "both")
pcps$month <- trimws(pcps$month, "both")
pcps$commodity.category <- trimws(pcps$commodity.category, "both")

# Create overview over amount of data per category
category.overview <- aggregate(value ~ commodity.category, subset(pcps, !is.na(value)), length)

save(pcps, file = "clean data/PCPS_all.RData")



##### Brent monthly ----
brent.monthly <- subset(pcps, commodity.name == "Brent Crude" & grepl(".*M.*", month))

# Extract months and years
brent.monthly$year <- as.numeric(str_extract(brent.monthly$month, "\\d{4}"))
brent.monthly$month <- as.numeric(str_remove(brent.monthly$month, "\\d{4}M"))

# Sort timeseries
brent.monthly <- arrange(brent.monthly, year, month)


### Extract return series
brent.monthly.return <- subset(brent.monthly, unit.code == "PC_PP_PT")

# Drop empty, duplicate rows
brent.monthly.return <- subset(brent.monthly.return, !is.na(value))

# Convert from percentage points to percent
brent.monthly.return$value <- brent.monthly.return$value / 100


### Extract absolute value series
brent.monthly.absolute <- subset(brent.monthly, unit.code == "USD")

# Drop empty, duplicate rows
brent.monthly.absolute <- subset(brent.monthly.absolute, !is.na(value))



##### WTI monthly ----
wti.monthly <- subset(pcps, commodity.name == "WTI Crude" & grepl(".*M.*", month))

# Extract months and years
wti.monthly$year <- as.numeric(str_extract(wti.monthly$month, "\\d{4}"))
wti.monthly$month <- as.numeric(str_remove(wti.monthly$month, "\\d{4}M"))

# Sort timeseries
wti.monthly <- arrange(wti.monthly, year, month)


### Extract return series
wti.monthly.return <- subset(wti.monthly, unit.code == "PC_PP_PT")

# Drop empty, duplicate rows
wti.monthly.return <- subset(wti.monthly.return, !is.na(value))

# Convert from percentage points to percent
wti.monthly.return$value <- wti.monthly.return$value / 100


### Extract absolute value series
wti.monthly.absolute <- subset(wti.monthly, unit.code == "USD")

# Drop empty, duplicate rows
wti.monthly.absolute <- subset(wti.monthly.absolute, !is.na(value))



##### Dubai monthly ----
dubai.monthly <- subset(pcps, commodity.name == "Dubai Crude" & grepl(".*M.*", month))

# Extract months and years
dubai.monthly$year <- as.numeric(str_extract(dubai.monthly$month, "\\d{4}"))
dubai.monthly$month <- as.numeric(str_remove(dubai.monthly$month, "\\d{4}M"))

# Sort timeseries
dubai.monthly <- arrange(dubai.monthly, year, month)


### Extract return series
dubai.monthly.return <- subset(dubai.monthly, unit.code == "PC_PP_PT")

# Drop empty, duplicate rows
dubai.monthly.return <- subset(dubai.monthly.return, !is.na(value))

# Convert from percentage points to percent
dubai.monthly.return$value <- dubai.monthly.return$value / 100


### Extract absolute value series
dubai.monthly.absolute <- subset(dubai.monthly, unit.code == "USD")

# Drop empty, duplicate rows
dubai.monthly.absolute <- subset(dubai.monthly.absolute, !is.na(value))



##### APSP monthly ----
apsp.monthly <- subset(pcps, commodity.name == "APSP crude oil($/bbl)" & grepl(".*M.*", month))

# Extract months and years
apsp.monthly$year <- as.numeric(str_extract(apsp.monthly$month, "\\d{4}"))
apsp.monthly$month <- as.numeric(str_remove(apsp.monthly$month, "\\d{4}M"))

# Sort timeseries
apsp.monthly <- arrange(apsp.monthly, year, month)


### Extract return series
apsp.monthly.return <- subset(apsp.monthly, unit.code == "PC_PP_PT")

# Drop empty, duplicate rows
apsp.monthly.return <- subset(apsp.monthly.return, !is.na(value))

# Convert from percentage points to percent
apsp.monthly.return$value <- apsp.monthly.return$value / 100


### Extract absolute value series
apsp.monthly.absolute <- subset(apsp.monthly, unit.code == "USD")

# Drop empty, duplicate rows
apsp.monthly.absolute <- subset(apsp.monthly.absolute, !is.na(value))



##### Nat Gas US monthly ----
natgas.us.monthly <- subset(pcps, commodity.name == "Natural Gas, US Henry Hub Gas" & grepl(".*M.*", month))

# Extract months and years
natgas.us.monthly$year <- as.numeric(str_extract(natgas.us.monthly$month, "\\d{4}"))
natgas.us.monthly$month <- as.numeric(str_remove(natgas.us.monthly$month, "\\d{4}M"))

# Sort timeseries
natgas.us.monthly <- arrange(natgas.us.monthly, year, month)


### Extract return series
natgas.us.monthly.return <- subset(natgas.us.monthly, unit.code == "PC_PP_PT")

# Drop empty, duplicate rows
natgas.us.monthly.return <- subset(natgas.us.monthly.return, !is.na(value))

# Convert from percentage points to percent
natgas.us.monthly.return$value <- natgas.us.monthly.return$value / 100


### Extract absolute value series
natgas.us.monthly.absolute <- subset(natgas.us.monthly, unit.code == "USD")

# Drop empty, duplicate rows
natgas.us.monthly.absolute <- subset(natgas.us.monthly.absolute, !is.na(value))





### Save cleaned datasets
save(apsp.monthly, apsp.monthly.absolute, apsp.monthly.return, file = "clean data/APSP monthly.RData")
save(brent.monthly, brent.monthly.absolute, brent.monthly.return, file = "clean data/Brent Crude monthly.RData")
save(dubai.monthly, dubai.monthly.absolute, dubai.monthly.return, file = "clean data/Dubai Crude monthly.RData")
save(natgas.us.monthly, natgas.us.monthly.absolute, natgas.us.monthly.return, file = "clean data/Nat Gas Henry Hub monthly.RData")
save(wti.monthly, wti.monthly.absolute, wti.monthly.return, file = "clean data/WTI Crude monthly.RData")
