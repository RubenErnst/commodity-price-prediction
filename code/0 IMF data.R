rm(list = ls())

### Load IMF data and clean it

library(tidyverse)
library(IMFData)


# Laod IMF data
oil.production.index <- CompactDataMethod("AOMPC_IX")
oil.production.perc.yoy <- CompactDataMethod("AOMPC_PC_CP_A_PT")
oil.production.perc.previous <- CompactDataMethod("AOMPC_PC_PP_PT")

commodities.prices.export <- CompactDataMethod("PXP_IX")
commodities.prices.import <- CompactDataMethod("PMP_IX")
commodities.prices.producer <- CompactDataMethod("PPPI_IX")
commodities.prices.industrial.producer <- CompactDataMethod("PPPII_IX")
