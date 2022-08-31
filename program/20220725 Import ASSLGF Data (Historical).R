# Import Annual Survey of State & Local Govt. Finance Data from US Census
# Source: https://www.census.gov/programs-surveys/gov-finances/data/historical-data.html

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, Hmisc, readxl)

# Historical data ----
dt.countyrev <- as.data.table(mdb.get(file = "data/Historical_Finance_Data/County_Area_Fin/County_Area_Finances.mdb", tables = c("1_Revenues")))
dt.countyexpA <- as.data.table(mdb.get(file = "data/Historical_Finance_Data/County_Area_Fin/County_Area_Finances.mdb", tables = c("2_ExpendituresA")))
dt.countyexpB <- as.data.table(mdb.get(file = "data/Historical_Finance_Data/County_Area_Fin/County_Area_Finances.mdb", tables = c("3_ExpendituresB")))

uniqueN(dt.countyrev, by = c("Year4", "ID")) == nrow(dt.countyrev) # TRUE --> an observation is uniquely identified by a Year, ID tuple
uniqueN(dt.countyexpA, by = c("Year4", "ID")) == nrow(dt.countyexpA)
uniqueN(dt.countyexpB, by = c("Year4", "ID")) == nrow(dt.countyexpB)

dt.07 <- fread("data/Historical_Finance_Data/County_Area_Finances_2007/County_Area_Finances_2007.txt")
dt.07[, Year4 := 2007]
dt.12 <- fread("data/Historical_Finance_Data/County_Area_Finances_2012/County_Area_Finances_2012.txt")
dt.12[, Year4 := 2012]
dt.712 <- rbindlist(list(dt.07, dt.12))

saveRDS(dt.countyexpA, file = "derived/County Area Expenditures A (1957-2002).Rds")
saveRDS(dt.countyrev, file = "derived/County Area Revenues (1957-2002).Rds")
saveRDS(dt.countyexpB, file = "derived/County Area Expenditures B (1957-2002).Rds")
saveRDS(dt.712, file = "derived/County Area Finances (2007-2012).Rds")

# dt.city <- as.data.table(mdb.get(file = "data/Historical_Finance_Data/City_Govt_Fin/City_Govt_Finances.mdb", tables = c("City_Govt_Finances")))
# saveRDS(dt.city, file = "derived/City Govt Finances (1951-2006).Rds")
# 
# dt.county <- as.data.table(mdb.get(file = "data/Historical Finance Data/County_Govt_Fin/County_Govt_Finances.mdb", tables = c("County_Govt_Finances")))
# saveRDS(dt.county, file = "derived/County Govt Finances (1957-2006)")




