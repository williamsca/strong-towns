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

saveRDS(dt.countyexpA, file = "derived/County Area Expenditures A (1957-2002).Rds")
saveRDS(dt.countyrev, file = "derived/County Area Revenues (1957-2002).Rds")
saveRDS(dt.countyexpB, file = "derived/County Area Expenditures B (1957-2002).Rds")

dt.cpi <- as.data.table(read_xlsx("data/SeriesReport-20220726093626_570e10.xlsx", skip = 11))

# dt.city <- as.data.table(mdb.get(file = "data/Historical_Finance_Data/City_Govt_Fin/City_Govt_Finances.mdb", tables = c("City_Govt_Finances")))
# saveRDS(dt.city, file = "derived/City Govt Finances (1951-2006).Rds")
# 
# dt.county <- as.data.table(mdb.get(file = "data/Historical Finance Data/County_Govt_Fin/County_Govt_Finances.mdb", tables = c("County_Govt_Finances")))
# saveRDS(dt.county, file = "derived/County Govt Finances (1957-2006)")

# Regression databuild ----
isGe0 <- function(x) {
  return(ifelse(x >= 0, x, NA))
}

colsA <- c("Regular.Hwy.Cur.Oper..E44.", "Regular.Hwy.Direct.Exp", "Regular.Hwy.Cap.Outlay")
for (col in colsA) set(dt.countyexpA, j = col, value = isGe0(dt.countyexpA[[col]]))

colsB <- c("Sewerage.Current.Oper..E80.", "Sewerage.Cap.Outlay", "Sewerage.Direct.Expend", "SW.Mgmt.Current.Oper..E81.", "SW.Mgmt.Direct.Expend", "SW.Mgmt.Capital.Outlay")
for (col in colsB) set(dt.countyexpB, j = col, value = isGe0(dt.countyexpB[[col]]))

dt.countyexpA[, hwyOpShare := Regular.Hwy.Cur.Oper..E44. / Regular.Hwy.Direct.Exp]
dt.countyexpB[,`:=`(sewerageOpShare = Sewerage.Current.Oper..E80. / Sewerage.Direct.Expend, swOpShare = SW.Mgmt.Current.Oper..E81. / SW.Mgmt.Direct.Expend)]

dt.infrastructure <- merge(dt.countyexpA[, .(Year4, ID, hwyOpShare, Regular.Hwy.Cur.Oper..E44., Regular.Hwy.Direct.Exp, Regular.Hwy.Cap.Outlay)], 
                           dt.countyexpB[, .(Year4, ID, sewerageOpShare, swOpShare, Sewerage.Current.Oper..E80., Sewerage.Cap.Outlay, Sewerage.Direct.Expend,
                                            SW.Mgmt.Current.Oper..E81., SW.Mgmt.Direct.Expend, SW.Mgmt.Capital.Outlay)], by = c("Year4", "ID"))
dt.infrastructure <- merge(dt.infrastructure, dt.countyrev[, .(Year4, ID, FIPS.Code.State, FIPS.Code.County)], by = c("Year4", "ID"))

min(between(dt.infrastructure[, hwyOpShare], 0, 1), na.rm = TRUE) # 1 --> operating expenses as a fraction of all direct expenditures is on the interval [0,1]
min(between(dt.infrastructure[, sewerageOpShare], 0, 1), na.rm = TRUE)
min(between(dt.infrastructure[, swOpShare], 0, 1), na.rm = TRUE)

dt.infrastructure <- merge(dt.infrastructure, dt.cpi[, .(Year, Annual)], by.x = c("Year4"), by.y = c("Year"), all.x = TRUE, all.y = FALSE)

dt.infrastructure[, (colsA) := lapply(.SD, "/", Annual / 100), .SDcols = colsA]
dt.infrastructure[, (colsB) := lapply(.SD, "/", Annual / 100), .SDcols = colsB]

ids <- c("Year4", "ID")
dt.infrastructure[, (ids) := lapply(.SD, factor), .SDcols = ids]

summary(dt.infrastructure[, Sewerage.Current.Oper..E80. + Sewerage.Cap.Outlay - Sewerage.Direct.Expend]) # verify that operations plus capital outlay equal direct expenditures
summary(dt.infrastructure[, Regular.Hwy.Cur.Oper..E44. + Regular.Hwy.Cap.Outlay - Regular.Hwy.Direct.Exp])

saveRDS(dt.infrastructure, file = "derived/County Area Infrastructure Spending (1957 - 2002).Rds")



