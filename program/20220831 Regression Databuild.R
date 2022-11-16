rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, readxl, writexl)

# Load data
dt.countyfin <- readRDS(file = "derived/County Area Finances (1957-2002).Rds")
dt.countypop <- readRDS(file = "derived/County Area Population (1957-2002).Rds")
dt.712 <- readRDS(file = "derived/County Area Finances (2007-2012).Rds")

dt.housing <- readRDS(file = "derived/Block Group Housing (2013).Rds")
dt.permits <- readRDS(file = "derived/County Residential Building Permits (1990-2021).Rds")

dt.cpi <- as.data.table(read_xlsx("crosswalks/CPI-U 1967 Dollars (1957-2022).xlsx", skip = 11)) 

dt.gov2fips <- readRDS("crosswalks/GOVS~FIPS.Rds")

# TODO: fix this up?
dt.tab <- as.data.table(read_xlsx("crosswalks/ASSLGF Summary Tabulations.xlsx", 
                      sheet = "Post-2010", skip = 3))
dt.tab <- dt.tab[Line %in% c(86, 87, 102, 103, 113, 114, 115, 116, 117, 118)]

# Regression databuild ----

# combine 1957-2002 and 2007-2012 county panels
dt.countyfin[, c("Category", "SAS_name") := NULL]
dt.countyfin <- merge(dt.countyfin, dt.countypop[, .(ID, Year4, Population)], by = c("ID", "Year4"))

for (col in c("ID", "Year4", "Population")) set(dt.countyfin, j = col, value = as.numeric(dt.countyfin[[col]]))
dt.712[, Population := as.numeric(Population)]
dt <- rbindlist(list(dt.countyfin, dt.712[ID != 0, .(ID, Year4, Amt, Code, Population)]))

# create county covariates (year, ID)
dt.countycov <- unique(dt[, .(Year4, ID, Population)], by = c("Year4", "ID", "Population"))

dt.housing <- unique(dt.housing[medYearBuiltCty > 0, .(medYearBuiltCty, nUnitsCty, state, county)]) # exclude the one county w/o median year built
# dt.housing[, FIPS.Code.State := as.numeric(state)][, FIPS.Code.County := as.numeric(county)]

# TODO: make sure mapping from GOVS to FIPS is accurate across years
dt.countycov <- merge(dt.countycov, dt.gov2fips, by = "ID", all.x = TRUE) # NB: two (county, year) tuples aren't matched to a FIPS

dt.countycov <- merge(dt.countycov, dt.housing, by.x = c("FIPS.Code.State", "FIPS.Code.County"), 
                      by.y = c("state", "county"), all = TRUE)
dt.countycov[, medAge := Year4 - medYearBuiltCty]
dt.permits[, Year4.5Y := floor((Year4 - 1987)/5)*5 + 1992]
dt.permits[, Bldgs1.5YT := sum(Bldgs1), by = .(Year4.5Y, FIPS.Code.State, FIPS.Code.County)] # compute trailing 5-year total for 1-unit residence permits

dt.countycov <- merge(dt.countycov, dt.permits, by = c("FIPS.Code.State", "FIPS.Code.County", "Year4"), all.x = TRUE)

# The '-11111' flag indicates that a particular financial code was unused that year
# I set these observations to NA in order to calculate total expenditures and revenues
# TODO: figure out why some debt codes appear to have legitimately negative amounts
dt[Amt == -11111 & !(Amt %in% c("X08", "Y08")), Amt := NA] 

# Merge in CPI and deflate to 1967 dollars
dt <- merge(dt, dt.cpi[, .(Year, Annual)], by.x = c("Year4"), by.y = c("Year"), all.x = TRUE, all.y = FALSE)
dt[, Amt1967 := Amt / (Annual / 100)][, Amt2012 := Amt / (Annual / 687.761)]

dt[grepl("[EIJXYFGKLS][0-9]", Code), Cat1 := "Expenditure"] # TODO: reconcile this amount with raw subtotals (use import code)
# TODO: reconcile the 2012 figures with Census report
# https://www.census.gov/library/publications/2014/econ/g12-cg-alfin.html

dt[grepl("[EFG]4[45]", Code), Cat2 := "Highway"]
dt[grepl("[EFG]80", Code), Cat2 := "Sewerage"]
dt[grepl("[EFG]91", Code), Cat2 := "Water Supply"] # I91: interest on water utility debt
dt[grepl("E..", Code), Cat3 := "Current Operations"]
dt[grepl("[FG]..", Code), Cat3 := "Capital Outlay"]

saveRDS(dt, file = "derived/County Area Expenditures (1957-2012).Rds")
saveRDS(dt.countycov, "derived/County Covariates (1957-2012).Rds")

# Superseded ----
# Expenditures: 2007 and 2012
# See https://www2.census.gov/govs/pubs/classification/2006_classification_manual.pdf for code descriptions
# codes <- c("E44", "F44", "G44", "E80", "F80", "G80", "E91", "F91", "G91", "I91")
# dt.712[,Current.Exp :=  Amt*grepl("E..", Code)]
# dt.712[, Current.Exp := sum(Current.Exp), by = .(ID, State, County, Year4)]
# # write_xlsx(dt.712[ID == 28092], path = "derived/TEST.xlsx")
# dt.712 <- dt.712[(Code %in% codes | grepl("T..", Code)) & ID != 0] # filter to highwa, sewerage, and water utility expenditures; drop US totals
# 
# # roll up taxes
# dt.712[grepl("T..", Code), Code := "Total.Taxes"]
# dt.712 <- dt.712[, sum(Amt), by = c("ID", "State", "Year4", "Code", "Population", "Current.Exp")]
# dt.712[, Population := as.numeric(Population)]
# 
# dt.712 <- dcast(dt.712, ID + Year4 + Population + State + Current.Exp ~ Code, value.var = c("V1"), fill = 0)
# 
# dt.712[, Regular.Hwy.Direct.Exp := E44 + F44 + G44][, Sewerage.Direct.Expend := E80 + F80 + G80]
# dt.712[, Regular.Hwy.Cap.Outlay := F44 + G44][, Sewerage.Cap.Outlay := F80 + G80]
# dt.712[, Water.Util.Total.Exp := E91 + F91 + G91 + I91][, Water.Util.Cap.Outlay := F91 + G91]
# setnames(dt.712, old = c("E44", "E80", "E91", "State"), 
#          new = c("Regular.Hwy.Cur.Oper..E44.", "Sewerage.Current.Oper..E80.", "Water.Util.Cur.Oper..E91.", "State.Code"))
# dt.712[, `:=`(hwyOpShare = Regular.Hwy.Cur.Oper..E44. / Regular.Hwy.Direct.Exp, sewerageOpShare = Sewerage.Current.Oper..E80. / Sewerage.Direct.Expend)]
# drop <- c("F44", "G44", "F80", "G80", "F91", "G91", "I91")
# dt.712[, (drop) := NULL]
# 
# # Expenditures: 1957-2002
# isGe0 <- function(x) {
#   return(ifelse(x >= 0, x, NA))
# }
# 
# colsA <- c("Regular.Hwy.Cur.Oper..E44.", "Regular.Hwy.Direct.Exp", "Regular.Hwy.Cap.Outlay")
# for (col in colsA) set(dt.countyexpA, j = col, value = isGe0(dt.countyexpA[[col]]))
# 
# colsB <- c("Sewerage.Current.Oper..E80.", "Sewerage.Cap.Outlay", "Sewerage.Direct.Expend",
#            "Water.Util.Cur.Oper..E91.", "Water.Util.Cap.Outlay", "Water.Util.Total.Exp")
# for (col in colsB) set(dt.countyexpB, j = col, value = isGe0(dt.countyexpB[[col]]))
# 
# dt.countyexpA[, hwyOpShare := Regular.Hwy.Cur.Oper..E44. / Regular.Hwy.Direct.Exp]
# dt.countyexpB[,`:=`(sewerageOpShare = Sewerage.Current.Oper..E80. / Sewerage.Direct.Expend)]
# 
# dt.infrastructure <- merge(dt.countyexpA[, .(Year4, ID, hwyOpShare, Regular.Hwy.Cur.Oper..E44., Regular.Hwy.Direct.Exp, Regular.Hwy.Cap.Outlay)],
#                            dt.countyexpB[, .(Year4, ID, sewerageOpShare, Sewerage.Current.Oper..E80., Sewerage.Cap.Outlay, Sewerage.Direct.Expend,
#                                              Water.Util.Cur.Oper..E91., Water.Util.Cap.Outlay, Water.Util.Total.Exp)], 
#                            by = c("Year4", "ID"))
# 
#  # merge in tax revenues, population
# dt.infrastructure <- merge(dt.infrastructure, dt.countyrev[, .(Year4, ID, State.Code, Total.Taxes, Population)], 
#                            by = c("Year4", "ID"))
# 
# dt.infrastructure[, (c("Year4", "ID", "Total.Taxes", "Population", "State.Code")) := lapply(.SD, as.numeric), 
#                   .SDcols = c("Year4", "ID", "Total.Taxes", "Population", "State.Code")]
# 
# # append 2007/2012 data
# dt.infrastructure <- rbindlist(list(dt.infrastructure, dt.712), use.names = TRUE)
# 
# dt.infrastructure[, min(State.Code == trunc(ID/1000))] # 1 --> State.Code is the first digits of ID
# 
# min(between(dt.infrastructure[, hwyOpShare], 0, 1), na.rm = TRUE) # 1 --> operating expenses as a fraction of all direct expenditures is on the interval [0,1]
# min(between(dt.infrastructure[, sewerageOpShare], 0, 1), na.rm = TRUE)
# 
# dt.infrastructure <- merge(dt.infrastructure, dt.cpi[, .(Year, Annual)], by.x = c("Year4"), by.y = c("Year"), all.x = TRUE, all.y = FALSE)
# 
# dt.infrastructure[, (colsA) := lapply(.SD, "/", Annual / 100), .SDcols = colsA]
# dt.infrastructure[, (colsB) := lapply(.SD, "/", Annual / 100), .SDcols = colsB]
# 
# ids <- c("Year4", "ID")
# dt.infrastructure[, (ids) := lapply(.SD, factor), .SDcols = ids]
# 
# summary(dt.infrastructure[, Sewerage.Current.Oper..E80. + Sewerage.Cap.Outlay - Sewerage.Direct.Expend]) # verify that operations plus capital outlay equal direct expenditures
# summary(dt.infrastructure[, Regular.Hwy.Cur.Oper..E44. + Regular.Hwy.Cap.Outlay - Regular.Hwy.Direct.Exp])
# 
# saveRDS(dt.infrastructure, file = "derived/County Area Infrastructure Spending and Tax Revenues (1957 - 2012).Rds")
# 
