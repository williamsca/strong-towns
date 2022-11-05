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

v.idvars <- c("Year4", "ID")
v.drop <- c("State.Code", "County.Code", "Name", "Sort.Code", "Survey.Year")
dt.countyexpA[, c(v.drop) := NULL]
dt.countyexpB[, c(v.drop) := NULL]
dt.countyexpA <- melt(dt.countyexpA, id.vars = v.idvars, variable.name = "Category", value.name = "Amt")
dt.countyexpB <- melt(dt.countyexpB, id.vars = v.idvars, variable.name = "Category", value.name = "Amt")

dt.countypop <- dt.countyrev[, .(Year4, ID, Name, FIPS.Code.State, FIPS.Code.County, Year.of.Pop.Data, Population)]
dt.countyrev <- dt.countyrev[, c(v.drop, "Pub.Sort", "Census.Region.Code", "FIPS.Code.State", "FIPS.Code.County", "Year.of.Pop.Data", 
                                 "Version", "Source", "Revision.Date", "Data.Flag", "Population") := NULL]
dt.countyrev <- melt(dt.countyrev, id.vars = v.idvars, variable.name = "Category", value.name = "Amt")

dt.category_cw <- as.data.table(read_xls(path = "data/Historical_Finance_Data/County_Area_Fin/_Finance_Publication_Data_Guide.xls",
                                        sheet = "5a.  CoArea Vars", skip = 146, col_names = FALSE))
names(dt.category_cw) <- c("V1", "Code", "SAS_name", "Category", paste0("V", 5:15))
dt.category_cw <- dt.category_cw[!is.na(Code), .(Code, Category, SAS_name)]
dt.category_cw[, Category := gsub(pattern = "[ ()&/-]", ".", Category)]

dt.countyrev[, Amt := as.numeric(Amt)]
dt.countyexpA[, Amt := as.numeric(Amt)]
dt.countyexpB[, Amt := as.numeric(Amt)]

dt.countyfin <- rbindlist(list(dt.countyexpA, dt.countyexpB, dt.countyrev))
dt.countyfin <- merge(dt.countyfin, dt.category_cw, by = c("Category"), all.x = TRUE)
nrow(dt.countyfin[is.na(Code)]) == 0 # 1 --> all categories have a matched code

dt.countyfin[, Total.Exp := max(Amt * (SAS_name == "C301")), by = .(Year4, ID)]
# dt.countyfin[, General.Exp := max(Amt * (SAS_name == "C315")), by = .(Year4, ID)]

table(dt.countyfin[grepl("[EIJXYFGK][0-9]", Code), Code])
table(dt.countyfin[grepl("[LMQS][0-9]", Code), Code])

dt.countyfin[grepl("[EIJXYFGKLS][0-9]", Code), Cat1 := "Expenditure"]
dt.countyfin[, Tot_Cat1 := sum(Amt), by = .(Year4, ID, Cat1)]
# TODO: align totals. Use methodology for summary tabulations information:
# https://www.census.gov/programs-surveys/gov-finances/technical-documentation/classification-manuals.html

head(dt.countyfin[Cat1 == "Expenditure"])


View(dt.countyfin[Year4 == 1997 & ID == 1014])

# v.dupcodes <- unique(dt.category_cw[duplicated(Code), Code])
dt.countyfin <- dt.countyfin[Code == SAS_name] # drop subtotal codes (and some extras that appear irrelevant)
max(duplicated(dt.countyfin, by = c("Year4", "ID", "Code"))) # 0 --> no duplicate code

# The 2007 and 2012 data are provided separately due to changes in how certain revenues and expenditures are categorized for local governments
# See ".../data/Historical_Finance_Data/County_Area_Fin/_Finance_Publication_Data_Guide.xls" tab "5a.  CoArea Vars" for a summary of the changes
dt.07 <- fread("data/Historical_Finance_Data/County_Area_Finances_2007/County_Area_Finances_2007.txt")
dt.07pop <- read_xlsx("data/Historical_Finance_Data/County_Area_Finances_2007/County_Area_Finances_2007_updated.xlsx",
                     sheet = "County_ID_Lookup")
dt.07pop$ID <- as.numeric(dt.07pop$ID)
dt.07pop$Population <- dt.07pop$`Population as of July 1, 2006 (2000-2010 Intercensal Est.)`
dt.07 <- merge(dt.07, subset(dt.07pop, select = c("ID", "Population")), by = "ID")
dt.07[, Year4 := 2007]

dt.12 <- fread("data/Historical_Finance_Data/County_Area_Finances_2012/County_Area_Finances_2012.txt")
dt.12pop <- read_xlsx("data/Historical_Finance_Data/County_Area_Finances_2012/County_Area_Finances_2012.xlsx",
                      sheet = "County_ID_Lookup")
dt.12pop$ID <- as.numeric(dt.12pop$ID)
dt.12pop$Population <- dt.12pop$`Population as of July 1, 2011 (Vintage 2013)`
dt.12 <- merge(dt.12, subset(dt.12pop, select = c("ID", "Population")), by = "ID")
dt.12[, Year4 := 2012]
dt.712 <- rbindlist(list(dt.07, dt.12))

saveRDS(dt.countyfin, file = "derived/County Area Finances (1957-2002).Rds")
saveRDS(dt.countypop, file = "derived/County Area Population (1957-2002).Rds")
saveRDS(dt.712, file = "derived/County Area Finances (2007-2012).Rds")

# dt.city <- as.data.table(mdb.get(file = "data/Historical_Finance_Data/City_Govt_Fin/City_Govt_Finances.mdb", tables = c("City_Govt_Finances")))
# saveRDS(dt.city, file = "derived/City Govt Finances (1951-2006).Rds")
# 
# dt.county <- as.data.table(mdb.get(file = "data/Historical Finance Data/County_Govt_Fin/County_Govt_Finances.mdb", tables = c("County_Govt_Finances")))
# saveRDS(dt.county, file = "derived/County Govt Finances (1957-2006)")




