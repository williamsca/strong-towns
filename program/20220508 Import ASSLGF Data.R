# Import Annual Survey of State & Local Govt. Finance Data from US Census
# Source: https://www.census.gov/programs-surveys/gov-finances/data/datasets.html

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, readr, readxl)

# Finance Data (2009-2019) ----
# TODO: add years
df.raw <- readr::read_fwf(file = "data/Contemporary_Finance_Data/2017_Individual_Unit_File/2017FinEstDAT_06102021modp_pu.txt",
                             fwf_widths(c(2, 1, 3, 6, 3, 12, 4, 1),
                                        c("FIPS State", "Type", "FIPS County", "Unit", "Item", "Amount", "Year", "Flag")))


dt <- as.data.table(df.raw) # Eventually: rbindlist() on a list of data.frame objects for each year
dt[, Amount := Amount * 1000]

saveRDS(dt, "derived/Finance Data (2019).Rds")

dt.summary <- as.data.table(readr::read_fwf(file = "data/Contemporary_Finance_Data/2019_individual_unit_file/19statetypepu.txt",
                              fwf_positions(start = c(1,3,5,9,23,34), end = c(2,3,7,20,32,35),
                                         c("FIPS State", "Level", "Item", "Amount", "CV", "Year"))))

# Crosswalks (entity names and unit code aggregations) ----
df.names <- readr::read_fwf(file = "data/Contemporary_Finance_Data/2019_individual_unit_file/Fin_PID_2019.txt",
                         fwf_widths(c(2,1,3,6,64,35,5,9,2,7,2,2,2,4,2),
                                    c("FIPS State", "Type", "FIPS County", "Unit", "Name", "County", "FIPS Place",
                                      "Pop", "Pop Year", "Enrollment", "Enr Year", "Fnc Code", "School Code", "Fisc Year End", "Year")))

dt.codes <- as.data.table(readxl::read_xls("data/Contemporary_Finance_Data/methodology_for_summary_tabulations.xls", skip = 1))

categories <- paste0("Cat", seq(1,10,1))
setnames(dt.codes, c("Line", categories, "Item Codes"))

vars <- paste0("V", seq(1,205,1))
dt.codes[, (vars):=(tstrsplit(`Item Codes`, split = ", "))]
dt.codes <- melt(dt.codes, id.vars = c(categories), measure = patterns("V"),
                 value.name = "Item", na.rm = TRUE)
dt.codes[, variable := NULL]
saveRDS(dt.codes, "derived/CROSSWALK Item Codes.Rds")


# Replicate 2019 Summary Report ----
# See file '.../reference/alfin_summary_brief.pdf' for reported figures
# Differences are likely due to rounding and the June 2022 data revision

dt <- readRDS("derived/Finance Data (2019).Rds")
dt.itmcw <- readRDS("derived/CROSSWALK Item Codes.Rds")

PctDiff <- function (x,y) {
  2*(x-y)/(x+y)
}

# Revenues
PctDiff(sum(merge(dt, dt.itmcw[Cat1 == "Total Revenue"], by = "Item")[, Amount]), 4.1*10^12)

PctDiff(sum(merge(dt, dt.itmcw[Cat2 == "General Revenue"], by = "Item")[, Amount]), 3.5*10^12)
PctDiff(sum(merge(dt, dt.itmcw[Cat2 == "Insurance Trust Revenue"], by = "Item")[, Amount]), 417.2*10^9)
PctDiff(sum(merge(dt, dt.itmcw[Cat2 == "Liquor Store Revenue"], by = "Item")[, Amount]), 10.7*10^9)

PctDiff(sum(merge(dt, dt.itmcw[Cat4 == "Total Taxes"], by = "Item")[, Amount]), 1.9*10^12)

PctDiff(sum(merge(dt, dt.itmcw[Cat5 == "Sales and Gross Receipts"], by = "Item")[, Amount]), 640.6*10^9)
PctDiff(sum(merge(dt, dt.itmcw[Cat5 == "Property Tax"], by = "Item")[, Amount]), 577*10^9)

PctDiff(sum(merge(dt, dt.itmcw[Cat4 == "From Federal"], by = "Item")[, Amount]), 762.1*10^9)
PctDiff(sum(merge(dt, dt.itmcw[Cat2 == "Current Charges"], by = "Item")[, Amount]), 574.5*10^9)

# Expenditures
PctDiff(sum(merge(dt, dt.itmcw[Cat1 == "Total Expenditure"], by = "Item")[, Amount]), 4.0*10^12)

PctDiff(sum(merge(dt, dt.itmcw[Cat3 == "Education"], by = "Item")[, Amount]), 1.1*10^12)

# Balance Sheet
PctDiff(sum(merge(dt, dt.itmcw[Cat2 == "Debt Outstanding"], by = "Item")[, Amount]), 3.2*10^12)

# NOTE: the data apparently do not include all security holdings item codes
PctDiff(sum(merge(dt, dt.itmcw[Cat2 == "Cash and Security Holdings"], by = "Item")[, Amount]), 7.6*10^12) # bad
PctDiff(sum(merge(dt, dt.itmcw[Cat2 == "Insurance Trust Funds"], by = "Item")[, Amount]), 4.7*10^12) # bad

