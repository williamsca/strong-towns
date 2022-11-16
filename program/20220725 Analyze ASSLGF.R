# Exploratory analysis of ASSLGF data
# Author: Colin Williams
# Last Update: 7/25/2022

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, ggplot2, stargazer, plm)

dt <- readRDS("derived/County Area Expenditures (1957-2012).Rds") 
dt.cov <- readRDS("derived/County Covariates (1957-2012).Rds")
dt.cov <- dt.cov[!is.na(FIPS.Code.County)]

dt.exp <- dt[Cat1 == "Expenditure"]

# Number of approved residential building permits by county population
ggplot(data = dt.cov[Year4 == 1992 & Population <= 1000000], mapping = aes(x = Population, y = Bldgs1)) +
  geom_point(alpha = .3)

# Regress per-capita infrastructure spending on age of housing stock, approved residential construction permits
# TODO: distinguish capital spending from current operations. should respond differently to residential construction!
dt.cat2exp <- dt.exp[, .(Amt1967 = sum(Amt1967)), by = .(Year4, ID, Cat1, Cat2)] # sum over finance codes
dt.cat2exp <- dt.cat2exp[!is.na(Amt1967)]
dt.cat2exp[is.na(Cat2), Cat2 := "Other Expenditure"]

dt.cat2exp <- merge(dt.cat2exp, dt.cov, by = c("ID", "Year4"), all.x = TRUE)
dt.cat2exp[, AmtPCap := Amt1967 * 1000 / Population]

dt.cat2exp <- dcast(dt.cat2exp, ID + Year4 + FIPS.Code.State + FIPS.Code.County + Name.x + medAge + Population + Bldgs1.5YT ~ Cat2, value.var = "AmtPCap")
setnames(dt.cat2exp, c("Regular Highway", "Sewerage", "Water Supply", "Other Expenditure"),
         c("RegHwy", "Sewerage", "WaterSup", "OtherExp"))

lm.expHwy <- plm(RegHwy ~ medAge + OtherExp + Bldgs1.5YT, data = dt.cat2exp, index = c("ID", "Year4"), model = "within")
lm.expSew <- plm(Sewerage ~ medAge + OtherExp + Bldgs1.5YT, data = dt.cat2exp, index = c("ID", "Year4"), model = "within")
lm.expH20 <- plm(WaterSup ~ medAge + OtherExp + Bldgs1.5YT, data = dt.cat2exp, index = c("ID", "Year4"), model = "within")

# Direct expenditure on water supply and sewerage is higher in counties with an older housing stock;
# expenditure on roads is somewhat lower (density)?
stargazer(lm.expHwy, lm.expSew, lm.expH20, type = "text", omit = c("Year4"),
          dep.var.labels = c("Regular Highways", "Sewerage", "Water Supply"), #           column.labels = c("Regular Highways", "Sewerage", "Water Supply"),
          covariate.labels = c("Median age of housing units", "Other government expenditures", "# of residential building permits"))

# Share of spending on highway, sewerage, water utility current operation

# Compute infrastructure spending as % of total and per-capita amounts ----
dt.summary <- dt.exp[, .(Tot_Cat2 = sum(Amt)), by = .(Year4, Cat1, Cat2)] # sum over finance codes and counties
dt.summary[, Tot_Cat1 := sum(Tot_Cat2), by = .(Year4)]

# TRUE --> totals are consistent
all.equal(dt.summary[is.na(Cat2) & Year4 >= 1972, .(Year4, Cat1, Tot_Cat1)], 
          dt[Cat1 == "Expenditure" & Year4 >= 1972, .(Tot_Cat1 = sum(Amt, na.rm = TRUE)), by = .(Year4, Cat1)]) 

dt.summary <- merge(dt.summary, dt.pop[, .(Pop = sum(Population)), by = .(Year4)], by = "Year4")
dt.summary[, `Expenditure Share (%)` := Tot_Cat2 / Tot_Cat1]
dt.summary[, `Expenditures Per-Capita ($)` := Tot_Cat2 / Pop]


# SUPSERSEDED ----
# Trends in direct infrastructure expenditures ----
dt.tot <- dt[, .(`Regular Highways` = sum(Regular.Hwy.Direct.Exp), Sewerage = sum(Sewerage.Direct.Expend), `Water Utilities` = sum(Water.Util.Total.Exp)),
                 by = .(Year = Year4)]
dt.tot <- melt(dt.tot, id.vars = c("Year"), variable.name = "Infrastructure Type", value.name = "Direct Expenditures")
ggplot(dt.tot, aes(x = Year, y = `Direct Expenditures`, group = `Infrastructure Type`, color = `Infrastructure Type`)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  labs(title = "Total Local Government Infrastructure Spending",
       subtitle = "1957 - 2012", y = "Direct Expenditures ($1000s)",
       caption = "Source: Annual Survey of State and Local Governments")

# Predicting current expenditures given historical capital outlays ----
# Regular highways
lagHWCapOut <- paste0("L_HWCapOut_", seq(5,30,5))
dt[, (lagHWCapOut) := shift(Regular.Hwy.Cap.Outlay, n = seq(1,7,1), type = "lag"), by = .(ID)] # create fields with lagged capital outlays
dt[, (lagHWCapOut) := lapply(.SD + 1, log), .SDcols = lagHWCapOut]

RHS <- paste("ID + State.Code*Year4 + ", paste(lagHWCapOut, collapse = " + "), sep = "")

fmla <- as.formula(paste0("log(Regular.Hwy.Direct.Exp + 1) ~ ", RHS))
lm.hwyExp <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(Regular.Hwy.Cur.Oper..E44. + 1) ~ ", RHS))
lm.hwyOper <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(Regular.Hwy.Cap.Outlay + 1) ~ ", RHS))
lm.hwyCap <- lm(fmla, data = dt)

covlabs.hw <- paste0("Log(capital outlays + 1) at t-", seq(5, 30, 5))
stargazer(lm.hwyExp, lm.hwyOper, lm.hwyCap, type = "text", omit = c("ID", "Year4", "State"), title = "Highways: Historical Capital Outlays and Contemporary Expenditures",
          dep.var.caption = c("Dependent variable:"),
          column.labels = c("log(Total Expenditures + 1)", "log(Current Operations + 1)", "log(Capital Outlays + 1)"),
          dep.var.labels.include = FALSE,
          omit.labels = c("County FEs", "Year FEs", "State-by-Year FEs"),
          covariate.labels = c(covlabs.hw))

# Sewerage
lagSEWCapOut <- paste0("L_SEWCapOut_", seq(5,30,5))
dt[, (lagSEWCapOut) := shift(Sewerage.Cap.Outlay, n = seq(1,7,1), type = "lag"), by = .(ID)]
dt[, (lagSEWCapOut) := lapply(.SD + 1, log), .SDcols = lagSEWCapOut]

RHS <- paste("ID + State.Code*Year4 + ", paste(lagSEWCapOut, collapse = " + "), sep = "")

fmla <- as.formula(paste0("log(Sewerage.Direct.Expend + 1) ~ ", RHS))
lm.sewExp <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(Sewerage.Current.Oper..E80. + 1) ~ ", RHS))
lm.sewOper <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(Sewerage.Cap.Outlay + 1) ~ ", RHS))
lm.sewCap <- lm(fmla, data = dt)

covlabs.hw <- paste0("Log(capital outlays + 1) at t-", seq(5, 30, 5))
stargazer(lm.sewExp, lm.sewOper, lm.sewCap, type = "text", omit = c("ID", "Year4", "State.Code"), 
          title = "Sewerage: Historical Capital Outlays and Contemporary Expenditures",
          dep.var.caption = c("Dependent variable:"),
          column.labels = c("log(Total Expenditures + 1)", "log(Current Operations + 1)", "log(Capital Outlays + 1)"),
          dep.var.labels.include = FALSE,
          omit.labels = c("County FEs", "Year FEs", "State-by-Year FEs"),
          covariate.labels = c(covlabs.hw))

# TODO: Water Utilities


# Predicting current taxes given historical capital outlays ----
lagINFCapOut <- paste0("L_INFCapOut_", seq(5,30,5))
dt[, INFCapOut := Sewerage.Cap.Outlay + Regular.Hwy.Cap.Outlay]
dt[, perCapTax := Total.Taxes / Population]

dt[, (lagINFCapOut) := shift(INFCapOut, n = seq(1,7,1), type = "lag"), by = .(ID)]
dt[, (lagINFCapOut) := lapply(.SD + 1, log), .SDcols = lagINFCapOut]

RHS <- paste0("ID + State.Code*Year4 + log(INFCapOut+1) + ", paste(lagINFCapOut, collapse = " + "))

fmla <- as.formula(paste0("log(Total.Taxes + 1) ~ ", RHS))
lm.tax <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(perCapTax + 1) ~ ", RHS))
lm.taxpcap <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(Population) ~ ", RHS))
lm.pop <- lm(fmla, data = dt)

covlabs <- paste0("Log(1 + infrastructure capital outlays) at t-", seq(0,30,5))
stargazer(lm.tax, lm.taxpcap, lm.pop, type = "text", omit = c("ID", "Year4", "State.Code"), 
          title = "Relationship between Historical Capital Outlays and Current Taxes",
          dep.var.caption = c("Dependent variable: Log(Total Taxes + 1)"),
          column.labels = c("Total Taxes", "Per-Capita Taxes"), 
          covariate.labels = covlabs,
          omit.labels = c("County FEs", "Year FEs", "State-by-Year FEs"),
          note = "Infrastructure is defined here to consist of regular highways and sewerage.")




# Operating costs as a share of direct expenditures ----
dt.summary <- dt[, .(hwy = mean(hwyOpShare, na.rm = TRUE), 
                                           sewerage = mean(sewerageOpShare, na.rm = TRUE)
                     ), by = .(Year4)]

dt.summary <- melt(dt.summary, id.vars = c("Year4"))



