# Exploratory analysis of ASSLGF data
# Author: Colin Williams
# Last Update: 7/25/2022

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, ggplot2, stargazer)

dt <- readRDS("derived/County Area Infrastructure Spending and Tax Revenues (1957 - 2012).Rds")

# TODO: add contemporary capital outlays
# Predicting current operating costs given historical capital outlays ----

# Regular highways
lagHWCapOut <- paste0("L_HWCapOut_", seq(5,30,5))
dt[, (lagHWCapOut) := shift(Regular.Hwy.Cap.Outlay, n = seq(1,7,1), type = "lag"), by = .(ID)] # create fields with lagged capital outlays
dt[, (lagHWCapOut) := lapply(.SD + 1, log), .SDcols = lagHWCapOut]

fmla <- as.formula(paste("log(Regular.Hwy.Cur.Oper..E44. + 1) ~ ID + ", paste(lagHWCapOut, collapse = " + ")))
lm.hwy <- lm(fmla, data = dt)

# Sewerage
lagSEWCapOut <- paste0("L_SEWCapOut_", seq(5,30,5))
dt[, (lagSEWCapOut) := shift(Sewerage.Cap.Outlay, n = seq(1,7,1), type = "lag"), by = .(ID)]
dt[, (lagSEWCapOut) := lapply(.SD + 1, log), .SDcols = lagSEWCapOut]

fmla <- as.formula(paste("log(Sewerage.Current.Oper..E80. + 1) ~ ID + ", paste(lagSEWCapOut, collapse = " + ")))
lm.sew <- lm(fmla, data = dt)

covlabs.hw <- paste0("Log highway capital outlays t-", seq(5, 30, 5))
covlabs.sew <- paste0("Log sewerage capital outlays t-", seq(5,30,5))
stargazer(lm.hwy, lm.sew, type = "text", omit = c("ID"), title = "Relationship between Historical Capital Outlays and Current Operating Expenses",
          dep.var.caption = c("Dependent variable: Log current operations expenditure at t"), 
          column.labels = c("Regular Highways", "Sewerage"), 
          dep.var.labels = c("", ""),
          covariate.labels = c(covlabs.hw, covlabs.sew)) 

# Predicting current taxes given historical capital outlays ----
lagINFCapOut <- paste0("L_INFCapOut_", seq(5,30,5))
dt[, INFCapOut := Sewerage.Cap.Outlay + Regular.Hwy.Cap.Outlay]
dt[, perCapTax := Total.Taxes / Population]

dt[, (lagINFCapOut) := shift(INFCapOut, n = seq(1,7,1), type = "lag"), by = .(ID)]
dt[, (lagINFCapOut) := lapply(.SD + 1, log), .SDcols = lagINFCapOut]

fmla <- as.formula(paste("log(Total.Taxes + 1) ~ ID + ", paste(lagINFCapOut, collapse = " + ")))
lm.tax <- lm(fmla, data = dt)

fmla <- as.formula(paste("log(perCapTax + 1) ~ ID + ", paste(lagINFCapOut, collapse = " + ")))
lm.taxpcap <- lm(fmla, data = dt)

covlabs <- paste0("Log(1 + highway + sewerage capital outlays) at t-", seq(5,30,5))
stargazer(lm.tax, lm.taxpcap, type = "text", omit = c("ID"), title = "Relationship between Historical Capital Outlays and Current Taxes",
          dep.var.caption = c("Dependent variable: Log(Total Taxes + 1)"),
          column.labels = c("Total Taxes", "Per-Capita Taxes"),
          covariate.labels = covlabs)



# Operating costs as a share of direct expenditures ----
dt.summary <- dt[, .(hwy = mean(hwyOpShare, na.rm = TRUE), 
                                           sewerage = mean(sewerageOpShare, na.rm = TRUE)
                     ), by = .(Year4)]

dt.summary <- melt(dt.summary, id.vars = c("Year4"))



