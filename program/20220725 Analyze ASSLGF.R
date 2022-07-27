# Exploratory analysis of ASSLGF data
# Author: Colin Williams
# Last Update: 7/25/2022

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, ggplot2, stargazer)

dt <- readRDS("derived/County Area Infrastructure Spending (1957 - 2002).Rds")

# Predicting future operating costs given current capital outlays ----

# Regular highways
lagHWCapOut <- paste0("L_HWCapOut_", seq(5,25,5))
dt[, (lagHWCapOut) := shift(Regular.Hwy.Cap.Outlay, n = seq(1,6,1), type = "lag"), by = .(ID)] # create fields with lagged capital outlays
dt[, (lagHWCapOut) := lapply(.SD + 1, log), .SDcols = lagHWCapOut]

fmla <- as.formula(paste("log(Regular.Hwy.Cur.Oper..E44. + 1) ~ ID + ", paste(lagHWCapOut, collapse = " + ")))
lm.hwy <- lm(fmla, data = dt)

# Sewerage
lagSEWCapOut <- paste0("L_SEWCapOut_", seq(5,25,5))
dt[, (lagSEWCapOut) := shift(Sewerage.Cap.Outlay, n = seq(1,6,1), type = "lag"), by = .(ID)]
dt[, (lagSEWCapOut) := lapply(.SD + 1, log), .SDcols = lagSEWCapOut]

fmla <- as.formula(paste("log(Sewerage.Current.Oper..E80. + 1) ~ ID + ", paste(lagSEWCapOut, collapse = " + ")))
lm.sew <- lm(fmla, data = dt)

covlabs.hw <- paste0("Log highway capital outlays t-", seq(5, 25, 5))
covlabs.sew <- paste0("Log sewerage capital outlays t-", seq(5,25,5))
stargazer(lm.hwy, lm.sew, type = "text", omit = c("ID"), title = "Relationship between Historical Capital Outlays and Current Operating Expenses",
          dep.var.caption = c("Dependent variable: Log current operations expenditure at t"), 
          column.labels = c("Regular Highways", "Sewerage"), 
          dep.var.labels = c("", ""),
          covariate.labels = c(covlabs.hw, covlabs.sew)) 

# Operating costs as a share of direct expenditures ----
dt.summary <- dt[, .(hwy = mean(hwyOpShare, na.rm = TRUE), 
                                           sewerage = mean(sewerageOpShare, na.rm = TRUE), 
                                           sw = mean(swOpShare, na.rm = TRUE)), by = .(Year4)]

dt.summary <- melt(dt.summary, id.vars = c("Year4"))



