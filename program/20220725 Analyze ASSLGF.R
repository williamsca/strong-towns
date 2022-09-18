# Exploratory analysis of ASSLGF data
# Author: Colin Williams
# Last Update: 7/25/2022

# TODO: add state to databuild; use state-by-year FEs

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, ggplot2, stargazer)

dt <- readRDS("derived/County Area Infrastructure Spending and Tax Revenues (1957 - 2012).Rds") 

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

RHS <- paste("ID + Year4 + ", paste(lagHWCapOut, collapse = " + "), sep = "")

fmla <- as.formula(paste0("log(Regular.Hwy.Direct.Exp + 1) ~ ", RHS))
lm.hwyExp <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(Regular.Hwy.Cur.Oper..E44. + 1) ~ ", RHS))
lm.hwyOper <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(Regular.Hwy.Cap.Outlay + 1) ~ ", RHS))
lm.hwyCap <- lm(fmla, data = dt)

covlabs.hw <- paste0("Log(capital outlays + 1) at t-", seq(5, 30, 5))
stargazer(lm.hwyExp, lm.hwyOper, lm.hwyCap, type = "text", omit = c("ID", "Year4"), title = "Highways: Historical Capital Outlays and Contemporary Expenditures",
          dep.var.caption = c("Dependent variable:"),
          column.labels = c("log(Total Expenditures + 1)", "log(Current Operations + 1)", "log(Capital Outlays + 1)"),
          dep.var.labels.include = FALSE,
          omit.labels = c("County FEs", "Year FEs"),
          covariate.labels = c(covlabs.hw))

# Sewerage
lagSEWCapOut <- paste0("L_SEWCapOut_", seq(5,30,5))
dt[, (lagSEWCapOut) := shift(Sewerage.Cap.Outlay, n = seq(1,7,1), type = "lag"), by = .(ID)]
dt[, (lagSEWCapOut) := lapply(.SD + 1, log), .SDcols = lagSEWCapOut]

RHS <- paste("ID + Year4 + ", paste(lagSEWCapOut, collapse = " + "), sep = "")

fmla <- as.formula(paste0("log(Sewerage.Direct.Expend + 1) ~ ", RHS))
lm.sewExp <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(Sewerage.Current.Oper..E80. + 1) ~ ", RHS))
lm.sewOper <- lm(fmla, data = dt)

fmla <- as.formula(paste0("log(Sewerage.Cap.Outlay + 1) ~ ", RHS))
lm.sewCap <- lm(fmla, data = dt)

covlabs.hw <- paste0("Log(capital outlays + 1) at t-", seq(5, 30, 5))
stargazer(lm.sewExp, lm.sewOper, lm.sewCap, type = "text", omit = c("ID", "Year4"), title = "Sewerage: Historical Capital Outlays and Contemporary Expenditures",
          dep.var.caption = c("Dependent variable:"),
          column.labels = c("log(Total Expenditures + 1)", "log(Current Operations + 1)", "log(Capital Outlays + 1)"),
          dep.var.labels.include = FALSE,
          omit.labels = c("County FEs", "Year FEs"),
          covariate.labels = c(covlabs.hw))

# TODO: Water Utilities


# Predicting current taxes given historical capital outlays ----
lagINFCapOut <- paste0("L_INFCapOut_", seq(5,30,5))
dt[, INFCapOut := Sewerage.Cap.Outlay + Regular.Hwy.Cap.Outlay]
dt[, perCapTax := Total.Taxes / Population]

dt[, (lagINFCapOut) := shift(INFCapOut, n = seq(1,7,1), type = "lag"), by = .(ID)]
dt[, (lagINFCapOut) := lapply(.SD + 1, log), .SDcols = lagINFCapOut]

fmla <- as.formula(paste("log(Total.Taxes + 1) ~ ID + Year4 + log(INFCapOut+1) + ", paste(lagINFCapOut, collapse = " + ")))
lm.tax <- lm(fmla, data = dt)

fmla <- as.formula(paste("log(perCapTax + 1) ~ ID + Year4 + log(INFCapOut+1) + ", paste(lagINFCapOut, collapse = " + ")))
lm.taxpcap <- lm(fmla, data = dt)

fmla <- as.formula(paste("log(Population) ~ ID + Year4 + log(INFCapOut+1) + ", paste(lagINFCapOut, collapse = " + ")))
lm.pop <- lm(fmla, data = dt)

covlabs <- paste0("Log(1 + infrastructure capital outlays) at t-", seq(0,30,5))
stargazer(lm.tax, lm.taxpcap, lm.pop, type = "text", omit = c("ID", "Year4"), title = "Relationship between Historical Capital Outlays and Current Taxes",
          dep.var.caption = c("Dependent variable: Log(Total Taxes + 1)"),
          column.labels = c("Total Taxes", "Per-Capita Taxes"), 
          covariate.labels = covlabs,
          omit.labels = c("County FEs", "Year FEs"),
          note = "Infrastructure is defined here to consist of regular highways and sewerage.")




# Operating costs as a share of direct expenditures ----
dt.summary <- dt[, .(hwy = mean(hwyOpShare, na.rm = TRUE), 
                                           sewerage = mean(sewerageOpShare, na.rm = TRUE)
                     ), by = .(Year4)]

dt.summary <- melt(dt.summary, id.vars = c("Year4"))



