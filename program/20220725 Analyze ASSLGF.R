# Exploratory analysis of ASSLGF data
# Author: Colin Williams
# Last Update: 7/25/2022

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, ggplot2, stargazer, plm, writexl)

dt <- readRDS("derived/County Area Expenditures (1957-2012).Rds") 
dt.cov <- readRDS("derived/County Covariates (1957-2012).Rds")
dt.cov <- dt.cov[!is.na(FIPS.Code.County)]

dt.exp <- dt[Cat1 == "Expenditure"]
dt.exp[is.na(Cat2), Cat2 := "Other Expenditure"]
dt.exp[Cat2 == "Other Expenditure", Cat3 := "Other Expenditure"]
dt.exp[Cat2 == "Other Expenditure" & is.na(Amt2012), Amt2012 := 0]

# Michigan
dt.permits <- readRDS(file = "derived/County Residential Building Permits (1990-2021).Rds")
dt.mi <- dt.permits[FIPS.Code.State %in% c("26", "39")]

dt.mi <- dt.mi[, .(Bldgs1 = sum(Bldgs1)), by = .(Year4, FIPS.Code.State)]
ggplot(data = dt.mi[Year4 <= 2000], mapping = aes(x = Year4, y = Bldgs1, group = FIPS.Code.State, color = FIPS.Code.State)) +
  geom_point() +
  geom_line(alpha = .5, linetype = "dashed")

# Number of approved residential building permits by county population
ggplot(data = dt.cov[Year4 == 1992 & Population <= 1000000], mapping = aes(x = Population, y = Bldgs1)) +
  geom_point(alpha = .3)

# Regress per-capita infrastructure spending on age of housing stock, approved residential construction permits
# TODO: distinguish capital spending from current operations. should respond differently to residential construction!
dt.cat3exp <- dt.exp[, .(Amt2012 = sum(Amt2012)), by = .(Year4, ID, Cat1, Cat2, Cat3)] # sum over finance codes
dt.cat3exp <- dt.cat3exp[!is.na(Amt2012)]

dt.cat3exp[, OtherExp := sum(Amt2012 * (Cat2 == "Other Expenditure")), by = .(Year4, ID)]

dt.cat3exp <- merge(dt.cat3exp, dt.cov, by = c("ID", "Year4"), all.x = TRUE)
dt.cat3exp[, AmtPCap := Amt2012 * 1000 / Population]
dt.cat3exp[, otherExpPCap := OtherExp * 1000 / Population]

dt.cat3exp[, Cat3 := gsub(" ", "_", Cat3)]
dt.cat3exp[, Cat2 := gsub(" ", "_", Cat2)]
dt.cat3exp <- dcast(dt.cat3exp, ID + Year4 + FIPS.Code.State + FIPS.Code.County + Name.x + 
                      medAge + Population + Bldgs1.5YT + Cat2 + otherExpPCap ~ Cat3,
                    value.var = "AmtPCap", fun.aggregate = sum)

dt.cat3exp[, Other_Expenditure := NULL]
dt.cat3exp <- dt.cat3exp[Cat2 != "Other_Expenditure"]

dt.cat3exp <- dcast(dt.cat3exp, ID + Year4 + FIPS.Code.State + FIPS.Code.County + Name.x + 
                      medAge + Population + Bldgs1.5YT + otherExpPCap ~ Cat2, value.var = c("Current_Operations", "Capital_Outlay"))

lhs <- "medAge + otherExpPCap + Bldgs1.5YT"
lm.currentHwy <- plm(formula(paste0("Current_Operations_Highway ~ ", lhs)) , data = dt.cat3exp, index = c("ID", "Year4"), model = "within")
lm.capHwy <- plm(formula(paste0("Capital_Outlay_Highway ~ ", lhs)), data = dt.cat3exp, index = c("ID", "Year4"), model = "within")

lm.currentSew <- plm(formula(paste0("Current_Operations_Sewerage ~ ", lhs)), data = dt.cat3exp, index = c("ID", "Year4"), model = "within")
lm.capSew <- plm(formula(paste0("Capital_Outlay_Sewerage ~ ", lhs)), data = dt.cat3exp, index = c("ID", "Year4"), model = "within")

lm.currentWS <- plm(formula(paste0("Current_Operations_Water_Supply ~ ", lhs)), data = dt.cat3exp, index = c("ID", "Year4"), model = "within")
lm.capWS <- plm(formula(paste0("Capital_Outlay_Water_Supply ~ ", lhs)), data = dt.cat3exp, index = c("ID", "Year4"), model = "within")

stargazer(lm.currentHwy, lm.capHwy, type = "text")
stargazer(lm.currentSew, lm.capSew, type = "text")
stargazer(lm.currentWS, lm.capWS, type = "text")


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
dt.summary <- dt.exp[, .(Tot_Cat3 = sum(Amt2012)), by = .(Year4, Cat1, Cat2, Cat3)] # sum over finance codes and counties
dt.summary[, Tot_Cat2 := sum(Tot_Cat3, na.rm = TRUE), by = .(Year4, Cat2)]
dt.summary[, Tot_Cat1 := sum(Tot_Cat3, na.rm = TRUE), by = .(Year4)]

dt.summary <- merge(dt.summary, dt.cov[, .(Pop = sum(Population)), by = .(Year4)], by = "Year4")
dt.summary[, `CurOpShare` := Tot_Cat3 * 100 / Tot_Cat1]
dt.summary[, `CurOpPC` := (Tot_Cat3 * 1000) / Pop]
dt.summary[, `ExpShare` := Tot_Cat2 * 100 / Tot_Cat1]
dt.summary[, `ExpPC` := (Tot_Cat2 * 1000) / Pop]

ggplot(data = dt.summary[Year4 >= 1972 & Cat3 == "Current Operations"], aes(x = Year4)) +
  geom_point(mapping = aes(y = `CurOpPC`, color = Cat2), size = 3) +
  geom_line(mapping = aes(y = `CurOpPC`, color = Cat2), linetype = "dashed") +
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 125, 25)) +
  scale_x_continuous(limits = c(1972, 2012), breaks = seq(1972, 2012, 5)) +
  labs(color = "O&M", # title = "Local Government O&M Expenditures", subtitle = "1972-2012", 
       x = "", y = "Spending Per-Capita ($)", 
       caption = "Note: Chart shows current operations expenditures in 2012 dollars based on urban consumer CPI.
Source: Annual Survey of State and Local Governments.") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0), plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color='black'))
ggsave("output/20221128 Local Govt O&M Expenditures.png")

ggplot(data = dt.summary[Year4 >= 1972 & Cat3 == "Capital Outlay"], aes(x = Year4)) +
  geom_point(mapping = aes(y = `CurOpPC`, color = Cat2, group = Cat3), shape = "triangle", size = 3) +
  geom_line(mapping = aes(y = `CurOpPC`, color = Cat2), linetype = "dashed") +
  geom_point(mapping = aes(y = `CurOpPC`, color = Cat2), alpha = .5, size = 3,
             data = dt.summary[Year4 >= 1972 & Cat3 == "Current Operations"]) +
  geom_line(mapping = aes(y = `CurOpPC`, color = Cat2), linetype = "dashed", alpha = .5,
            data = dt.summary[Year4 >= 1972 & Cat3 == "Current Operations"]) +
  
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 125, 25)) +
  scale_x_continuous(limits = c(1972, 2012), breaks = seq(1972, 2012, 5)) +
  labs(color = "Capital Outlays", # title = "Local Government O&M Expenditures", subtitle = "1972-2012", 
       x = "", y = "Spending Per-Capita ($)", 
       caption = "Note: Chart shows current operations expenditures in 2012 dollars based on urban consumer CPI.
Source: Annual Survey of State and Local Governments.") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0), plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color='black'))
ggsave("output/20221128 Local Govt Capital Expenditures.png")

ggplot(data = dt.summary[Year4 >= 1972 & Cat3 == "Current Operations"], aes(x = Year4)) +
  geom_point(mapping = aes(y = `CurOpShare`, color = Cat2)) +
  geom_line(mapping = aes(y = `CurOpShare`, color = Cat2), linetype = "dashed") +
  geom_point(mapping = aes(y = ExpShare, color = Cat2))

# dt.out <- dcast(dt.summary[!is.na(Cat3) & !is.na(Cat2)], Year4 + Cat1 + Cat2 ~ Cat3, value.var = c("Expenditure Share (%)", "Expenditures Per-Capita ($)"))
# write_xlsx(dt.out, path = "output/20221116 Local Government Expenditures (1967-2012).xlsx")

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



