# Import Building Permit Survey Data
# Source: https://www2.census.gov/econ/bps/County/

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, bit64)

url <- "https://www2.census.gov/econ/bps/County/"
l.files <- paste0(url, "co", 1990:2021, "a.txt")

dt <- rbindlist(lapply(l.files, fread, header = FALSE, blank.lines.skip = TRUE, 
                       fill = TRUE, skip = 2, sep =))

colnames <- paste0(paste0(rep(c("Bldgs", "Units", "Value"), 4)), 
                   c(rep("1", 3), rep("2", 3), rep("3-4", 3), rep("5+", 3)))
colnames <- c(c("Date", "FIPS.Code.State", "FIPS.Code.County", "Region Code", "County Code", "Name"), 
              colnames, paste0(colnames, "-rep"))
setnames(dt, new = colnames)

dt[Date > 9000, Year4 := (Date - 99)/100 + 1900]
dt[is.na(Year4), Year4 := Date]

# Sanity checks ----
# The 2 and 3-4 checks fail for a handful of rows
nrow(dt[Bldgs1 != Units1]) == 0 # TRUE --> every 1-unit building has 1 unit
nrow(dt[2*Bldgs2 != Units2]) == 0 # TRUE --> every 2-unit building has 2 units (some errors in data here)
nrow(dt[between(`Units3-4`, 3*`Bldgs3-4`, 4*`Bldgs3-4`)]) == 0 # TRUE --> every 3-4 unit building has at least 3 units
nrow(dt[5*`Bldgs5+` > `Units5+`]) == 0 # TRUE --> every 5+ unit building has more than 5 units

# Again, in some cases the imputed values are smaller than the reported
for (val in c("Bldgs", "Units", "Value")) {
  for (units in c("1", "2", "3-4", "5+")) {
    var <- paste0(val, units)
    print(min(dt[[var]] >= dt[[paste0(var, "-rep")]])) # 1 --> imputed figures always exceed reported
  }
}

saveRDS(dt, file = "derived/County Residential Building Permits (1990-2021).Rds")


# Testing ----
dt90 <- fread(paste0(url, "co1990a.txt"), header = FALSE, blank.lines.skip = TRUE, 
              fill = TRUE, skip = 2, sep = ",")
dt91 <- fread(paste0(url, "co1991a.txt"), header = FALSE, blank.lines.skip = TRUE, fill = TRUE, skip = 2)

dt08 <- fread(paste0(url, "co2008a.txt"), header = FALSE, blank.lines.skip = TRUE, fill = TRUE, skip = 0)

dt.y <- fread(paste0(url, "co0002y.txt"), header = TRUE, blank.lines.skip = TRUE)
dt.c <- fread(paste0(url, "co0002c.txt"), header = TRUE, blank.lines.skip = TRUE)

dt.m <- merge(dt.y, dt.c, all = TRUE)

dt.y <- fread("data/Building Permit Survey/co0001y.txt", header = TRUE,
      blank.lines.skip = TRUE)


dt.c <- fread("data/Building Permit Survey/co0001c.txt", header = TRUE,
      blank.lines.skip = TRUE)


l.files <- paste0("data/Building Permit Survey/", list.files("data/Building Permit Survey"))

dt <- rbindlist(lapply(l.files, fread, header = TRUE))



