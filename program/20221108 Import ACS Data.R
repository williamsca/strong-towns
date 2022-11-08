# Import American Community Survey
# Source: https://api.census.gov/data/2013/acs/acs5/variables.html

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, censusapi)

Sys.setenv(CENSUS_KEY="1cc02abe8c0e40482642650aaf4f230e511c650c") # Add key to .Renviron
readRenviron("~/.Renviron") # Reload .Renviron
Sys.getenv("CENSUS_KEY") # Check to see that the expected key is output in your R console

# Import 'Median year structure built' by block group from ACS 2013 ----

# Create state, county FIPS crosswalk
dt.county_cw <- readRDS(file = "derived/County Area Population (1957-2002).Rds")
dt.county_cw[, County_FIPS := sprintf("%03d", FIPS.Code.County)][, State_FIPS := sprintf("%02d", FIPS.Code.State)]
dt.fips <- unique(dt.county_cw[, .(State_FIPS, County_FIPS)])

# Loop over each state, county and request data from Census API
l.medyear <- list()
for (i in 1:nrow(dt.fips)) { 
  state <- dt.fips[i, State_FIPS]
  county <- dt.fips[i, County_FIPS]
  
  print(paste0(state, county))
  try(df <- getCensus(name = "acs/acs5", vintage = 2013, regionin = paste0("state:", state, "+county:", county), region = "block group:*",
                     vars = c("B25034_001E", "B25035_001E")))
  
  
  l.medyear <- append(l.medyear, list(as.data.table(df)))
  
}

dt <- rbindlist(l.medyear)
setnames(dt, c("B25034_001E", "B25035_001E"), c("nUnits", "medYearBuilt"))

dt.county <- getCensus(name = "acs/acs5", vintage =  2013, region = "county:*", vars = c("B25034_001E", "B25035_001E"))
setnames(dt.county, c("B25034_001E", "B25035_001E"), c("nUnitsCty", "medYearBuiltCty"))

dt <- merge(dt, dt.county, by = c("state", "county"))

saveRDS(dt, "derived/Block Group Housing (2013).Rds")

# Testing ----
# df <- getCensus(name = "acs/acs5", vintage = 2013, regionin = "state:40+county:047", region = "block group:*",
#                      vars = c("B25035_001E"))
# 
# v.regionin <- dt.fips[, paste0("state:", State_FIPS, "+county:", County_FIPS)]
# 
# getBlockData <- function(x) {
#   return(as.data.table(getCensus(name = "acs/acs5", vintage = 2013, regionin = x, region = "block group:*",
#                        vars = c("B25_034_001E", "B25035_001E"))))
# }
# dt <- rbindlist(lapply(v.regionin, getBlockData))
