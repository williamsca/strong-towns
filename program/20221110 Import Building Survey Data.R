# Import Building Permit Survey Data
# Source: https://www2.census.gov/econ/bps/County/

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, curl)

url <- "https://www2.census.gov/econ/bps/County/"
h <- new_handle(dirlistonly=TRUE)
con <- curl(url, "r", h)
read.table(con, stringsAsFactors = TRUE, fill = TRUE)



# Testing
l.files <- paste0("data/Building Permit Survey/", list.files("data/Building Permit Survey"))

dt <- rbindlist(lapply(l.files, fread, header = TRUE))



