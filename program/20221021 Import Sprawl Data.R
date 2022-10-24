# Import "A Century of Sprawl" Shaprefiles
# Source: https://datadryad.org/stash/dataset/doi:10.5061/dryad.3k502

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, sf)

dt <- read_sf("data/doi_10.5061_dryad.3k502__v1/shapefiles/Barrington-Leigh-Millard-Ball-PNAS2015-Century-of-sprawl-county.dbf")

