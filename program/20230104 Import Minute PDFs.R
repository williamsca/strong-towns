# Import and parse local government data from minute pdfs
# References: 
# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html#read_from_pdf_files
# https://docs.ropensci.org/pdftools/

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, lubridate, pdftools) 

# VA ----
# * Albemarle ----
src <- "data/Minutes/VA/Albemarle/"
l.files <- list.files(src, "*.pdf")

l.paths <- paste0(src, l.files)
dt <- data.table(l.files, lapply(l.paths, pdf_text))

dt[, V2 := gsub("\\s+", " ", V2)]
dt[, V2 := gsub("\\\\n", "", V2)]

dt <- dt[!duplicated(V2)]

s.date <- "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+(\\d{1,2}),\\s+\\d{4}"
dt[, date := as.POSIXct(regmatches(V2, regexpr(s.date, V2)), format = "%B %d, %Y")]

dt[, hasRezone := grepl("ZMA", V2)]
dt[, hasProffer := grepl("Proffer", V2, ignore.case = TRUE)]

dt[, isZMA := regmatches(V2, gregexpr(".{10}ZMA.{10}", V2))]

test <- "Here is Jan and Jan and yet another Jan"
regmatches(test, gregexpr("Jan", test))
