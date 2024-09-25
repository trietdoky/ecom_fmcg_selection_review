# CLEAN EVIRONMENT ===============

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage.

# INITIALIZATION =================

pacman::p_load(pacman,
               DT,
               tidyverse,
               tidyselect,
               rmarkdown,
               tibble,
               dplyr,
               purrr,
               zoo,
               lubridate,
               plotly,
               rio,
               stringr,
               tidyr,
               janitor,
               highcharter,
               tibble,
               bigrquery,
               googlesheets4,
               googledrive,
               readxl,
               openxlsx,
               DBI,
               gargle,
               telegram.bot,
               bigrquery,
               RDataOps,
               scales,udpipe,
               textrank,
               ggraph,
               wordcloud)

# IMPORT FUNCTIONS ============

files_source <- file.path("FUNCTIONS",
                          list.files("FUNCTIONS"))

r_files <- files_source %>%
  map_chr(~ str_extract(., "^.+\\.R"))

r_files <- r_files[!is.na(r_files)]

walk(r_files, source)

rm(files_source)
rm(r_files)


# IMPORT QUERY

files_source <- file.path("QUERY",
                          list.files("QUERY"))

r_files <- files_source %>%
  map_chr(~ str_extract(., "^.+\\.R"))

r_files <- r_files[!is.na(r_files)]

walk(r_files, source)

rm(files_source)
rm(r_files)

# CONSTANT
ActiveSouthWarehouse <- c("MFD2", "MFD7", "MFTBI", "MFBTA", "MFD10", "MFD4", "MFD6", "MFTHD")
ActiveNorthWarehouse <- c("HN5", "MFHDO", "MFTHO", "MFLBI")
ActiveCentralWarehouse <- c("DN")
Regions <- c("SOUTH", "NORTH")
DriveFolderId <- as_id("10njNXVaWZ6ekQtuteaIlLEWL4oc8slWS")
Today <- Sys.Date() - days(1)
WACC <- 4.4/100


# VIETNAMESE DICTIONARY

#ud_model <- udpipe_download_model(language = "vietnamese-vtb")
#ud_model <- udpipe_load_model(ud_model$file_model)
ud_model <- udpipe_load_model(paste0(getwd(), '/vietnamese-vtb-ud-2.5-191206.udpipe'))
keys <- list('name','y','drilldown', 'group')
