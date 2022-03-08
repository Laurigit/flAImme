library(adagio)
library(shiny)
library(data.table)
library(stringr)
library(shinydashboard)
library(httr)
library(jsonlite)
library(RMySQL)
library(shinyjs)
library(readtext)
library(clipr)
#library(qdapRegex)
#library(magick)
library(dragulaR)
library(lubridate)
library(readxl)
library(zoo)
library(optiRum)
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(shinyWidgets)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)
library(testthat)
library(Rcpp)
library(ggplot2)
library(doParallel)
library(foreach)
global_cores <- detectCores() - 1
#options(shiny.trace=TRUE)


options(shiny.fullstacktrace = TRUE)
options(traceback.max.lines = 10)
options(deparse.max.lines = 10)
options(digits=4)
sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
sourcelist[, rivi := seq_len(.N)]
sourcelist[, kansio := strsplit(polku, split = "/")[[1]][1], by = rivi]
sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]

input_kansio_list <- c("utility",
                       "c_functions",
                       "solution_functions",
                       "solution",
                       "bots",
                       "UID")
for(input_kansio in input_kansio_list) {

  dir_list <- sourcelist[kansio == input_kansio, polku]

  dir_list<- setdiff(dir_list, "solution/main.R")

  for(filename in dir_list) {
    result = tryCatch({
      print(paste0("sourcing ", filename))
      source(paste0("./scripts/", filename), local = TRUE)
      print(paste0("sourced ", filename))
    }, error = function(e) {
      print(paste0("error in loading file: ", filename))
    })
  }
}
#con <- connDB(con)
#rm(con)
con <- connDB(con, "flaimme")
#rm(con)
dbSendQuery(con, 'SET NAMES utf8')
dbQ("SHOW TABLES", con)
luettu <- dbSelectAll("ADM_OPTIMAL_MOVES", con)

ADM_OPTIMAL_MOVES <- fix_colnames(luettu)
setDTthreads(4)
required_data("ADM_CYCLER_DECK")
required_data("STG_TRACK")
required_data("STG_TRACK_PIECE")
