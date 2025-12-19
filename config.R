# PACKAGES ----

library(tidyverse)
library(readr)
library(data.table)
library(dplyr)
library(readxl)
library(openxlsx)
library(tableone)
library(naniar)
library(VIM)
library(ggplot2)
library(tableone)
library(labelled)


# PATH ----
baseDir="/home/imer/works/algo_sdo"
sdoDir=paste0(baseDir,"/sdo")
stage_0Dir=paste0(baseDir, "/stage_0")
stage_1Dir=paste0(baseDir, "/stage_1")
stage_2Dir=paste0(baseDir, "/stage_2")
stage_3Dir=paste0(baseDir, "/stage_3")
stage_4Dir=paste0(baseDir, "/stage_4")
stage_5Dir=paste0(baseDir, "/stage_5")
stage_6Dir=paste0(baseDir, "/stage_6")
stage_7Dir=paste0(baseDir, "/stage_7")
stage_8Dir=paste0(baseDir, "/stage_8")
stage_9Dir=paste0(baseDir, "/stage_9")
stage_10Dir=paste0(baseDir, "/stage_10")
stage_11Dir=paste0(baseDir, "/stage_11")
stage_mapping=paste0(baseDir, "/mapping")
stage_12Dir=paste0(baseDir, "/stage_12")


cedapDir="/home/imer/works/DARIO/cedap"
cedapFileName="cedap_plus_2023.csv"
sdoFileName_stage_0="sdo_2023_all.csv"
sdoFileName_stage_1="sdo_1yfup_2023.csv"
exportDir=paste0(baseDir,"/export")
tableDir=paste0(baseDir,"/tables")
cedap_plus_2023_file <- file.path(cedapDir, cedapFileName)
edcFileDir <- "/home/imer/works/DARIO/export"


# FILE HANDLER ----

con = file(paste0(exportDir, "/job.log"))

# GLOBAL ----

yearOfBirth = 2023



