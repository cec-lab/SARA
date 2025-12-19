# Clear existing data and graphics
rm(list=ls())
graphics.off()

# SOURCE CONFIGURATION FILE ----

source("/home/imer/works/algo_sdo/config.R", echo = T)

# SOURCE CUSTOM FUNCTIONS FILE ----

source("/home/imer/works/algo_sdo/functions.R", echo = T)

# LOG FILE OPEN ----

sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

# SET WORKING DIRECTORY ----

setwd(baseDir)

# LOAD DATA FILE ----
print("READING SDO FILE..")
sdo <- read_csv2(paste0(sdoDir, "/sdo_1yfup_2023.csv"))
print("SDO FILE LOADED")

# STAGE 1 ----

print("STAGE 1 START")
source(paste0(stage_1Dir, "/scan.R"), echo = T)
print("STAGE 1 END")

# STAGE 2 ----

print("STAGE 2 START")
source(paste0(stage_2Dir, "/scan_extra.R"), echo = T)
print("STAGE 2 END")

#STAGE 3 ----

print("STAGE 3 START")
source(paste0(stage_3Dir, "/minors_removal.R"), echo = T)
print("STAGE 3 END")

#Stage 4 ----

print("STAGE 4 START")
source(paste0(stage_4Dir, "/validation_1.R"), echo = T)
print("STAGE 4 END")

#Stage5----

print("STAGE 5 START")
source(paste0(stage_5Dir, "/validation_2.R"), echo = T)
print("STAGE 5 END")

#Stage6----

print("STAGE 6 START")
source(paste0(stage_6Dir, "/validation_3.R"), echo = T)
print("STAGE 6 END")

#Stage7----

print("STAGE 7 START")
source(paste0(stage_7Dir, "/validation_4.R"), echo = T)
print("STAGE 7 END")

#Stage8----

print("STAGE 8 START")
source(paste0(stage_8Dir, "/filters3.R"), echo = T)
print("FILTR OK")
print("SCORE")
source(paste0(stage_8Dir, "/score.R"), echo = T)
print("SCORE OK")
print("STAGE 8 END")

#Stage9----

print("STAGE 9 START")
source(paste0(stage_9Dir, "/collapse.R"), echo = T)
print("STAGE 9 END")

#Stage10 ----

print("STAGE 10 START")
source(paste0(stage_10Dir, "/linkage.R"), echo = T)
print("STAGE 10 END")


#Stage11 ----

print("STAGE 11 START")
source(paste0(stage_11Dir, "/label.R"), echo = T)
print("STAGE 11 END")


#Mapping x transcode.R ----
# 
# print("MAPPING START")
# source(paste0(stage_mapping, "/tracciato2.R"), echo = T)
# print("MAPPING END")


#Stage12 ----

print("STAGE 12 START")
source(paste0(stage_12Dir, "/transcode.R"), echo = T)
print("STAGE 12 END")





# LOG FILE CLOSE ----

sink()
sink(type="message")







