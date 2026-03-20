# =====================================================================
# STAGE 2 - VERSIONE 1.0
# Questo script cerca di recuperare le righe escluse nello stage 1
# (sdo_invalid) identificando la presenza di codici ICD9 extra.
# - Rilegge i dataset da stage 1
# - Filtra le righe non valide che contengono codici extra(sdo_extra_valid)
# - Uniforma e unisce queste righe con il dataset valido dello stage 1
# - Pulisce le colonne ICD9 lasciando solo: codici nel range 740–759 e codici presenti in extra_codes
# - Esporta i dataset aggiornati
# =====================================================================

rm(list = ls())

# LOAD CONFIG ---
baseDir <- getwd()
source(paste0(baseDir, "/config.R"), echo = TRUE)
source(paste0(baseDir, "/functions.R"), echo = TRUE)

library(data.table)
library(readr)

# LOAD DATI ---
extra_codes <- read_csv2(paste0(tableDir, "/extra_codes.csv")) 
sdo_valid_stage1 <- read_csv2(paste0(exportDir, "/sdo_stage_1_valide_export.csv"))
sdo_invalid <- read_csv2(paste0(exportDir, "/sdo_stage_1_nonvalide_export.csv"))

# Filtra righe con codici extra
sdo_extra_valid <- filter_rows_extra(sdo_invalid,columns_icd9_extra,extra_codes)

# Uniforma colonne
sdo_extra_valid <- sdo_extra_valid[, colnames(sdo_valid_stage1), with = FALSE]

# Unione
sdo_stage_2_complete <- rbindlist(list(sdo_valid_stage1, sdo_extra_valid), fill = TRUE)

# Assicura data.table
setDT(sdo_stage_2_complete)

# Pulizia codici non validi
sdo_stage_2_complete <- clean_invalid_patologies(
  sdo_stage_2_complete, icd9SearchCols, extra_codes
)

setDT(sdo_stage_2_complete)  # sicurezza

# Shift a sinistra (dopo pulizia)
sdo_stage_2_complete <- shift_icd9_left(
  sdo_stage_2_complete, icd9SearchCols
)

# EXPORT ---
write_csv2(sdo_stage_2_complete, paste0(exportDir, "/sdo_stage_2_valide_all_export.csv"))
write_csv2(sdo_extra_valid, paste0(exportDir, "/sdo_stage_2_valide_extra_codes_export.csv"))


