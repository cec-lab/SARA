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

rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)


extra_codes <- read_csv2(paste0(tableDir, "/extra_codes.csv")) 
sdo_valid_stage1 <- read_csv2(paste0(exportDir, "/sdo_stage_1_valide_export.csv"))
sdo_invalid <-  read_csv2(paste0(exportDir, "/sdo_stage_1_nonvalide_export.csv"))

# Filtra le righe con almeno un codice extra valido
sdo_extra_valid <- filter_rows_extra(sdo_invalid, columns_icd9_extra)

# Convertire esplicitamente tutte le colonne dei codici in caratteri per sicurezza
sdo_valid_stage1[] <- lapply(sdo_valid_stage1, as.character)
sdo_extra_valid[] <- lapply(sdo_extra_valid, as.character)

# Uniformare le colonne per l'unione
sdo_extra_valid <- sdo_extra_valid[, colnames(sdo_valid_stage1), drop = FALSE]

# Unione dei dataset
sdo_stage_2_complete <- bind_rows(sdo_valid_stage1, sdo_extra_valid)

# Applica la funzione di pulizia delle patologie non valide
setDT(sdo_stage_2_complete)  # Converte in data.table se necessario
sdo_stage_2_complete <- clean_invalid_patologies(sdo_stage_2_complete, icd9SearchCols, extra_codes)

# Salva i dataset risultanti
write_csv2(sdo_stage_2_complete, file=paste0(exportDir, "/sdo_stage_2_valide_all_export.csv"))
write_csv2(sdo_extra_valid, file=paste0(exportDir, "/sdo_stage_2_valide_extra_codes_export.csv"))