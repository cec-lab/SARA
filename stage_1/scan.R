# =====================================================================
# STAGE 1 - VERSIONE 1.0
# Questo script filtra il dataset SDO mantenendo solo le righe che
# contengono codici ICD9 compresi nell’intervallo 740–759 (patologie).
# - Applica la funzione di filtro rowFilterByCodes
# - Divide in righe valide e non valide
# - Esporta entrambi i dataset per lo stage successivo
# =====================================================================

rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)


# LOAD DATA ---
sdo <- read_csv2(paste0(sdoDir, "/", sdoFileName_stage_1))

# Stage 1 - Filtering dataset by IDC codes range 740:759 (pathology) ----

# Chiamata alla funzione rowFilterByCodes senza malfoCodes
sdo_filtered <- rowFilterByCodes(sdo, icd9SearchCols)
# Righe valide
sdo_valid <- sdo_filtered[[1]]

# Righe invalide
sdo_invalid <- sdo_filtered[[2]]

write_csv2(sdo_valid, file=paste0(exportDir, "/sdo_stage_1_valide_export.csv"))

write_csv2(sdo_invalid, file=paste0(exportDir, "/sdo_stage_1_nonvalide_export.csv"))


