# =====================================================================
# STAGE 3 - VERSIONE 1.0
# Questo script rimuove dai dati i codici ICD9 considerati "minor",
# secondo la lista fornita in input (minor_codes.csv).
# - Carica i codici minor da file
# - Carica il dataset pulito dallo stage 2
# - Rimuove i codici minor dalle colonne di interesse
# - Esporta il dataset aggiornato
# =====================================================================

rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)


# LOAD DATA
file_minor_codes <- read_csv2(paste0(tableDir,"/minor_codes.csv"))
minor_codes <- read_csv2(paste0(tableDir, "/minor_codes.csv"))
minor_codes <- minor_codes$ICD9
sdo_stage_2_complete <- read_csv2(paste0(exportDir,"/sdo_stage_2_valide_all_export.csv"))
extra_codes <- read_csv2(paste0(tableDir, "/extra_codes.csv")) 


# Applica la funzione al dataset completo
sdo_stage_3_complete <- clean_minor_codes(sdo_stage_2_complete, icd9SearchCols, minor_codes)

write_csv2(sdo_stage_3_complete, file=paste0(exportDir, "/sdo_stage_3_valide_all_export.csv"))

invalid_codes <- verify_invalid_codes(sdo_stage_3_complete, icd9SearchCols, minor_codes, extra_codes) # CHIARIRE FUNZIONAMENTO

# Stampa i codici invalidi
if(length(invalid_codes) > 0) {
  print("Codici patologici non validi trovati:")
  print(invalid_codes)
} else {
  print("Nessun codice non valido trovato.")
}