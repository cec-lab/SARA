# =====================================================================
# STAGE 6 - VERSIONE 1.0
# - Carica i dati validati dallo stage 5 e i dati CEDAP
# - Per ogni riga delle SDO, verifica se almeno un codice ICD9 è
#   presente tra i codici ICD9 associati al CEDAP (usando l’indice cedap_linked)
# - Se esiste una corrispondenza, imposta la riga come validata
#   e aggiorna il campo `validation_type` con "3" o aggiungendo "|3"
# - Esporta il dataset aggiornato
# =====================================================================
rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)

# LOAD DATA ----
sdo_stage_5_validated_file <- paste0(exportDir, "/sdo_stage_5_validated_2_export.csv")
sdo_stage_5_validated <- read_csv2(sdo_stage_5_validated_file, locale = locale(encoding = "UTF-8"), col_types = cols(validation_type = col_character()))
cedap_plus_2023 <- read_csv2(cedap_plus_2023_file, locale = locale(encoding = "UTF-8"))
setDT(sdo_stage_5_validated)
setDT(cedap_plus_2023)

# Applicazione della funzione di validazione
sdo_stage_6_validated <- validate_icd9_with_cedap(
  sdo_stage_5_validated, 
  cedap_plus_2023, 
  icd9_sdo_cols, 
  icd9_cedap_cols, 
  "cedap_linked"
)


# Stampa il conteggio delle validazioni per validation_type
table(sdo_stage_6_validated$validation_type)

# SALVATAGGIO OUTPUT ----
write_csv2(sdo_stage_6_validated, file = paste0(exportDir, "/sdo_stage_6_validated_3_export.csv"))
