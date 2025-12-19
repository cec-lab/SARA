# =====================================================================
# STAGE 5 - VERSIONE 1.0
# - Carica il dataset validato dallo stage 4 e il file valid_default
# - Applica un'ulteriore validazione controllando se almeno un codice
#   ICD9 delle SDO è presente in valid_default
# - Aggiorna le colonne `validated` e `validation_type` per le righe che soddisfano la condizione
# - Esporta il dataset aggiornato
# =====================================================================
rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)


# LOAD DATA ----
sdo_stage_4_validated_file <- paste0(exportDir, "/sdo_stage_4_validated_1_export.csv")
valid_default<- read_csv2(paste0(tableDir, "/valid_default.csv"))

# CARICAMENTO DATI ----
sdo_stage_4_validated <- fread(sdo_stage_4_validated_file, colClasses = c(validation_type = "character"))

icd9_column_valid <- names(valid_default)[1]  # 1 <- icd9

# CHIAMATA DELLA FUNZIONE DI VALIDAZIONE ----
sdo_stage_5_validated <- validate_sdo(
  sdo_stage_4_validated, 
  valid_default, 
  icd9SearchCols, 
  icd9_column_valid
)

table(sdo_stage_5_validated$validation_type
      )

# ESPORTAZIONE DEL DATASET AGGIORNATO ----
write_csv2(sdo_stage_5_validated, paste0(exportDir, "/sdo_stage_5_validated_2_export.csv"))
