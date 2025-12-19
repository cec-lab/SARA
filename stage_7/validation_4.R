# =====================================================================
# STAGE 7 - VERSIONE 1.0
# - Carica il dataset validato dallo stage 6 e i codici ICD9 delle
#   malformazioni minori (minor_codes)
# - Rimuove dai campi ICD9 i codici "V3000" e le malformazioni minori
# - Identifica le sottomatrici dello stesso paziente (stesso PROG_PAZ)
# - Se tra le righe della sottomatrice esistono patologie uguali (escluse
#   le minori), aggiorna il campo validation_type a "4" o aggiunge "|4"
# - Esporta il dataset aggiornato
# =====================================================================
rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)

# LOAD DATA ----
input_file <- paste0(exportDir, "/sdo_stage_6_validated_3_export.csv")
minor_codes <- read_csv2(paste0(tableDir, "/minor_codes.csv"))
sdo_stage_6 <- read_csv2(paste0(exportDir, "/sdo_stage_6_validated_3_export.csv"))

# CONVERTIRE PROG_PAZ IN NUMERICO ----
sdo_stage_6$PROG_PAZ <- as.numeric(sdo_stage_6$PROG_PAZ)

# APPLICARE LA VALIDAZIONE ----
sdo_stage_7 <- validate_duplicates(sdo_stage_6, minor_codes, icd9SearchCols)
table(sdo_stage_7$validation_type)


# SALVARE IL FILE RISULTANTE ----
write_csv2(sdo_stage_7, file = paste0(exportDir, "/sdo_stage_7_validated_4_export.csv"))



