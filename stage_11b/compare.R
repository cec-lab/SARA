# =====================================================================
# STAGE 11B - VERSIONE 1.1
# - Converte i codici ICD9CM in ICD10 (codice e descrizione)
# - Usa solo il metodo clinico (PROG_PAZ) per marcare i soggetti già registrati in REDCap
# =====================================================================
rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)


# LOAD DATA ----

sdo_stage11_cedap_with_labels <- read_csv2(paste0(exportDir, "/sdo_stage11_cedap_with_labels_export.csv"))

redcapData_stage_1_1 <- read_delim(paste0(edcFileDir, "/redcapData_stage_1_1.csv"), 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

icd_conversion_table <- read_excel(paste0(tableDir, "/icd_conversion_table.xlsx"))

cedap_plus_2023 <- read_csv2(paste0(cedapDir,"/",cedapFileName))


# Codifica ICD10: creazione colonne vuote ----

for (col in c(patology_code_cols_icd10, patology_label_cols_icd10)) {
  sdo_stage11_cedap_with_labels[[col]] <- NA
}


# Conversione ICD10: codici ----

for (col in patology_cols) {
  label_col <- paste0(col, "_code_icd10")
  sdo_stage11_cedap_with_labels[[label_col]] <- sapply(sdo_stage11_cedap_with_labels[[col]], get_icd10_code)
}


# Conversione ICD10: descrizioni ----

for (col in patology_cols) {
  label_col <- paste0(col, "_label_icd10")
  sdo_stage11_cedap_with_labels[[label_col]] <- sapply(sdo_stage11_cedap_with_labels[[col]], get_icd10_description)
}


# Metodo PROG_PAZ per identificare record già presenti in REDCap ----

# Step 1: Indici delle righe CedAP collegate a REDCap
linked_indices <- redcapData_stage_1_1$cedap_linked

# Step 2: Filtra indici validi
linked_indices <- linked_indices[linked_indices != 0]

# Step 3: Estrai i PROG_PAZ corrispondenti da CedAP
redcap_prog_paz <- unique(na.omit(cedap_plus_2023$prog_paz_neo[linked_indices]))

# Step 4: Confronta con PROG_PAZ nello SDO
sdo_prog_paz <- sdo_stage11_cedap_with_labels$PROG_PAZ
alreadyRecorded_PROG_PAZ <- ifelse(sdo_prog_paz %in% redcap_prog_paz, 1, 0)

# Step 5: Aggiungi colonna al dataset
sdo_stage11_cedap_with_labels$alreadyRecorded_PROG_PAZ <- alreadyRecorded_PROG_PAZ

# (opzionale) riepilogo
table(alreadyRecorded_PROG_PAZ)


# Esporta dataset finale ----

write_csv2(sdo_stage11_cedap_with_labels, file = paste0(exportDir, "/sdo_stage_11b_clinical_rev_export.csv"), na = "")
