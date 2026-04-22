# =====================================================================
# STAGE 11B - VERSIONE 1.1
# - Converte i codici ICD9CM in ICD10 (codice e descrizione)
# - Usa solo il metodo clinico (PROG_PAZ) per marcare i soggetti già registrati in REDCap
# =====================================================================
rm(list=ls())

# LOAD CONFIG ---
baseDir=getwd()
source(paste0(baseDir,"/config.R"), echo = T)
source(paste0(baseDir,"/functions.R"), echo = T)


# LOAD DATA ----

sdo_stage11_cedap_with_labels <- read_csv2(paste0(exportDir, "/sdo_stage11_cedap_with_labels_export.csv"))

redcapData_stage_1_1 <- read_csv2(paste0(edcFileDir, "/redcapData_stage_1_1.csv"))
                                   

icd_conversion_table <- read_excel(paste0(tableDir, "/icd_conversion_table.xlsx"))

cedap_plus_2023 <- read_csv2(paste0(cedapDir,"/",cedapFileName))


# Codifica ICD10: creazione colonne vuote ----

for (col in c(patology_code_cols_icd10, patology_label_cols_icd10)) {
  sdo_stage11_cedap_with_labels[[col]] <- NA
}


# ICD10 codici
for (col in patology_cols) {
  label_col <- paste0(col, "_code_icd10")
  sdo_stage11_cedap_with_labels[[label_col]] <- sapply(
    sdo_stage11_cedap_with_labels[[col]],
    get_icd10_code,
    dict = icd_conversion_table
  )
}

# ICD10 descrizioni
for (col in patology_cols) {
  label_col <- paste0(col, "_label_icd10")
  sdo_stage11_cedap_with_labels[[label_col]] <- sapply(
    sdo_stage11_cedap_with_labels[[col]],
    get_icd10_description,
    dict = icd_conversion_table
  )
}

# # Metodo PROG_PAZ per identificare record già presenti in REDCap ----

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

#aggiunta colonne revcode e note ----

sdo_stage11_cedap_with_labels$revcode <- 9
sdo_stage11_cedap_with_labels$note <- ""



write_csv2(sdo_stage11_cedap_with_labels, file = paste0(exportDir, "/sdo_stage_11b_clinical_rev_export.csv"))

# sdo_stage_11b_clinical_rev_export_final <- read_delim("export/sdo_stage_11b_clinical_rev_export_final.csv", 
#                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
# # #test per revcode causale
# sdo_stage_11b_clinical_rev_export_final$revcode <- sample(
#   c(0,1,2),
#   nrow(sdo_stage_11b_clinical_rev_export_final),        #da commentare
#   replace = TRUE,
#   prob = c(0.7,0.2,0.1)
# )
# 
# 
# write_csv2(sdo_stage_11b_clinical_rev_export_final, file = paste0(exportDir, "/sdo_stage_11b_clinical_rev_export_final.csv"))
# 
