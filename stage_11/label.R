# =====================================================================
# STAGE 11 - VERSIONE 1.0
# 
# - Carica il dataset combinato SDO-CedAP prodotto allo stage 10
# - Mappa i codici di patologia (ICD-9-CM) e intervento chirurgico con 
#   le relative descrizioni testuali, utilizzando tabelle di conversione esterne
# - Gestisce codici multipli concatenati con "|" e applica padding ai codici 
#   patologia per uniformarne la lunghezza a 6 caratteri
# - Crea nuove colonne con le etichette descrittive corrispondenti ai codici
# - Esporta:
#     * il dataset completo con le nuove colonne label
#     * due file campione con i soli codici e le rispettive etichette per patologie e interventi
# =====================================================================
rm(list=ls())

# LOAD CONFIG ---
baseDir=getwd()
source(paste0(baseDir,"/config.R"), echo = T)
source(paste0(baseDir,"/functions.R"), echo = T)

# LOAD DATA ----

icd_conversion_table <- read_excel(paste0(tableDir,"/icd_conversion_table.xlsx"))
surgery <- read_excel(paste0(tableDir, "/surgery.xlsx"))
sdo_stage10_cedap_combined <- read_csv2(paste0(exportDir, "/sdo_stage10_cedap_combined_export.csv"))
extra_codes <- read_csv2(paste0(tableDir,"/extra_codes.csv"))



# Crea colonne label vuote ----

for (col in c(patology_cols, intervention_cols)) {
  sdo_stage10_cedap_combined[[paste0(col, "_label")]] <- NA
}

surgery_map <- surgery %>%
  transmute(codice = as.character(Codice), descrizione = DescrIntervento)



# Applica alle colonne di patologia ----

extra_codes <- extra_codes %>%
  mutate(ICD9 = str_pad(as.character(ICD9), 6, "right", "0"))

sdo_stage10_cedap_combined <- sdo_stage10_cedap_combined %>%
  mutate(across(all_of(patology_cols),
                ~ str_pad(as.character(.), width = 6, side = "right", pad = "0")))

for (col in patology_cols) {
  label_col <- paste0(col, "_label")
  sdo_stage10_cedap_combined[[label_col]] <- sapply(
    sdo_stage10_cedap_combined[[col]],
    map_multi_codes_pat,
    dict = icd_conversion_table,
    extra_codes = extra_codes
  )
}


# Applica alle colonne di intervento ----

for (col in intervention_cols) {
  label_col <- paste0(col, "_label")
  sdo_stage10_cedap_combined[[label_col]] <- sapply(sdo_stage10_cedap_combined[[col]], map_multi_codes_interv, dict = surgery_map)
}


# Estrazione delle colonne per le tabelle ----

patology_label_cols <- sdo_stage10_cedap_combined %>%
  select(PROG_PAZ, all_of(patology_cols), all_of(patology_label_cols_icd9))

intervention_labels_only <- sdo_stage10_cedap_combined %>%
  select(PROG_PAZ, all_of(intervention_cols), all_of(intervention_label_cols))


# OUT ----
write_csv2(sdo_stage10_cedap_combined, paste0(exportDir, "/sdo_stage11_cedap_with_labels_export.csv"))
