# =====================================================================
# STAGE 10 - VERSIONE 1.0
# - Carica i dataset dallo stage 9 (collassato + unici) e il CedAP
# - Effettua pulizia sulle colonne con valori concatenati ("|")
# - Estrae e converte le date (ISO e DD/MM/YYYY)
# - Rimuove il formato collassato mantenendo il primo valore utile
# - Ricombina collassati e unici
# - Esegue linkage con CedAP per ottenere variabili aggiuntive mancanti
# - Esporta il dataset finale combinato
# =====================================================================

rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)


# LOAD DATA ----

sdo_stage9_collapsed <- read_csv2(paste0(exportDir, "/sdo_stage_9_validated_collapsed_export.csv"))
sdo_stage9_unique <- read_csv2(paste0(exportDir, "/sdo_stage_9_validated_unique_export.csv"))
cedap_plus_2023 <- read_csv2(paste0(cedapDir, "/", cedapFileName))
data_structure <- read_csv2(paste0(tableDir, "/sdo_cedap_dd.csv"))


# Clean collapsed dataset ----

# Sostituzione delle virgole con il punto
sdo_stage9_collapsed$sds_cc <- gsub(",", ".", sdo_stage9_collapsed$sds_cc)

colsToClean <- data_structure |> filter(Origine == "SDO" & clean == 1) |> pull(Tracciato)
colsDataType <- data_structure |> filter(Origine == "SDO" & clean == 1) |> pull(data_type)

k = 1
for (col in colsToClean) {
  if (col %in% colonne_date) {
    sdo_stage9_collapsed[[col]] <- estrai_data_valida(sdo_stage9_collapsed[[col]])
  } else {
    cond = paste0("as.", colsDataType[k], "(sdo_stage9_collapsed[['", col, "']])")
    sdo_stage9_collapsed[[col]] <- unlist(lapply(strsplit(sdo_stage9_collapsed[[col]], "\\|"), `[[`, 1))
    sdo_stage9_collapsed[[col]] <- eval(parse(text = cond))
  }
  k = k + 1
}


# Conversione coerente per tutte le colonne data ----

for (col in colonne_date) {
  if (col %in% names(sdo_stage9_collapsed)) {
    sdo_stage9_collapsed[[col]] <- estrai_data_valida(as.character(sdo_stage9_collapsed[[col]]))
  }
  if (col %in% names(sdo_stage9_unique)) {
    sdo_stage9_unique[[col]] <- estrai_data_valida(as.character(sdo_stage9_unique[[col]]))
  }
}


# Unione dataset collassati e unici ----

sdo_stage9_combined <- rbind(sdo_stage9_collapsed, sdo_stage9_unique)


# Linkage con CedAP ----

cedap_da_unire <- data_structure |> filter(Origine == "CedAP") |> pull(Tracciato)
cedap_plus_2023 <- cedap_plus_2023 |> select(all_of(cedap_da_unire))

cedap_plus_2023 <- cedap_plus_2023 %>%
  mutate(row_num = row_number())

sdo_stage10_cedap_combined <- merge(
  sdo_stage9_combined,
  cedap_plus_2023,
  by.x = "cedap_linked",
  by.y = "row_num",
  all.x = TRUE
)



# Selezione delle righe con sds_cc <= -2.8 
righe_sotto_cutoff <- sdo_stage10_cedap_combined[sdo_stage10_cedap_combined$sds_cc <= -2.8, ]
colonne_da_mostrare <- c("PROG_PAZ", icd9SearchCols, intervention_cols, "sds_cc")
righe_filtrate_sds_da_escludere <- righe_sotto_cutoff[, colonne_da_mostrare, drop = FALSE]
print(righe_filtrate_sds_da_escludere)



# OUT ----

sdo_stage10_cedap_combined$dt_nas_m <- as.Date(sdo_stage10_cedap_combined$dt_nas_m, format = "%d/%m/%Y")
sdo_stage10_cedap_combined$dt_nas_m <- format(sdo_stage10_cedap_combined$dt_nas_m, "%Y-%m-%d")
write_csv2(sdo_stage10_cedap_combined, paste0(exportDir, "/sdo_stage10_cedap_combined_export.csv"))

