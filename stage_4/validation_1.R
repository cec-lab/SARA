# =====================================================================
# STAGE 4 - VERSIONE 1.0
# Questo script applica le regole di validazione chirurgica alle SDO:
# - Carica il dataset pulito privo di codici minor (da stage 3)
# - Aggiunge le variabili "validated" e "validation_type"
# - Carica le regole di validazione da file (surgery_rules.csv)
# - Applica le regole con la funzione `update_validation_optimized`
# - Esporta il dataset con l'esito della validazione
# =====================================================================
rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)

#LOAD DATA----
sdo_stage_3_nominors_export <-read_csv2(paste0(exportDir,"/sdo_stage_3_valide_all_export.csv"))
surgery_rules <- read_csv2(paste0(tableDir, "/surgery_rules.csv"))

#Creare le variabili "validated" e "validation_type" impostate a 0
sdo_stage_3_nominors_export$validated <- 0
sdo_stage_3_nominors_export$validation_type <- 0


# Convertire tutti i codici in character per evitare problemi di confronto
surgery_rules <- surgery_rules %>%
  mutate(Icd = as.character(Icd),
         CodInterv = as.character(CodInterv))

sdo_stage_4_nominors_export <- update_validation_optimized(sdo_stage_3_nominors_export, surgery_rules)
table(sdo_stage_4_nominors_export$validated)


write_csv2(sdo_stage_4_nominors_export, file=paste0(exportDir, "/sdo_stage_4_validated_1_export.csv"))


