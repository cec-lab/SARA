# =====================================================================
# STAGE 9 - VERSIONE 1.0
# - Carica il dataset con score validati dallo stage 8
# - Identifica i pazienti (PROG_PAZ) presenti più volte
# - Per ciascun paziente ripetuto:
#     * estrae tutte le malformazioni da COD_PAT1 a patol6
#     * rimuove i valori NA e aggrega tutti i codici univoci
#     * genera un record "collassato" in cui:
#         - le variabili vengono concatenate per colonna (con "|")
#         - i codici malformativi univoci vengono assegnati ordinatamente a COD_PAT1 e patol*
# - I record univoci vengono mantenuti senza modifiche
# - Esporta:
#     * il dataset collassato (sdo_stage_9_validated_collapsed_export.csv)
#     * i record univoci (sdo_stage_9_validated_unique_export.csv)
#     * tutti i record ripetuti prima del collasso (sdo_stage_9_validated_dup_review_export.csv)
# =====================================================================
rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)

# LOAD DATA ----

sdo_stage8 <- read_csv2(paste0(exportDir, "/sdo_stage_8_validated_score_export.csv"))

sdo_stage9_collapsed <- data.frame()

# global vars ----

patol<-c("patol2", "patol3", "patol4", "patol5", "patol6")
interv<-c("interv1", "interv2", "interv3", "interv4", "interv5", "interv6", "interv7", "interv8", "interv9", "interv10", "interv11")

# calcolo prog_paz ripetuti ----

dupProgPaz <- sdo_stage8 |> group_by(PROG_PAZ) |> summarise(n=n()) |> filter(n>1)

# generazione matrice per prog_paz ----

for(i in 1: nrow(dupProgPaz)){
  
  tmpSDO <- sdo_stage8 |> filter(PROG_PAZ==dupProgPaz[[i,1]])
  
  tmpPat <- tmpSDO |> select(COD_PAT1, patol2, patol3, patol4, patol5, patol6)
  
  # estrazione vettore malfo ----
  
  vPat <- unique(as.vector(as.matrix(tmpPat)))
  
  vPat <- vPat[!is.na(vPat)]
  
  # generazione caso malformato ----
  
  # Separiamo cedap_linked per salvarlo prima del collasso
  #linked_value <- tmpSDO$cedap_linked[1]  
  
  # Rimuoviamo cedap_linked prima del collasso
  #tmpSDO_no_linked <- tmpSDO |> select(-cedap_linked)
  
  # Genera il record collassato senza modificare sds_cc
  malformedCase <- as.data.frame(t(apply(tmpSDO |> select(-sds_cc), 2, paste, collapse = "|")))
  
  # Ripristina sds_cc originale (numerico, senza concatenare)
  malformedCase$sds_cc <- tmpSDO$sds_cc[1]
  
  
  # Ripristina cedap_linked originale
  #malformedCase$cedap_linked <- linked_value
  
  malformedCase[, c(patol)] <- NA
  
  malformedCase[, "COD_PAT1"] <- vPat[1]
  
  if(length(vPat)>1){
    
    malformedCase[, c(30:(30+length(vPat)-2))] <- vPat[2:length(vPat)]
    
  }
  
  sdo_stage9_collapsed <- rbind(sdo_stage9_collapsed, malformedCase)
}



# OUT ----

sdo_stage9_unique <- sdo_stage8 |> filter(!(PROG_PAZ %in% dupProgPaz$PROG_PAZ)) |> write_csv2(file=paste0(exportDir, "/sdo_stage_9_validated_unique_export.csv"))
write_csv2(sdo_stage9_collapsed, file=paste0(exportDir, "/sdo_stage_9_validated_collapsed_export.csv"))
sdo_stage9_dup_review <- sdo_stage8 |> filter(PROG_PAZ %in% dupProgPaz$PROG_PAZ) |> write_csv2(file=paste0(exportDir, "/sdo_stage_9_validated_dup_review_export.csv"))


