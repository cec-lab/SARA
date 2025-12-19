# =====================================================================
# STAGE 8 - VERSIONE 1.0
# - Carica il dataset validato dallo stage 7, il CEDAP e la tabella sulle misure della circonferenza cranica
# - Calcola l'SDS della circonferenza cranica in base a età gestazionale,
#   sesso e primogenitura, utilizzando la tabella INeS
# - Identifica i casi di malformazione isolata vs multipla
# - Applica i filtri di esclusione contenuti nel file 'filter.csv' sulla base di:
#     * età gestazionale
#     * giorni dal parto (GG_DEG)
#     * tipo di malformazione (isolata)
#     * SDS della circonferenza cranica
# - Segna come 'violazione_filtro' le righe che soddisfano i criteri
# - Esporta:
#     * i casi esclusi dai filtri (filter_out.csv)
#     * i casi trattenuti (sdo_stage_8_validated_filters_export.csv)
# - Calcola uno 'score' di validazione aggregato da validation_type
# - Seleziona i casi validati e li divide in top-ranked (score ≥ 4)
#   e low-ranked (score < 4)
# - Esporta i dataset finali con score e la tabella incrociata
# =====================================================================
rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)

SDS_circonferenza_cranica_cutoff = -2.8

# Caricamento dei dati ----

sdo_stage7 <- read_csv2(paste0(exportDir, "/sdo_stage_7_validated_4_export.csv"), locale = locale(encoding = "UTF-8"))
cedap_plus_2023 <- read_csv2(paste0(cedapDir, "/cedap_plus_2023.csv"),locale = locale(encoding = "UTF-8"))
INeS_circonferenza_cranica <- read_csv2(paste0(tableDir, "/INeS_circonferenza_cranica.csv"), locale = locale(encoding = "UTF-8"))
filter <- read_csv2(paste0(tableDir, "/filter.csv"), locale = locale(encoding = "UTF-8"))

setDT(sdo_stage7)
setDT(cedap_plus_2023)
setDT(filter)
setDT(INeS_circonferenza_cranica)

# Converti la colonna 'sds' in integer
filter <- filter %>%
  mutate(sds = as.integer(sds))

# Aggiunta colonna 'row_num' in cedap_plus_2023 ----
cedap_plus_2023 <- cedap_plus_2023 %>%
  mutate(row_num = row_number())

# Unione dei dati ----
sdo_stage8 <- merge(sdo_stage7, 
                    cedap_plus_2023[, .(row_num, eta_gestazionale, CIRCONFERENZA_CRANICA, CONCEPIMENTI_PRECEDENTI)], 
                    by.x = "cedap_linked", by.y = "row_num", all.x = TRUE)

# Calcolo SDS per la circonferenza cranica ----

# Conversione di CONCEPIMENTI_PRECEDENTI in "SI"/"NO"
sdo_stage8$CONCEPIMENTI_PRECEDENTI <- ifelse(sdo_stage8$CONCEPIMENTI_PRECEDENTI == 1, "SI", "NO")

# Pre-allocazione vettore numerico
SDS_circonferenza <- vector("numeric", length = nrow(sdo_stage8))

# Ciclo per calcolare SDS per ogni riga
for (i in seq_len(nrow(sdo_stage8))) {
  SDS_circonferenza[i] <- SDS(
    y = sdo_stage8$CIRCONFERENZA_CRANICA[i],
    pEG = sdo_stage8$eta_gestazionale[i],
    pSesso = sdo_stage8$SEX[i],
    pPrimogenito = sdo_stage8$CONCEPIMENTI_PRECEDENTI[i],
    ines_table = INeS_circonferenza_cranica
  )
}


# Inserimento della nuova colonna nel dataset
sdo_stage8$sds_cc <- SDS_circonferenza

# Continua con il resto dello script...


# Creazione della colonna 'violazione_filtro' ----

sdo_stage8[, violazione_filtro := FALSE]


# Creazione colonna 'malformazione_tipo' per identificare se malformazione è isolata o multipla ----

sdo_stage8 <- sdo_stage8 %>%
  mutate(
    malformazione_tipo = case_when(
      rowSums(!is.na(cbind(COD_PAT1, patol2, patol3, patol4, patol5, patol6))) == 1 ~ "isolata",  
      rowSums(!is.na(cbind(COD_PAT1, patol2, patol3, patol4, patol5, patol6))) > 1 ~ "multipla"
    )
  )


# Applicazione dei filtri ----

conds<-c("eta_gestazionale < eg",
         "GG_DEG < ricgg",
         "malformazione_tipo == 'isolata'",
         "sds_cc < SDS_circonferenza_cranica_cutoff")


for(i in 1:dim(filter)[1]){

  icd = filter[i, ICD9]
  eg = filter[i, eg]
  ricgg = filter[i, ricgg]
  isolata = filter[i, isolata]
  sds = filter[i, sds]
  print(paste(icd, eg, ricgg, isolata, sds))

  f1 = ifelse(eg>0,eg,NA)
  f2 = ifelse(ricgg>0,ricgg,NA)
  f3 = ifelse(isolata == 1, 1, NA)
  f4 = ifelse(sds == 1, 1, NA)

  fts <- c(f1, f2, f3, f4)

  cond <- paste(conds[which(!is.na(fts))], collapse = " & ")
  #cond <- paste0("if(", cond, ")")

  print(cond)

  sdo_stage8 <- sdo_stage8 |> mutate(violazione_filtro = case_when(eval(parse(text=cond)) & COD_PAT1 == icd ~ TRUE,
                                                                 eval(parse(text=cond)) & patol2 == icd ~ TRUE,
                                                                 eval(parse(text=cond)) & patol3 == icd ~ TRUE,
                                                                 eval(parse(text=cond)) & patol4 == icd ~ TRUE,
                                                                 eval(parse(text=cond)) & patol5 == icd ~ TRUE,
                                                                 eval(parse(text=cond)) & patol6 == icd ~ TRUE,
                                                                 TRUE ~ violazione_filtro))
}



#verifica funzione filtro sds ----
# sdo_stage8[
#   violazione_filtro == TRUE & 
#     !is.na(sds_cc) & 
#     sds_cc < SDS_circonferenza_cranica_cutoff & 
#     (COD_PAT1 %in% filter[sds == 1, ICD9] |
#        patol2  %in% filter[sds == 1, ICD9] |
#        patol3  %in% filter[sds == 1, ICD9] |
#        patol4  %in% filter[sds == 1, ICD9] |
#        patol5  %in% filter[sds == 1, ICD9] |
#        patol6  %in% filter[sds == 1, ICD9]),
#   .(COD_PAT1, patol2, patol3, patol4, patol5, patol6,
#     sds_cc, violazione_filtro, eta_gestazionale, malformazione_tipo)
# ]
# 
# #verifica sds<=2.8 <- esclusi
# 
# malfo_7421_rows <- sdo_stage8[
#   COD_PAT1 == 7421 |
#     patol2 == 7421 |
#     patol3 == 7421 |
#     patol4 == 7421 |
#     patol5 == 7421 |
#     patol6 == 7421
# ]
# 
# # Tra queste, seleziono quelle con SDS ≤ -2.8
# malfo_7421_sotto_cutoff <- malfo_7421_rows[sds_cc<= -2.8]
# 
# # Output del controllo
# if (nrow(malfo_7421_sotto_cutoff) == 0) {
#   cat("✅ Nessuna malformazione 7421 è presente con SDS ≤ -2.8.\n")
# } else {
#   cat(" ATTENZIONE:", nrow(malfo_7421_sotto_cutoff), "righe escluse con malformazione 7421 e SDS ≤ -2.8.\n")
#   print(malfo_7421_sotto_cutoff[, .(violazione_filtro, cedap_linked, sds_cc, COD_PAT1, patol2, patol3, patol4, patol5, patol6)])
# }
# 
# #verifica esckusione righe evidenziate in tab
# # filter_out_check <- read_csv2(paste0(exportDir, "/sdo_stage_8_validated_filters_export.csv"))
# # malfo_7421_sotto_cutoff <- filter_out_check %>%
# #   filter(
# #     sds_cc <= -2.8,
# #     COD_PAT1 == 7421 |
# #       patol2 == 7421 |
# #       patol3 == 7421 |
# #       patol4 == 7421 |
# #       patol5 == 7421 |
# #       patol6 == 7421
# #   )
# # 
# # # Output di controllo
# # if (nrow(malfo_7421_sotto_cutoff) == 0) {
# #   cat("✅ Nessun caso con 7421 e SDS ≤ -2.8 è stato trattenuto: sono tutti esclusi correttamente.\n")
# # } else {
# #   cat("⚠️ ATTENZIONE:", nrow(malfo_7421_sotto_cutoff), "righe con 7421 e SDS ≤ -2.8 sono escluse (filter_out), verifica ok.\n")
# #   print(
# #     malfo_7421_sotto_cutoff %>%
# #       select(cedap_linked, sds_cc, COD_PAT1, patol2, patol3, patol4, patol5, patol6)
# #   )
# # }

# OUT ----

filtered_out <- sdo_stage8 |> filter(violazione_filtro == T) |> write_csv2(file = paste0(exportDir, "/filter_out_export.csv"))
retained <- sdo_stage8 |> filter(violazione_filtro == F) |> write_csv2(file = paste0(exportDir, "/sdo_stage_8_validated_filters_export.csv"))
