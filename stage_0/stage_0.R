

# =====================================================================
# STAGE 0 - VERSIONE 1.0
# Questo script effettua il primo collegamento tra SDO e CEDAP:
# - Carica i dataset SDO e CEDAP
# - Converte le date (nascita, decesso)
# - Filtra i nati nell’anno specificato
# - Collega le SDO al CEDAP tramite PROG_PAZ
# - Recupera la data di nascita da CEDAP dove mancante
# - Calcola i giorni tra nascita e ricovero
# - Esporta i dataset intermedi per lo stage successivo
# =====================================================================

# rm(list=ls())
# 
# # LOAD CONFIG ---
# baseDir=getwd()
# source(paste0(baseDir,"/config.R"), echo = T)
# source(paste0(baseDir,"/functions.R"), echo = T)
# 
# 
# 
# # LOAD DATA ---
# #sdo <- read_csv2(paste0(stage_0Dir, "/sdo_2023_all.csv")) #cambiare dataset con campione creato da script: create_dataset_test_algo.R
# sdo <- read_csv2("~/Desktop/git_hub/dataset_test_casuali/sdo_test.csv")
# cedap <- read_csv2(paste0(cedapDir,"/",cedapFileName))
# 
# # dt_nasc CHAR -> DATE ----
# 
# sdo$dt_nasc <- dmy(sdo$dt_nasc)
# 
# # dt_decesso CHAR -> DATE ----
# 
# sdo$dt_decesso <- dmy(sdo$dt_decesso)
# 
# # COHORT ----
# 
# sdo$cohort <- year(sdo$dt_nasc)
# sdo |> group_by(cohort) |> count()
# 
# # FILTER BY COHORT ----
# 
# sdo_by_dt_nas <- sdo |> filter(cohort == yearOfBirth)
# 
# # LINK TO CEDAP BY PROG_PAZ ----
# 
# ## LOOK UP w dt_nasc----
# 
# linked = rep(0, dim(sdo_by_dt_nas)[1])
# 
# for(i in 1:dim(sdo_by_dt_nas)[1]){
#   linked[i]<-linkByProgPaz(sdo_by_dt_nas[i, "PROG_PAZ"], cedap$prog_paz_neo)
#   print(paste0("ROW:", i, " - SDO PROG_PAZ:", sdo_by_dt_nas[i, "PROG_PAZ"], " - CEDAP ROW: ", linked[i]))
# }
# 
# sdo_by_dt_nas$cedap_linked=linked
# 
# sdo_by_dt_nas_cedap_linked <- sdo_by_dt_nas  |> filter(cedap_linked!=0) 
# 
# sdo_by_dt_nas_cedap_nonlinked <- sdo_by_dt_nas  |> filter(cedap_linked==0)
# 
# ## LOOK UP wo dt_nasc----
# 
# sdo_wo_dt_nas <- sdo |> filter(is.na(dt_nasc))
# 
# linked = rep(0, dim(sdo_wo_dt_nas)[1])
# 
# for(i in 1:dim(sdo_wo_dt_nas)[1]){
#   linked[i]<-linkByProgPaz(sdo_wo_dt_nas[i, "PROG_PAZ"], cedap$prog_paz_neo)
#   print(paste0("ROW:", i, " - SDO PROG_PAZ:", sdo_wo_dt_nas[i, "PROG_PAZ"], " - CEDAP ROW: ", linked[i]))
# }
# 
# sdo_wo_dt_nas$cedap_linked=linked
# 
# sdo_wo_dt_nas_cedap_linked <- sdo_wo_dt_nas  |> filter(cedap_linked!=0) 
# 
# sdo_wo_dt_nas_cedap_nonlinked <- sdo_wo_dt_nas  |> filter(cedap_linked==0)
# 
# # FILL EMPTY dt_nasc ----
# 
# sdo_wo_dt_nas_cedap_linked |> group_by(dt_nasc) |> count()
# 
# for(i in 1:dim(sdo_wo_dt_nas_cedap_linked)[1]){
#   cl<-slice(sdo_wo_dt_nas_cedap_linked,i) |> pull(cedap_linked)
#   dtNas <- cedap |> slice(cl) |> pull(dt_parto)
#   sdo_wo_dt_nas_cedap_linked[i, "dt_nasc"] <- dmy(dtNas)
# }
# 
# sdo_wo_dt_nas_cedap_linked |> filter(!is.na(dt_nasc)) |> select(dt_nasc)
# 
# sdo_wo_dt_nas_cedap_linked$cohort <- year(sdo_wo_dt_nas_cedap_linked$dt_nasc)
# 
# sdo_wo_dt_nas_cedap_linked |> group_by(cohort) |> count()
# 
# sdo_wo_dt_nas_cedap_linked_by_dt_nas <- sdo_wo_dt_nas_cedap_linked |> filter(cohort==yearOfBirth)
# 
# # MERGE DATASET ----
# 
# sdo_1yfup <- bind_rows(sdo_by_dt_nas, sdo_wo_dt_nas_cedap_linked_by_dt_nas)
# 
# 
# # ADMISSION AT DELIVERY ----
# 
# admissionDate <- dmy(sdo_1yfup$dt_amm)
# daysAfterDelivery <- difftime(admissionDate, sdo_1yfup$dt_nasc, units = "days")
# sdo_1yfup$daysAfterDelivery <- as.numeric(daysAfterDelivery)
# 
# sdo_1yfup |> filter(daysAfterDelivery>0) |> count()
# 
# # OUT ----
# 
# write_csv2(sdo_by_dt_nas, paste0(stage_0Dir,"/sdo_by_dt_nas.csv"))
# 
# write_csv2(sdo_by_dt_nas_cedap_linked, paste0(stage_0Dir,"/sdo_by_dt_nas_cedap_linked.csv"))
# 
# write_csv2(sdo_by_dt_nas_cedap_nonlinked, paste0(stage_0Dir,"/sdo_by_dt_nas_cedap_nonlinked.csv"))
# 
# write_csv2(sdo_wo_dt_nas, paste0(stage_0Dir,"/sdo_wo_dt_nas.csv"))
# 
# write_csv2(sdo_wo_dt_nas_cedap_linked, paste0(stage_0Dir,"/sdo_wo_dt_nas_cedap_linked.csv"))
# 
# write_csv2(sdo_wo_dt_nas_cedap_nonlinked, paste0(stage_0Dir,"/sdo_wo_dt_nas_cedap_nonlinked.csv"))
# 
# write_csv2(sdo_wo_dt_nas_cedap_linked_by_dt_nas, paste0(stage_0Dir,"/sdo_wo_dt_nas_cedap_linked_by_dt_nas.csv"))
# 
# write_csv2(sdo_1yfup, paste0(sdoDir,"/sdo_1yfup_", yearOfBirth,".csv"))



# =====================================================================
# STAGE 0 - VERSIONE 1.1 (ottimizzata, stessa logica)
# =====================================================================

rm(list=ls())

# LOAD CONFIG ---
baseDir=getwd()
source(paste0(baseDir,"/config.R"), echo = T)
source(paste0(baseDir,"/functions.R"), echo = T)

# LOAD DATA ---
sdo <- read_csv2(paste0(stage_0Dir, "/sdo_2023_all.csv"))
cedap <- read_csv2(paste0(cedapDir,"/",cedapFileName))

# =====================================================================
# DATE ----
# =====================================================================

sdo$dt_nasc <- dmy(sdo$dt_nasc)
sdo$dt_decesso <- dmy(sdo$dt_decesso)

# =====================================================================
# COHORT ----
# =====================================================================

sdo$cohort <- year(sdo$dt_nasc)
sdo |> group_by(cohort) |> count()

# =====================================================================
# FILTER ----
# =====================================================================

sdo_by_dt_nas <- sdo |> filter(cohort == yearOfBirth)

# =====================================================================
# LINK TO CEDAP (con dt_nasc)
# =====================================================================

## VERSIONE VECCHIA
# linked = rep(0, dim(sdo_by_dt_nas)[1])
# for(i in 1:dim(sdo_by_dt_nas)[1]){
#   linked[i]<-linkByProgPaz(sdo_by_dt_nas[i, "PROG_PAZ"], cedap$prog_paz_neo)
# }

# VERSIONE CORRETTA

linked <- match(sdo_by_dt_nas$PROG_PAZ, cedap$prog_paz_neo)
linked[is.na(linked)] <- 0

sdo_by_dt_nas$cedap_linked = linked

sdo_by_dt_nas_cedap_linked <- sdo_by_dt_nas |> filter(cedap_linked != 0)
sdo_by_dt_nas_cedap_nonlinked <- sdo_by_dt_nas |> filter(cedap_linked == 0)

# =====================================================================
# LINK TO CEDAP (senza dt_nasc)
# =====================================================================

sdo_wo_dt_nas <- sdo |> filter(is.na(dt_nasc))

## VERSIONE VECCHIA
# linked = rep(0, dim(sdo_wo_dt_nas)[1])
# for(i in 1:dim(sdo_wo_dt_nas)[1]){
#   linked[i]<-linkByProgPaz(sdo_wo_dt_nas[i, "PROG_PAZ"], cedap$prog_paz_neo)
# }

## VERSIONE NUOVA

linked <- match(sdo_wo_dt_nas$PROG_PAZ, cedap$prog_paz_neo)
linked[is.na(linked)] <- 0

sdo_wo_dt_nas$cedap_linked = linked

sdo_wo_dt_nas_cedap_linked <- sdo_wo_dt_nas |> filter(cedap_linked != 0)
sdo_wo_dt_nas_cedap_nonlinked <- sdo_wo_dt_nas |> filter(cedap_linked == 0)

# =====================================================================
# FILL dt_nasc ----
# =====================================================================

sdo_wo_dt_nas_cedap_linked |> group_by(dt_nasc) |> count()

## VERSIONE VECCHIA
# for(i in 1:dim(sdo_wo_dt_nas_cedap_linked)[1]){
#   cl<-slice(sdo_wo_dt_nas_cedap_linked,i) |> pull(cedap_linked)
#   dtNas <- cedap |> slice(cl) |> pull(dt_parto)
#   sdo_wo_dt_nas_cedap_linked[i, "dt_nasc"] <- dmy(dtNas)
# }

## VERSIONE NUOVA (vettoriale corretta)

idx <- sdo_wo_dt_nas_cedap_linked$cedap_linked

valid <- !is.na(idx) & idx > 0 & idx <= nrow(cedap)

dt_parto_vec <- cedap$dt_parto[idx[valid]]

valid_dates <- !is.na(dt_parto_vec)

valid_idx <- which(valid)
valid_idx <- valid_idx[valid_dates]

sdo_wo_dt_nas_cedap_linked$dt_nasc[valid_idx] <-
  dmy(dt_parto_vec[valid_dates])

# =====================================================================
# CONTROLLI ----
# =====================================================================

sdo_wo_dt_nas_cedap_linked |> filter(!is.na(dt_nasc)) |> select(dt_nasc)

sdo_wo_dt_nas_cedap_linked$cohort <- year(sdo_wo_dt_nas_cedap_linked$dt_nasc)

sdo_wo_dt_nas_cedap_linked |> group_by(cohort) |> count()

sdo_wo_dt_nas_cedap_linked_by_dt_nas <-
  sdo_wo_dt_nas_cedap_linked |> filter(cohort == yearOfBirth)

# =====================================================================
# MERGE ----
# =====================================================================

sdo_1yfup <- bind_rows(
  sdo_by_dt_nas,
  sdo_wo_dt_nas_cedap_linked_by_dt_nas
)

# =====================================================================
# DAYS ----
# =====================================================================

admissionDate <- dmy(sdo_1yfup$dt_amm)

daysAfterDelivery <- difftime(
  admissionDate,
  sdo_1yfup$dt_nasc,
  units = "days"
)

sdo_1yfup$daysAfterDelivery <- as.numeric(daysAfterDelivery)

sdo_1yfup |> filter(daysAfterDelivery > 0) |> count()

# =====================================================================
# OUTPUT ----
# =====================================================================

write_csv2(sdo_by_dt_nas, paste0(stage_0Dir,"/sdo_by_dt_nas.csv"))
write_csv2(sdo_by_dt_nas_cedap_linked, paste0(stage_0Dir,"/sdo_by_dt_nas_cedap_linked.csv"))
write_csv2(sdo_by_dt_nas_cedap_nonlinked, paste0(stage_0Dir,"/sdo_by_dt_nas_cedap_nonlinked.csv"))

write_csv2(sdo_wo_dt_nas, paste0(stage_0Dir,"/sdo_wo_dt_nas.csv"))
write_csv2(sdo_wo_dt_nas_cedap_linked, paste0(stage_0Dir,"/sdo_wo_dt_nas_cedap_linked.csv"))
write_csv2(sdo_wo_dt_nas_cedap_nonlinked, paste0(stage_0Dir,"/sdo_wo_dt_nas_cedap_nonlinked.csv"))

write_csv2(sdo_wo_dt_nas_cedap_linked_by_dt_nas, paste0(stage_0Dir,"/sdo_wo_dt_nas_cedap_linked_by_dt_nas.csv"))

write_csv2(sdo_1yfup, paste0(sdoDir,"/sdo_1yfup_", yearOfBirth,".csv"))