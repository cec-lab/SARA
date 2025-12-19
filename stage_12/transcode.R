
# ======================================================================
#MAPPING EUROCAT - VERSIONE 1.0 
# Questo script effettua la trasformazione, pulizia e mappatura dei dati SDO+CEDAP in un dataset compatibile con le specifiche EUROCAT.
# Le operazioni principali includono:
# 1. Conversione e normalizzazione delle date, gestendo diversi formati (Excel numerico, date testuali, valori multipli separati da "|").
# 2. Mapping diretto delle variabili da input_df a dset, basato su una tabella di mapping (final_mapping) e su regole fisse per alcune variabili.
# 3. Ricodifica delle variabili secondo le specifiche EUROCAT (es. sex, consang, socm/socf, ecc.).
# 4. Calcoli derivati per alcune variabili presenti in eurocat e non in sdo(es. età genitori, numero di nati, sopravvivenza, tipo di diagnosi, ecc.).
# 5. Gestione di valori mancanti "Not known" tramite codifiche standardizzate ("9") o spazi vuoti ("")
# 
# Lo script è strutturato in sezioni modulari:
# • Pulizia iniziale dei dati
# • Mapping diretto
# • Ricodifica
# • Ricalcolo di variabili complesse
# ======================================================================
rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)


# LOAD DATA ----
input_df_revcode <- read_csv2(paste0(stage_12Dir, "/sdo_stage_11b_clinical_rev_export_final.csv"))
input_df <- read_csv2(paste0(exportDir, "/sdo_stage_11b_clinical_rev_export.csv"))
revcode_subset <- input_df_revcode[, c(patology_code_cols_icd10, patology_label_cols_icd9, intervention_label_cols, "revcode")]
eurocat_data_dict <- read_excel(paste0(tableDir,"/eurocat_data_dict.xlsx"))
final_mapping <- read_csv2(paste0(tableDir, "/final_mapping.csv"))
cedap_plus_2023 <- read_csv2(paste0(cedapDir,"/",cedapFileName))
eurocat_sdo_mapping <- read_excel(paste0(tableDir, "/eurocat_sdo_mapping.xlsx"))

# SOSTITUZIONE LABEL IN input_df
# all(input_df$PROG_PAZ == revcode_subset$PROG_PAZ)  # Deve restituire TRUE per essere sicuri che le righe siano identiche
input_df[, patology_label_cols_icd9] <- revcode_subset[, patology_label_cols_icd9]
input_df[, intervention_label_cols] <- revcode_subset[, intervention_label_cols]

# AGGIUNTA COLONNA revcode
input_df$revcode <- input_df_revcode$revcode

sdo_cedap_dd <- read_csv2(paste0(tableDir, "/sdo_cedap_dd.csv"))
eurocat_vars <- final_mapping$EUROCAT_VARIABLE %>% unique() %>% na.omit()
dset <- as.data.frame(matrix(NA, nrow = nrow(input_df), ncol = length(eurocat_vars)))
colnames(dset) <- eurocat_vars





# Inizializza il nuovo data frame vuoto di Eurocat
n_righe <- nrow(input_df)
vars_eurocat <- eurocat_sdo_mapping$EUROCAT_Variable
# Crea un data frame vuoto con colonne da EUROCAT e righe = dataset sorgente
dset <- as_tibble(setNames(replicate(length(vars_eurocat), rep(NA, n_righe), simplify = FALSE), vars_eurocat))
# Loop attraverso tutte le righe di final_mapping per il mapping
for (i in seq_along(final_mapping$EUROCAT_VARIABLE)) {
  # Nome della variabile EUROCAT e della variabile SDO associata
  eurocat_var <- final_mapping$EUROCAT_VARIABLE[i]
  sdo_var <- final_mapping$SDO_CEDAP_DD[i]
  
  # Aggiungiamo un controllo per evitare errori quando la variabile EUROCAT è NA
  if (!is.na(eurocat_var) && !is.na(sdo_var) && sdo_var %in% colnames(input_df)) {
    # Se la variabile SDO esiste nel dataset "input_df", prendiamo i valori
    dset[[eurocat_var]] <- input_df[[sdo_var]]
    
    # Correggi i formati per variabili specifiche
    # 1. Se la variabile è una data (ad esempio 'birth_date'), convertiamola in formato data
    if (eurocat_var == "birth_date") {
      dset[[eurocat_var]] <- as.Date(dset[[eurocat_var]])
    }
    # 2. Se la variabile è un numero (ad esempio 'prog_paz'), convertiamola in formato numerico
    if (eurocat_var == "prog_paz") {
      dset[[eurocat_var]] <- as.numeric(dset[[eurocat_var]])
    }
    
  } else {
    # Se la variabile SDO non esiste o è NA, lasciamo i valori come NA (già inizializzati)
    warning(paste("Colonna SDO mancante o NA per:", sdo_var))
  }
}


# === 1. MAPPING DIRETTO ===----

direct_mapping <- final_mapping %>%
  filter(RICODIFICARE == 0, CALCOLARE == 0, !is.na(SDO_CEDAP_DD))

for (i in seq_len(nrow(direct_mapping))) {
  eurocat_var <- as.character(direct_mapping$EUROCAT_VARIABLE[i])
  source_var <- as.character(direct_mapping$SDO_CEDAP_DD[i])
  
  # Salta se eurocat_var è NA
  if (is.na(eurocat_var)) {
    next
  }
  
  if (source_var %in% names(input_df)) {
    dset[[eurocat_var]] <- input_df[[source_var]]
  } else {
    warning(glue::glue("Variabile '{source_var}' non trovata in input_df (i = {i})"))
  }
}

# Aggiunta manuale delle variabili con valore "Not Known"
not_known_vars <- c("presyn", "premal1", "premal2", "premal3", "premal4",
                    "premal5", "premal6", "premal7", "premal8")

for (var in not_known_vars) {
  dset[[var]] <- 9
}

# Mapping diretto delle etichette da input_df a sp_malfo1, sp_malfo2, sp_malfo3, etc.
dset$sp_malfo1 <- ifelse(!is.na(input_df$COD_PAT1_label_icd10), input_df$COD_PAT1_label, 9)
dset$sp_malfo2 <- ifelse(!is.na(input_df$patol2_label_icd10), input_df$patol2_label, 9)
dset$sp_malfo3 <- ifelse(!is.na(input_df$patol3_label_icd10), input_df$patol3_label, 9)
dset$sp_malfo4 <- ifelse(!is.na(input_df$patol4_label_icd10), input_df$patol4_label, 9)
dset$sp_malfo5 <- ifelse(!is.na(input_df$patol5_label_icd10), input_df$patol5_label, 9)
dset$sp_malfo6 <- ifelse(!is.na(input_df$patol6_label_icd10), input_df$patol6_label, 9)


# Mapping diretto delle patol code icd10 con le malfo1..malfo6 ----
for (i in 1:6) {
  # Nome colonna in input_df
  if (i == 1) {
    patol_code_col <- "COD_PAT1_code_icd10"
  } else {
    patol_code_col <- paste0("patol", i, "_code_icd10")
  }
  
  malfo_col <- paste0("malfo", i)
  
  if (patol_code_col %in% colnames(input_df)) {
    dset[[malfo_col]] <- input_df[[patol_code_col]]
  } else {
    warning(paste("Colonna", patol_code_col, "non trovata in input_df"))
  }
}
for (i in 1:6) {
  malfo_col <- paste0("malfo", i)
  if (malfo_col %in% colnames(dset)) {
    dset[[malfo_col]] <- gsub("\\.", "", dset[[malfo_col]])
  }
}

#mapping revcode ----

dset$revcode <- input_df$revcode

#mapping Neo_trasf ----
dset$NEO_TRASF <- sapply(input_df$NEO_TRASF, function(x) paste0(gsub("^NA$", "9", str_trim(unlist(strsplit(as.character(x), "\\|")))), collapse = "|"))

# Lista delle variabili usate nel mapping diretto (inclusi gli aggiunti a mano)
mapping_list <- paste0(direct_mapping$EUROCAT_VARIABLE, " = ", direct_mapping$SDO_CEDAP_DD)
cat(mapping_list, sep = "\n")






# === 2. MAPPING CON RICODIFICA ===----
ricodifica_vars <- final_mapping %>%
  filter(RICODIFICARE == 1, CALCOLARE == 0, !is.na(SDO_CEDAP_DD)) %>%
  pull(EUROCAT_VARIABLE)




#SOCM E SOCF ----
dset$socm <- 9
dset$socf <- 9

# Codifica: se SOCIO_ECONOMICA_MADRE o PADRE è 4 o 6 => 8, altrimenti 9
dset$socm <- ifelse(input_df$CONDIZIONE_PROF_MADRE %in% c(4, 6), 8, dset$socm)
dset$socf <- ifelse(input_df$CONDIZIONE_PROF_PADRE %in% c(4, 6), 8, dset$socf)

# SEX ----
# CEDAP: 1 = maschio, 2 = femmina, 3 = indeterminato, 9 = non noto
# EUROCAT: 1 = Male, 2 = Female, 3 = Indeterminate, 9 = Not known
# STEP 1: ricodifica da stringa a numerico (M/F -> 1/2)
input_df$SEX_NUM <- recode(input_df$SEX,
                           "M" = 1,
                           "F" = 2,
                           .default = 9)  # Not known

# STEP 2: ricodifica da numerico a EUROCAT
dset$sex <- recode(as.character(input_df$SEX_NUM),
                   `1` = 1,  # Male
                   `2` = 2,  # Female
                   `3` = 3,  # Indeterminate
                   `9` = 9,  # Not known
                   .default = 9)

# TYPE ----
dset$type <- recode(input_df$VITALITA,
                    `1` = 1,  # Live birth
                    `2` = 2,  # Stillbirth
                    .default = 9)  # Not known

# consang ----
# CEDAP: 1-3 = gradi di consanguineità (fino al 6°), 0 = non consanguinei
# EUROCAT: 1 = Relationship of second cousin or closer, 0 = Not related or more distant, 9 = Not known
dset$consang <- recode(input_df$CONSANGUINEITA,
                       `1` = 1,
                       `2` = 1,
                       `3` = 1,
                       `0` = 0,
                       .default = 9)

# totpreg ----
# CEDAP: 
# 0 = Nessuno,
# 1 = Uno,
# 2 = Due,
# 3 = Tre o più
# EUROCAT: 
# "00" = None, 
# "01", "02" = 1 or 2 previous conceptions, 
# "03" = Three or more, 
# "99" = Not known
# Step 1: Calcolo dei concepimenti precedenti

input_df$CONCEPIMENTI_PRECEDENTI <- dplyr::case_when(   #avevamo messo le etichette in sds. adesso riconvertito per calcolo eurocat
  input_df$CONCEPIMENTI_PRECEDENTI == "SI" ~ 1,
  input_df$CONCEPIMENTI_PRECEDENTI == "NO" ~ 0,
  TRUE ~ NA_real_
)

# Step 2: Transcodifica secondo EUROCAT
dset$totpreg <- recode(
  input_df$CONCEPIMENTI_PRECEDENTI,
  `0` = "00",   # Nessuno
  `1` = "01",   # Uno
  `2` = "02",   # Due
  `3` = "03",   # Tre o più
  `4` = "03",
  `5` = "03",
  `6` = "03",
  `7` = "03",
  .default = "99"  # Non noto 
)

# moanom ----
# CEDAP: 1 = Sì, 2 = No
# EUROCAT: 1 = "Same", 4 = "No", 9 = "Not known"
# Regola: se è "No", allora 4, altrimenti 9
dset$moanom <- ifelse(input_df$MALFORMAZIONI_PARENTI_MADRE == 2, 4, 9)

# faanom ----
# CEDAP: 1 = Sì, 2 = No
# EUROCAT: "1" = Same, "4" = No, "9" = Not known
# Regola: se è "No", allora "4", altrimenti "9"
dset$faanom <- ifelse(input_df$MALFORMAZIONI_PARENTI_PADRE == 2, 4, 9)

# sibanom ----
# # CEDAP: 1 = Sì, 2 = No
# EUROCAT: "4" = No, "9" = Not known
# Regola: se è "NO", allora "4", altrimenti "9"
dset$sibanom <- ifelse(input_df$MALFORMAZIONI_FRATELLI_SORELLE == 2, 4, 9)



# whendisc ----
# CEDAP: settimane alla diagnosi (valori numerici o NA).Se l’età gestazionale è < 42 settimane, si tratta di diagnosi prenatale → whendisc = 6
#Se è ≥ 42 settimane, è postnatale →  usare whendisc = 1

# EUROCAT: 6 = Prenatal diagnosis, 1 = At birth
dset$whendisc <- ifelse(
  (!is.na(input_df$ETA_GESTAZIONALE_ALLA_DIAGNOSI) & input_df$ETA_GESTAZIONALE_ALLA_DIAGNOSI < 42),
  6,
  1
)

# pm ----
# CEDAP: 1 = confermata da autopsia, 2 = risultato disponibile successivamente, 3 = non richiesta
# EUROCAT: 1 = Performed results known, 2 = Performed results not known, 3 = Not performed, 9 = Not known
dset$pm <- recode(as.character(input_df$riscontro_autoptico),
                  `1` = 1,  # Performed results known
                  `2` = 2,  # Performed results not known
                  `3` = 3,  # Not performed
                  .default = 9)  # Not known

# matedu ----
# CEDAP: 1 = laurea, 2 = diploma universitario, 3 = diploma superiore, 4 = media inferiore/elementare/nessun titolo
# EUROCAT: 3 = Tertiary, 2 = Upper secondary, 1 = Elementary and lower secondary, 9 = Not known
dset$matedu <- recode(input_df$TITOLO_DI_STUDIO_MADRE,
                      `1` = 3,
                      `2` = 3,
                      `3` = 2,
                      `4` = 1,
                      .default = 9)



ricodifica_vars <- c("sex", "consang", "totpreg", "socm", "moanom", "faanom", 
                     "sibanom", "type", "whendisc", "assconcept", "socf", 
                     "pm", "matedu")





# === 5. MAPPING CON RICALCOLO ===
ricalcolo_vars <- final_mapping %>%
  filter(CALCOLARE == 1) %>%
  pull(EUROCAT_VARIABLE)

# UTILIZZARE ANNO DATA DI NASCITA BAMBINO
birth_year <- year(dset$birth_date)

# agefa ----
dset$agefa <- input_df %>%
  transmute(agefa = ifelse(!is.na(data_nascita_padre) & !is.na(birth_year),
                           birth_year - data_nascita_padre, NA)) %>%
  pull(agefa)


# agemo ----
dset$agemo <- input_df %>%
  transmute(agemo = ifelse(!is.na(dt_nas_m) & !is.na(birth_year),
                           birth_year - lubridate::year(dt_nas_m),
                           NA)) %>%
  pull(agemo)

# centre ----
dset$centre <- 18

# karyo ----
# Origine CEDAP: CARIOTIPO_DEL_NATO (indica se è stato fatto il cariotipo)
# Destinazione EUROCAT: 
#   1 = Performed result known (se presente cariotipo),
#   9 = Not known (se assente)
dset$karyo <- ifelse(!is.na(input_df$CARIOTIPO_DEL_NATO) & input_df$CARIOTIPO_DEL_NATO != "", 1, 9)




#survival ----
# Destinazione EUROCAT:
# •	1: Il bambino è vivo dopo una settimana.
# •	2: Il bambino è morto entro la prima settimana.
# •	9: Non noto.

dset$survival <- with(input_df, ifelse(
  !is.na(dt_decesso) & !is.na(dt_nasc),
  ifelse(as.numeric(difftime(ymd(dt_decesso), ymd(dt_nasc), units = "days")) >= 7, 2, 2),  # morto (entro o dopo 7 gg, ma sempre 2 = No)
  ifelse(!is.na(dt_dim) & !is.na(dt_nasc),
         ifelse(as.numeric(difftime(ymd(dt_dim), ymd(dt_nasc), units = "days")) >= 7, 1, 1),  # vivo e dimesso >= 7 giorni = 1 = Yes
         9  # nessuna info
  )
))





# condisc ----
# DA VERIFICARE
# Origine CEDAP: default implicito = '9' 
# Destinazione EUROCAT:
#   1 = Alive ,
#   2 = Dead,
#   9 = Not known (default)
dset$condisc <- rep(9, nrow(input_df))  # Default '9' per tutti



#firstpre ----
# DEFAULT NON NOTO
# Origine CEDAP: nessuna (lasciare vuoto)
# Destinazione EUROCAT: 1, Ultrasound at GA < 14 weeks | 2, Ultrasound at GA 14-21 weeks | 3, Ultrasound at GA ≥ 22 weeks | 4, Ultrasound GA not known | 5, Serum/ combined screening | 6, Chorion villus sampling or amniocentesis | 7, Other test positive | 8, Test(s) performed, result negative | 9, Not known | 10, No test performed | 11, Fetal karyotype on maternal blood
dset$firstpre <- 9

#gentest ----
# DEFAULT NON NOTO
# Origine CEDAP: ""
# Destinazione EUROCAT: 1, Specific genetic test positive | 2, Specific genetic test negative | 3, Specific genetic test not Performed | 9, Not Known if genetic test is performed or result not known
dset$gentest <- 9



#nrbaby ----

#Calcolo bambini nati per ogni madre tramite prog_mpaz_m dal cedap 
input_df <- input_df %>%
  mutate(prog_paz_m = as.character(prog_paz_m))

cedap_plus_2023 <- cedap_plus_2023 %>%
  mutate(prog_paz_m = as.character(prog_paz_m))

# Conta il numero di righe nel CEDAP per ogni madre (prog_paz_m)
baby_counts_cedap <- cedap_plus_2023 %>%
  group_by(prog_paz_m) %>%
  summarise(nrbaby_cedap = n(), .groups = "drop")

# Join del conteggio direttamente su input_df tramite prog_paz_m
input_df <- input_df %>%
  left_join(baby_counts_cedap, by = "prog_paz_m")

dset$nbrbaby <- case_when(
  !is.na(input_df$nrbaby_cedap) & input_df$nrbaby_cedap == 1 ~ 1,
  !is.na(input_df$nrbaby_cedap) & input_df$nrbaby_cedap == 2 ~ 2,
  !is.na(input_df$nrbaby_cedap) & input_df$nrbaby_cedap == 3 ~ 3,
  !is.na(input_df$nrbaby_cedap) & input_df$nrbaby_cedap == 4 ~ 4,
  !is.na(input_df$nrbaby_cedap) & input_df$nrbaby_cedap == 5 ~ 5,
  !is.na(input_df$nrbaby_cedap) & input_df$nrbaby_cedap >= 6 ~ 6,
  TRUE ~ 9
)



 
#civreg ----
# Origine CEDAP: VITALITA
# Destinazione EUROCAT: 1 se vitalità = 1, altrimenti 9 = Not Known
dset$civreg <- ifelse(input_df$VITALITA == 1, 1, 9)





#bmi ----
# Origine CEDAP: ALTEZZA_MADRE (in cm), PESO_MADRE_AL_PARTO (in kg)
# Destinazione EUROCAT: BMI calcolato = peso / (altezza in m)^2
dset$bmi <- with(input_df, {
  altezza_m <- as.numeric(ALTEZZA_MADRE) / 100
  peso <- as.numeric(PESO_MADRE_AL_PARTO)
  bmi_value <- peso / (altezza_m^2)
  round(bmi_value, 1)
})





#'metodi_PMA' ----
dset$assconcept <- recode(input_df$metodi_PMA,
                          `1` = 1,
                          `2` = 2,
                          `3` = 4,
                          `4` = 3,
                          `5` = 5,
                          `6` = 10,
                          .default = 9)                      
                          
                          
                          

#syndromes ----

# Carica file sindromi
# syndromes <- read_delim("/home/imer/works/algo_sdo/tables/syndromes.csv", 
#                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
# 
# # Codici e etichette
# icd10 <- trimws(syndromes$`ICD10- BPA`)
# labels <- syndromes$Syndrome
# 
# # Scorri ogni riga
# for (i in 1:nrow(dset)) {
#   # Estrai codici malfo
#   malfos <- as.character(unlist(dset[i, paste0("malfo", 1:6)]))
#   malfos <- trimws(malfos)
#   
#   # Cerca la prima sindrome
#   match_idx <- match(malfos, icd10)
#   first_match <- which(!is.na(match_idx))[1]
#   
#   if (!is.na(first_match)) {
#     # Salva codice sindrome e descrizione
#     dset$syndrome[i] <- icd10[match_idx[first_match]]
#     dset$sp_syndrome[i] <- labels[match_idx[first_match]]
#     
#     # Rimuovi il codice matched e shift left
#     malfos <- malfos[-first_match]
#     malfos <- c(malfos, rep(NA, 6 - length(malfos)))
#     
#     # Riscrivi colonne malfo aggiornate
#     for (j in 1:6) {
#       dset[i, paste0("malfo", j)] <- malfos[j]
#     }
#   }
# }
# === syndromes corretto con shift ===

# Carica file sindromi
syndromes <- read_delim("/home/imer/works/algo_sdo/tables/syndromes.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Codici e etichette
icd10 <- trimws(syndromes$`ICD10- BPA`)
labels <- syndromes$Syndrome

# Loop sulle righe
for (i in 1:nrow(dset)) {
  # Estrai codici malfo e sp_malfo, riempi con NA se mancanti
  malfos <- as.character(unlist(dset[i, paste0("malfo", 1:6)]))
  sp_malfos <- as.character(unlist(dset[i, paste0("sp_malfo", 1:6)]))
  
  if (length(malfos) < 6) malfos <- c(malfos, rep(NA, 6 - length(malfos)))
  if (length(sp_malfos) < 6) sp_malfos <- c(sp_malfos, rep(NA, 6 - length(sp_malfos)))
  
  # Nuovi vettori per malfo e sp_malfo
  new_malfo <- rep(NA, 6)
  new_sp_malfo <- rep(NA, 6)
  
  sind_codes <- c()
  sind_labels <- c()
  
  malfo_idx <- 1
  
  for (j in 1:6) {
    code <- trimws(malfos[j])
    label <- sp_malfos[j]
    
    if (!is.na(code) && code != "") {
      if (code %in% icd10) {
        # È una sindrome
        sind_codes <- c(sind_codes, code)
        sind_labels <- c(sind_labels, labels[match(code, icd10)])
      } else {
        # È una malformazione, metti a sinistra
        if (malfo_idx <= 6) {
          new_malfo[malfo_idx] <- code
          new_sp_malfo[malfo_idx] <- label
          malfo_idx <- malfo_idx + 1
        }
      }
    }
  }
  
  # Aggiorna dset con malfo shiftate
  for (k in 1:6) {
    dset[i, paste0("malfo", k)] <- new_malfo[k]
    dset[i, paste0("sp_malfo", k)] <- new_sp_malfo[k]
  }
  
  # Aggiorna sindrome
  if (length(sind_codes) > 0) {
    dset$syndrome[i] <- paste(sind_codes, collapse = "|")
    dset$sp_syndrome[i] <- paste(sind_labels, collapse = "|")
  } else {
    dset$syndrome[i] <- NA
    dset$sp_syndrome[i] <- NA
  }
}


#surgery ----
#eurocat: 1, Performed (or expected) in the first year of life | 2, Performed (or expected) after the first year of life | 3, Prenatal surgery | 4, No surgery required | 5, Too severe for surgery | 6, Died before surgery | 9, Not known
# Imposta surgery a "1" se validation_type contiene il valore 2 (da solo o tra pipe), altrimenti "9"

dset$surgery <- ifelse(
  grepl("(^2$|(^|\\|)2(\\||$))", as.character(input_df$validation_type)),
  1,
  9
)



#VARIABILI MANCANTI


# lista delle variabili EUROCAT con mapping diretto
direct_mapping_vars <- final_mapping %>%
  filter(RICODIFICARE == 0, CALCOLARE == 0, !is.na(SDO_CEDAP_DD)) %>%
  pull(EUROCAT_VARIABLE) %>%
  unique()

variabili_attese <- unique(c(ricodifica_vars, ricalcolo_vars, direct_mapping_vars))
variabili_mapping <-final_mapping$EUROCAT_VARIABLE
variabili_mancanti<- setdiff(variabili_mapping, variabili_attese)
print(variabili_mancanti)








#Eliminare le righe revisionate con revcode=0----


# Riepilogo prima del filtro
cat("Righe totali prima del filtro:", nrow(dset), "\n")
print(table(dset$revcode))

# Filtro: rimuove le righe con revcode == 0
dset <- dset %>% filter(revcode != 0)

# Riepilogo dopo il filtro
cat("Righe totali dopo il filtro:", nrow(dset), "\n")
print(table(dset$revcode))

sdo_stage12_transcode <- dset %>% filter(revcode != 0)

colSums(is.na(dset))






# Sostituzione NA per variabili specifiche in dset ----

# 1. Rimozione colonne non necessarie
dset$cov_severity <- NULL
dset$start_cov <- NULL
dset$`NA` <- NULL
dset$source <-"SDO"

# 2. Conversione delle stringhe "NA" in veri NA
dset <- dset %>%
  mutate(across(where(is.character), ~na_if(., "NA")))

# 3. Conversione colonne logical in numeriche
dset <- dset %>%
  mutate(across(where(is.logical), ~as.numeric(.)))

# 4. Riempimento NA per malformazioni  (malfo1 - malfo6) ----
dset <- dset %>%
  mutate(across(matches("^malfo[1-6]$"), ~replace_na(., "")))

# 5. Riempimento NA per variabili specifiche con codici "9", "99", "999" ecc.
dset$weight <- as.character(dset$weight)
dset$weight <- sapply(dset$weight, function(x) paste(ifelse(is.na(x), 9999, ifelse(x == "NA", 9999, x)), collapse = "|"), USE.NAMES = FALSE)
dset$weight <- sapply(strsplit(as.character(dset$weight), "\\|"), function(parts) paste(ifelse(parts %in% c("NA", "", NA), 9999, parts), collapse = "|"))

# gestlength
dset$gestlength[is.na(dset$gestlength)] <- 99
dset$gestlength <- gsub("\\bNA\\b", 99, dset$gestlength)


dset$drugs1 <- as.character(dset$drugs1)
dset$drugs2 <- as.character(dset$drugs2)
dset$drugs3 <- as.character(dset$drugs3)
dset$drugs4 <- as.character(dset$drugs4)
dset$drugs5 <- as.character(dset$drugs5)
dset$extra_drugs <- as.character(dset$extra_drugs)
dset$sp_firstpre <- as.character(dset$sp_firstpre)

dset <- dset %>%
  mutate(
    datemo = replace_na(as.character(datemo), "xx-xx-xxxx"),
    nbrbaby = replace_na(nbrbaby, 9),
    sex = replace_na(sex, 9),
    type = replace_na(type, 9),
    survival = replace_na(survival, 9),
    totpreg = replace_na(totpreg, "99"),
    whendisc = replace_na(whendisc, 9),
    agedisc = replace_na(agedisc, 99),
    condisc = replace_na(condisc, 9),
    karyo = replace_na(karyo, 9),
    surgery = replace_na(surgery, 9),
    pm = replace_na(pm, 9),
    presyn = replace_na(presyn, 9),
    matdiab = replace_na(matdiab, 9),
    sp_drugs = replace_na(sp_drugs, ),
    syndrome = replace_na(syndrome, ),
    premal1 = replace_na(premal1, 9),
    premal2 = replace_na(premal2, 9),
    premal3 = replace_na(premal3, 9),
    premal4 = replace_na(premal4, 9),
    premal5 = replace_na(premal5, 9),
    premal6 = replace_na(premal6, 9),
    premal7 = replace_na(premal7, 9),
    premal8 = replace_na(premal8, 9),
    socf = replace_na(socf, 9),
    illbef1 = replace_na(illbef1, 9),
    illbef2 = replace_na(illbef2, 9),
    illdur1 = replace_na(illdur1, 9),
    illdur2 = replace_na(illdur2, 9),
    numloc = replace_na(numloc, 9),
    sp_gentest = replace_na(sp_gentest, ),
    imer_key = replace_na(imer_key, 9),
    omim = replace_na(omim, 9),
    orpha = replace_na(orpha, 9),
    extra_er_resmo = replace_na(extra_er_resmo, 9),
    occupmo = replace_na(occupmo, 9999),
    folic_g14 = replace_na(folic_g14, 9),
    extra_drugs = replace_na(extra_drugs, ""),
    firsttri = replace_na(firsttri, 9),
    assconcept = replace_na(assconcept, 9),
    agefa = replace_na(agefa, 99),
    agemo = replace_na(agemo, 99),
    firstpre = replace_na(firstpre, 9),
    sp_firstpre = replace_na(sp_firstpre, ""),
    migrant =  replace_na(migrant, 9),
    sp_syndrome = replace_na(sp_syndrome, ""),
    drugs1 = replace_na(drugs1, ""),
    drugs2 = replace_na(drugs2, ""),
    drugs3 = replace_na(drugs3, ""),
    drugs4 = replace_na(drugs4, ""),
    drugs5 = replace_na(drugs5, ""),
    inf_cov_test = replace_na(inf_cov_test, 9),
    imm_cov_test = replace_na(imm_cov_test, 9),
    oth_cov_test = replace_na(oth_cov_test, 9),
    record_id = replace_na(record_id, 9999),
    nbrmalf = replace_na(nbrmalf, 9),
    death_date = ifelse(is.na(death_date), 2222-22-22, death_date),
    sds_cc = replace_na(sds_cc, 9),
    mocitizenship = replace_na(mocitizenship, 999),
    sp_karyo = replace_na(sp_karyo, ""),
    bmi = replace_na(bmi, 99),
    circ_cran_neo = replace_na(circ_cran_neo, 99),
    mo_smoking = replace_na(mo_smoking, 99),
    mo_alcohol = replace_na(mo_alcohol, 99),
    civreg = replace_na(civreg, 9),
    consang = replace_na(consang, 9),
    moanom = replace_na(moanom, 9),
    sibanom = replace_na(sibanom, 9),
    faanom =  replace_na(faanom, 9),
    matedu = replace_na(matedu, 9),
    
    
  )

# 6. Riempimento per tutte le colonne che iniziano con "sp_" non già trattate
target_cols <- grep("^sp_|^pre_", names(dset), value = TRUE)

# Ciclo su ogni colonna target
for (col in target_cols) {
  # Se la colonna è character: sostituisco "NA" stringa
  if (is.character(dset[[col]])) {
    dset[[col]] <- ifelse(dset[[col]] == "NA", "", dset[[col]])
    
    # Se la colonna è numeric: la converto in character, poi sostituisco NA
  } else if (is.numeric(dset[[col]])) {
    dset[[col]] <- as.character(dset[[col]])
    dset[[col]][is.na(dset[[col]])] <- ""
  }
}


for (var in vars_with_na) {
  # Se è numerica, converti in character
  if (is.numeric(dset[[var]])) {
    dset[[var]] <- as.character(dset[[var]])
  }
  
  # Trova posizioni di NA o valori "9" (in character)
  idx_na <- is.na(dset[[var]])
  idx_9 <- dset[[var]] == 9
  
  # Sostituisci NA o 9 con stringa vuota
  dset[[var]][idx_na | idx_9] <- ""
}



# Rimuove spazi bianchi da tutte le colonne del dataframe
dset <- data.frame(lapply(dset, function(x) {
  if(is.character(x)) {
    x <- trimws(x)        # elimina spazi a inizio/fine
          
  }
  return(x)
}))

dset$NEO_TRASF <- sapply(dset$NEO_TRASF, function(x) {
  if (is.na(x)) return(9)
  x_split <- unlist(strsplit(x, "\\|"))
  x_clean <- ifelse(trimws(x_split) %in% c("NA", ""), 9, trimws(x_split))
  paste(x_clean, collapse = "|")
})

colSums(is.na(dset))




#Le righe collassate vengono riportate splittate ----
# Identifica colonne che contengono almeno una cella con "|"
cols_collassate <- sapply(dset, function(col) any(grepl("\\|", col)))
cols_to_tyde<-names(cols_collassate)[cols_collassate]



# cols_to_split <- c("gestlength", "weight", "cod_pres") # colonne da "pulire"

# clean_first_value <- function(column) {
#   
#     # Prendo solo la prima parte prima di "|"
#     first_part <- strsplit(column, "\\|")[[1]][1]
#     
#     # Se la colonna è numerica (gestlength, weight), converto a numerico
#     #if (col %in% c("gestlength", "weight")) {
#     #  first_part <- as.numeric(first_part)
#     #}
#     
#     # Assegno il valore "pulito" alla riga
#     row[[col]] <- first_part
#   }
#   return(row)
# }

for (cln in cols_to_tyde) {
  print(cln)
  dset[, cln]<-sapply(strsplit(dset[,cln], "\\|"), "[[", 1)
}

# Applico la funzione a tutto il dataset
# dset[cols_to_split] <- strsplit(dset[cols_to_split, ], "\\|")[[1]][1]

# Converto gestlength e weight in numerico perché apply le trasforma in carattere
# dset$gestlength <- as.numeric(dset$gestlength)
# dset$weight <- as.numeric(dset$weight)
# 
# print(dset)


# Ordinare le vars come Eurocat ----
# 1. Pulisci i nomi da spazi indesiderati
eurocat_vars <- trimws(eurocat_data_dict$`Variable / Field Name`)
names(dset) <- trimws(names(dset))

# 2. Trova e stampa le variabili mancanti
missing_vars <- setdiff(eurocat_vars, names(dset))
cat("Variabili mancanti che verranno aggiunte:\n")
print(missing_vars)

# 3. Aggiungi le variabili mancanti come colonne vuote ("")
for (var in missing_vars) {
  dset[[var]] <- ""
}

# 4. Riorganizza le colonne: prima le EUROCAT, poi le extra aggiunte da noi
ordered_cols <- c(eurocat_vars, setdiff(names(dset), eurocat_vars))
dset <- dset[, ordered_cols]

dset$sp_cario <- NULL #abbiamo compilato sp_kario che è quella giusta DA RIMUOVERE
#dset$assoconcept <- NULL


# date_cols <- sapply(dset, inherits, "Date")
# dset[date_cols] <- lapply(dset[date_cols], function(x) paste0("'", format(x, "%Y-%m-%d")))



write_csv2(dset, file = paste0(exportDir, "/sdo_stage12_transcode_export.csv"), na = "")




