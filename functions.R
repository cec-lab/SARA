# =====================================================
# SCRIPT COMPLETO CON 12 STAGE - VERSIONE 1.0
# Descrizione: Script integrato per gestione, pulizia, 
# validazione e analisi dei dati ICD9 nei dataset SDO e 
# CEDAP. Include funzioni per codifiche, filtri, 
# patologie minori, validazioni, ricodifiche, 
# malformazioni uniche e variabili derivate.
# =====================================================

# =====================================================
# GLOBAL DEFINITIONS ----
# =====================================================

malfoCodes <- as.character(740:759)

icd9SearchCols <- c("COD_PAT1", "patol2", "patol3", "patol4", "patol5", "patol6")
columns_icd9_extra <- c("COD_PAT1", "patol2", "patol3", "patol4", "patol5", "patol6")
sdo_link_column <- "cedap_linked"  
# extra_codes <- extra_codes$ICD9
icd9_sdo_cols <- c("COD_PAT1", "patol2", "patol3", "patol4", "patol5", "patol6")
icd9_cedap_cols <- c("Malformazione_diagnosticata_1", "Malformazione_diagnosticata_2", "Malformazione_diagnosticata_3")
colonne_date <- c("dt_nasc", "dt_decesso", "dt_amm", "dt_dim")
patology_cols <- c("COD_PAT1", "patol2", "patol3", "patol4", "patol5", "patol6")
intervention_cols <- paste0("interv", 1:11)
intervention_label_cols <- paste0(intervention_cols, "_label")
patology_cols <- c("COD_PAT1", "patol2", "patol3", "patol4", "patol5", "patol6")
patology_label_cols_icd9 <- paste0(patology_cols, "_label")
patology_code_cols_icd10 <- paste0(patology_cols, "_code_icd10")
patology_label_cols_icd10 <- paste0(patology_cols, "_label_icd10")

ricalcolo_vars <- c("agefa", "agemo", "centre", "source", NA, NA, "karyo", 
                    "survival", "condisc", "firstpre", "gentest", "nbrbaby", 
                    "civreg", "bmi", "syndromes", "assconcept", "surgery")

vars_with_na <- c("sdo_number", "sp_malfo1", "sp_malfo2", "sp_malfo3", "sp_malfo4",
                  "sp_malfo5", "sp_malfo6", "malfo7", "malfo8", "prevsib", 
                  "sib1", "sib2", "sib3", "genrem", "lung_neo")

# =====================================================
# STAGE 0 - Funzioni di collegamento tra PROG_PAZ
# =====================================================

#' linkByProgPaz
#' 
#' @description Funzione di collegamento uno-a-uno per identificare la posizione di un valore all’interno di un vettore (targetKey). 
#' @param inKey Chiave da cercare.
#' @param targetKey Vettore in cui effettuare la ricerca.
#' @return Indice della riga in `targetKey` dove si trova `inKey`, oppure 0 se non univoco.
linkByProgPaz <- function(inKey, targetKey){
  l = grep(inKey, targetKey)
  return(ifelse(length(l) == 1, l , 0))
}

#Viene utilizzata per trovare la riga del CEDAP corrispondente a un soggetto SDO, usando il codice PROG_PAZ

# =====================================================
# STAGE 1 - Filtro iniziale codici ICD9 malformativi
# =====================================================

#' codeFilter
#'
#' @description Valida un codice ICD9 compreso nel range 740-759.
#' @param code Codice ICD9 da valutare.
#' @return TRUE se il codice è tra 740 e 759, FALSE altrimenti.
codeFilter <- function(code) {
  if (is.na(code)) return(FALSE)
  valore <- as.character(code)
  if (nchar(valore) < 3) return(FALSE)
  primi_tre <- substr(valore, 1, 3)
  return(primi_tre >= "740" && primi_tre <= "759")
}

#' rowFilterByCodes
#'
#' @description Filtra righe in base a codici ICD9 presenti in colonne definite
#' @param data Data frame da filtrare.
#' @param colNames Nomi colonne da ispezionare.
#' @return Lista con due oggetti: righe filtrate con almeno un codice valido, righe escluse.
rowFilterByCodes <- function(data, colNames) {
  righe_valide <- logical(nrow(data))
  for (i in seq_len(nrow(data))) {
    for (col in colNames) {
      if (codeFilter(data[i, col])) {
        righe_valide[i] <- TRUE
        break
      }
    }
  }
  return(list(data[righe_valide, ], data[righe_valide == FALSE, ]))
}

# =====================================================
# STAGE 2 -Recupero dei casi SDO esclusi tramite codici ICD9 aggiuntivi
# =====================================================

#' clean_invalid_patologies
#'
#' @description Rimuove i codici non ICD9 o non presenti in "extra_codes.csv".
#' @param df Dataset.
#' @param columns Vettore nomi colonne.
#' @param extra_codes Codici ICD9 extra oltre il range 740-759.
#' @return Dataset con celle non valide svuotate.
clean_invalid_patologies <- function(df, columns, extra_codes) {
  for (i in 1:nrow(df)) {
    for (col in columns) {
      cell <- df[i, get(col)]
      if (!is.na(cell) & !(grepl("^74[0-9]|^75[0-9]", cell) | cell %in% extra_codes)) {
        df[i, (col) := ""]
      }
    }
    if (all(df[i, ..columns] == "")) {
      df[i, (columns) := ""]
    }
  }
  return(df)
}

#' extra_code_filter
#'
#' @description Controlla se un codice è tra quelli extra definiti.
#' @param value Codice da validare.
#' @return TRUE se codice è valido tra quelli extra, FALSE altrimenti.
extra_code_filter <- function(value) {
  if (is.na(value)) return(FALSE)
  value <- as.character(value)
  if (nchar(value) < 3) return(FALSE)
  return(value %in% extra_codes)
}

#' filter_rows_extra
#'
#' @description Filtra righe contenenti almeno un codice extra valido.
#' @param data Data frame.
#' @param col_names Colonne da esplorare.
#' @return Subset di righe contenenti almeno un codice extra valido.
filter_rows_extra <- function(data, col_names) {
  valid_rows <- logical(nrow(data))
  for (i in seq_len(nrow(data))) {
    for (col in col_names) {
      if (extra_code_filter(data[i, col])) {
        valid_rows[i] <- TRUE
        break
      }
    }
  }
  return(data[valid_rows, ])
}

# =====================================================
# STAGE 3 - Rimozione codici ICD9 minori
# =====================================================

#' clean_minor_codes
#'
#' @description Rimuove le righe contenenti solo codici minori.
#' @param df Dataset con patologie.
#' @param columns Colonne ICD9.
#' @param minor_codes Lista codici considerati minori.
#' @param extra_codes Codici ICD validi extra.
#' @return Dataset con righe filtrate.
clean_minor_codes <- function(df, columns, minor_codes) {
    df[columns] <- lapply(df[columns], function(x) as.character(x))
  df[columns] <- lapply(df[columns], function(x) replace_na(x, ""))
  rows_to_remove <- c()  
  
  for (i in 1:nrow(df)) {
    patol_values <- df[i, columns]  
    patol_values <- unlist(patol_values) 
    patol_values <- patol_values[patol_values != ""]  
        if (length(patol_values) == 0) {
      next
    }
        if (all(patol_values %in% minor_codes)) {
      rows_to_remove <- c(rows_to_remove, i)  # Rimuovi la riga
    }
        if (length(patol_values) == 1 && patol_values %in% minor_codes) {
      rows_to_remove <- c(rows_to_remove, i)  # Rimuovi la riga
    }
  }
    if (length(rows_to_remove) > 0) {
    df <- df[-rows_to_remove, ]
  }
  
  return(df)
}

#' clean_minor_codes_with_removed
#'
#' @description Variante della precedente, restituisce anche righe rimosse.
#' @return Lista con `cleaned_data` e `removed_data`.
clean_minor_codes_with_removed <- function(df, columns, minor_codes) {
    df[columns] <- lapply(df[columns], function(x) as.character(x))
  df[columns] <- lapply(df[columns], function(x) replace_na(x, ""))
  rows_to_remove <- c()  
  removed_rows <- list() 
  
  for (i in 1:nrow(df)) {
    patol_values <- df[i, columns]  
    patol_values <- unlist(patol_values)  
    patol_values <- patol_values[patol_values != ""]  
        if (length(patol_values) == 0) {
      next
    }
    
    if (all(patol_values %in% minor_codes)) {
      rows_to_remove <- c(rows_to_remove, i)  
      removed_rows <- append(removed_rows, list(df[i, ])) 
    }
        if (length(patol_values) == 1 && patol_values %in% minor_codes) {
      rows_to_remove <- c(rows_to_remove, i)  # Rimuovi la riga
      removed_rows <- append(removed_rows, list(df[i, ])) 
    }
  }
    if (length(rows_to_remove) > 0) {
    df <- df[-rows_to_remove, ]
  }
    removed_rows_df <- do.call(rbind, removed_rows)
  
  return(list(cleaned_data = df, removed_data = removed_rows_df))
}


#' verify_invalid_codes
#'
#' @description Individua codici errati o fuori range tra le patologie.
#' @return Lista di entry con codici invalidi.
verify_invalid_codes <- function(df, columns, minor_codes, extra_codes) {
  invalid_codes <- list()
  for (i in 1:nrow(df)) {
    patol_values <- df[i, columns]  
    patol_values <- unlist(patol_values) 
    print(paste("Riga", i, "- Valori:", paste(patol_values, collapse = ", ")))
    for (cell in patol_values) {
      if (is.na(cell) || cell == "") next  
      if (cell == "7750") {
        print(paste("Debug: Controllando codice", cell))
        print(paste("È in extra_codes?", cell %in% extra_codes))
      }
      if (!(cell %in% minor_codes)) {
        if (!grepl("^[7][4-5][0-9]", cell) & !(cell %in% extra_codes)) {
          invalid_codes <- append(invalid_codes, list(list(row = i, patol = cell)))
        }
      }
    }
  }
  
  return(invalid_codes)
}


# =====================================================
# STAGE 4 - Validazione combinata patologie + interventi
# =====================================================

#' update_validation_optimized
#'
#' @description Validazione combinata: per ogni codice ICD9 di patologia definito in `surgery_rules`, verifica se esiste lo stesso codice tra le colonne `interv1–interv11` del dataset SDO.
#' Se sì, imposta `validated = 1` e `validation_type = "1"` sulla riga corrispondente.
#' Utilizza un approccio vettorializzato efficiente per ridurre i loop nidificati di grandi dimensioni.
#' @param sdo_stage_3_nominors_export Data frame SDO già filtrato dalle patologie minori.
#' @param surgery_rules Data frame con almeno due colonne: `Icd` e `CodInterv`.
#' @return Data frame SDO con colonne `validated` e `validation_type` aggiornate per le righe corrispondenti.

update_validation_optimized <- function(sdo_stage_3_nominors_export, surgery_rules) {
  for (i in 1:nrow(surgery_rules)) {
    code_pat <- surgery_rules$Icd[i]
    code_interv <- surgery_rules$CodInterv[i]
        rows_with_pat <- which(sdo_stage_3_nominors_export$COD_PAT1 == code_pat | 
                             sdo_stage_3_nominors_export$patol2 == code_pat |
                             sdo_stage_3_nominors_export$patol3 == code_pat |
                             sdo_stage_3_nominors_export$patol4 == code_pat |
                             sdo_stage_3_nominors_export$patol5 == code_pat |
                             sdo_stage_3_nominors_export$patol6 == code_pat)
        for (j in rows_with_pat) {
      if (code_interv %in% sdo_stage_3_nominors_export[j, c("interv1", "interv2", "interv3", "interv4", "interv5", "interv6",
                                                            "interv7", "interv8", "interv9", "interv10", "interv11")]) {
        sdo_stage_3_nominors_export$validated[j] <- 1
        sdo_stage_3_nominors_export$validation_type[j] <- "1"
      }
    }
  }
  return(sdo_stage_3_nominors_export)
}


# =====================================================
# STAGE 5 - Validazione estesa da default
# =====================================================

#' validate_sdo
#'
#' @description Aggiunge validazione ai codici ICD9 all'interno del dataset SDO confrontandoli con un elenco di codici `valid_default`.
#' @param sdo_data Data frame SDO contenente colonne ICD9 da validare.
#' @param valid_default Data frame o tibble con colonna ICD9 valida (`icd9_column_valid`).
#' @param icd9_columns_sdo Vettore di nomi colonne ICD9 in `sdo_data`.
#' @param icd9_column_valid Nome della colonna in `valid_default` contenente codici ICD9 validi.
#' @return Data frame `sdo_data` arricchito con:
#'   - `validated = 1` per le righe contenenti almeno un codice valido;
#'   - `validation_type` aggiornato (aggiunge “2” o “|2” ai valori esistenti).

validate_sdo <- function(sdo_data, valid_default, icd9_columns_sdo, icd9_column_valid) {
  missing_columns <- setdiff(icd9_columns_sdo, names(sdo_data))
  if (length(missing_columns) > 0) {
    stop(paste("Errore: le seguenti colonne non esistono nel dataset SDO:", paste(missing_columns, collapse = ", ")))
  }
  
  if (!(icd9_column_valid %in% names(valid_default))) {
    stop(paste("Errore: la colonna ICD9 nel file valid_default non esiste. Colonne disponibili:", paste(names(valid_default), collapse = ", ")))
  }
    sdo_data[, validation_type := as.character(validation_type)]
    sdo_data[, any_match := Reduce(`|`, lapply(.SD, function(x) x %in% valid_default[[icd9_column_valid]])), 
           .SDcols = icd9_columns_sdo]
    sdo_data[any_match == TRUE, `:=`(
    validated = 1, 
    validation_type = ifelse( validation_type == "0", 
                              "2", 
                              paste0(validation_type, "|2")))  ]
    sdo_data[, any_match := NULL]
  
  return(sdo_data)
}

# =====================================================
# STAGE 6 - Validazione su CEDAP linkato
# =====================================================

#' validate_icd9_with_cedap
#'
#' @description Confronta codici ICD9 tra record SDO e record CEDAP linkati.
#' Se almeno un codice SDO è presente nel record corrispondente in CEDAP, imposta `validated = 1` e aggiunge codice “3” in `validation_type`.
#' @param sdo_data Data frame SDO contenente `cedap_linked` (indice di riga CEDAP).
#' @param cedap_data Data frame CEDAP contenente colonne ICD9 (`icd9_cedap_cols`).
#' @param icd9_sdo_cols Vettore colonne ICD9 in SDO.
#' @param icd9_cedap_cols Vettore colonne ICD9 in CEDAP.
#' @param linked_col Nome colonna in SDO che indica riga link in CEDAP.
#' @return Data frame SDO con validazione “3” aggiunta, se riscontro dei codici.

validate_icd9_with_cedap <- function(sdo_data, cedap_data, icd9_sdo_cols, icd9_cedap_cols, linked_col) {
  for (i in 1:nrow(sdo_data)) {
    cedap_row_num <- sdo_data[[linked_col]][i]
        sdo_icd9_codes <- unlist(sdo_data[i, ..icd9_sdo_cols])
        cedap_icd9_codes <- unlist(cedap_data[cedap_row_num, ..icd9_cedap_cols])
        sdo_icd9_codes <- sdo_icd9_codes[!is.na(sdo_icd9_codes)]
    cedap_icd9_codes <- cedap_icd9_codes[!is.na(cedap_icd9_codes)]
        match_found <- any(sdo_icd9_codes %in% cedap_icd9_codes)
        if (match_found) {
      sdo_data[i, validated := 1]  # Segnare come validato
            current_validation <- sdo_data$validation_type[i]
      if (current_validation == "0") {
        sdo_data[i, validation_type := "3"]
      } else if (!grepl("\\b3\\b", current_validation)) {  # Evitiamo duplicati
        sdo_data[i, validation_type := paste0(current_validation, "|3")]
      }
    }
  }
  return(sdo_data)
}


# =====================================================
# STAGE 7 - Duplicati tra pazienti con stesso PROG_PAZ
# =====================================================

#' validate_duplicates
#'
#' @description
#' Identifica i pazienti con stesso `PROG_PAZ` presenti in più righe e verifica, all’interno delle loro rispettive righe, 
#' se esistono codici ICD9 identici tra le colonne specificate (`icd9SearchCols`). I codici minori e "V3000" vengono ignorati. 
#' Se viene rilevato almeno un codice ICD9 ripetuto tra righe dello stesso paziente, la funzione aggiorna il campo 
#' `validation_type`, impostandolo a `"4"` o aggiungendo `"|4"` se già presente un altro codice di validazione.
#'
#' @param dataset Data frame SDO contenente i dati dei pazienti.
#' @param minor_codes Vettore di codici ICD9 da escludere dal controllo (malformazioni minori).
#' @param icd9SearchCols Vettore di nomi di colonne da ispezionare per la presenza di codici ICD9 duplicati.
#'
#' @return Il data frame originale con la colonna `validation_type` aggiornata per le righe che presentano duplicazioni 
#' di codici ICD9 all’interno dello stesso `PROG_PAZ`.
validate_duplicates <- function(dataset, minor_codes, icd9SearchCols) {
  # 1. Aggiungere ID numerico per riferimento
  dataset <- dataset %>%
    mutate(ID = row_number())
  
  # 2. Rimuovere i codici "V3000" e i codici delle malformazioni minori
  dataset[icd9SearchCols] <- lapply(dataset[icd9SearchCols], function(x) {
    x[x %in% c("V3000", minor_codes)] <- ""
    return(x)
  })
  
  # 3. Identificare i pazienti ripetuti
  duplicated_patients <- dataset %>%
    group_by(PROG_PAZ) %>%
    filter(n() > 1) %>%
    pull(PROG_PAZ) %>%
    unique()
  
  # 4. Iterare su ciascun paziente ripetuto
  for (patient in duplicated_patients) {
    
    # 4a. Seleziona solo le righe con lo stesso PROG_PAZ
    tmpProgPaz <- dataset %>% filter(PROG_PAZ == patient)
    
    # 4b. Cerca malfo ripetute nelle colonne di patologia
    for (i in 1:nrow(tmpProgPaz)) {
      for (j in seq_along(icd9SearchCols)) {
        current_value <- tmpProgPaz[i, icd9SearchCols[j], drop = TRUE]
        
        # Se il valore è valido (escludendo "V3000" e codici minori)
        if (!is.na(current_value) && current_value != "" && !current_value %in% minor_codes) {
          
          # 4c. Controlla se esiste un altro valore identico nella stessa matrice
          for (k in 1:nrow(tmpProgPaz)) {
            for (l in seq_along(icd9SearchCols)) {
              if (!is.na(tmpProgPaz[k, icd9SearchCols[l], drop = TRUE]) &&
                  tmpProgPaz[k, icd9SearchCols[l], drop = TRUE] == current_value && i != k) {  
                
                # 4d. Se lo trova, aggiorna solo validation_type
                dataset$validation_type[dataset$ID == tmpProgPaz$ID[k]] <- ifelse(
                  dataset$validation_type[dataset$ID == tmpProgPaz$ID[k]] == "0", "4",
                  ifelse(!grepl("4", dataset$validation_type[dataset$ID == tmpProgPaz$ID[k]]),
                         paste0(dataset$validation_type[dataset$ID == tmpProgPaz$ID[k]], "|4"),
                         dataset$validation_type[dataset$ID == tmpProgPaz$ID[k]])
                )
                
                dataset$validation_type[dataset$ID == tmpProgPaz$ID[i]] <- ifelse(
                  dataset$validation_type[dataset$ID == tmpProgPaz$ID[i]] == "0", "4",
                  ifelse(!grepl("4", dataset$validation_type[dataset$ID == tmpProgPaz$ID[i]]),
                         paste0(dataset$validation_type[dataset$ID == tmpProgPaz$ID[i]], "|4"),
                         dataset$validation_type[dataset$ID == tmpProgPaz$ID[i]])
                )
              }
            }
          }
        }
      }
    }
  }
  
  # 5. Rimuovere la colonna ID temporanea
  dataset <- dataset %>% select(-ID)
  
  return(dataset)
}


# =====================================================
# STAGE 8 - CALCOLO SDS E CLASSIFICAZIONE VARIABILI
# VERSIONE 1.0
# Calcola lo SDS (Standard Deviation Score) della circonferenza cranica
# utilizzando i parametri INES (modello LMS). Aggiunge inoltre informazioni
# sulle classi delle variabili in CEDAP/SDO.
# =====================================================

#' get_class
#'
#' @description Restituisce la classe R di una variabile presente in un dataframe.
#' @param var_name Nome della variabile (stringa).
#' @param df Data frame contenente la variabile.
#' @return Stringa contenente la classe della variabile (es. "character", "numeric"), oppure `NA_character_` se non esiste nel dataframe.
get_class <- function(var_name, df) {
  if (!is.na(var_name) && var_name %in% names(df)) {
    return(paste(class(df[[var_name]]), collapse = "|"))
  } else {
    return(NA_character_)
  }
}

#' SDS
#'
#' @description Calcola la SDS (Standard Deviation Score) della circonferenza cranica
#' secondo il modello LMS, sulla base dei parametri INES.
#' @param y Valore osservato (numeric).
#' @param pEG Età gestazionale (intero).
#' @param pSesso Sesso del neonato: "M" o "F".
#' @param pPrimogenito Valore primogenitura: "SI", "NO", 1 o 0.
#' @param ines_table Tabella con i parametri INES: colonne `EG`, `sesso`, `primogenito`, `M`, `L`, `S`.
#' @return Valore SDS calcolato (numeric), oppure `NA` se i parametri non sono disponibili o input non validi.
SDS <- function(y, pEG, pSesso, pPrimogenito, ines_table) {
  
  if (is.na(pEG) | is.na(pSesso) | is.na(pPrimogenito) | is.na(y)) {
    return(NA)
  }
  
  pSesso <- as.character(pSesso)
  pPrimogenito <- ifelse(pPrimogenito %in% c("SI", "NO"), pPrimogenito,
                         ifelse(pPrimogenito %in% c(1, "1"), "SI", "NO"))
  
  # Estrai i parametri
  parametri_riga <- ines_table[EG == pEG & sesso == pSesso & primogenito == pPrimogenito]
  
  if (nrow(parametri_riga) == 0) {
    return(NA)
  }
  
  M <- parametri_riga$M
  L <- parametri_riga$L
  S <- parametri_riga$S
  
  sds_value <- ((y / M)^L - 1) / (L * S)
  
  return(sds_value)
}


# =====================================================
# STAGE 8 - SCORE MALFORMAZIONI
# VERSIONE 1.0
# Calcola uno score sulla base del tipo di validazione attuata sui codici malformativi
# =====================================================

#' scoreCalculation
#'
#' @description Calcola uno score sommando pesi associati agli ICD9 (da 1 a 4) in una riga.
#' @param mRow Vettore numerico contenente i codici di validazione (valori ammessi: 1–4).
#' @return Score numerico (intero) ottenuto secondo la seguente logica:
#'         codice 1 o 2 = +4 punti, codice 3 = +1 punto, codice 4 = +2 punti.
scoreCalculation <- function(mRow){
  s=0
  for(i in 1:length(mRow)){
    if(!is.na(mRow[i])) {
      if(mRow[i]==1) s=s+4
      if(mRow[i]==2) s=s+4
      if(mRow[i]==3) s=s+1
      if(mRow[i]==4) s=s+2
    }
    else s = s
  }
  return(s)
}

# =====================================================
# STAGE 10 - Analisi celle con valori uguali per escludere da collapse
# VERSIONE 1.0
# Estrae la prima data valida da stringhe concatenate e verifica se tutte le date
# fornite per un campo sono uguali.
# =====================================================

#' estrai_data_valida
#'
#' @description Estrae e normalizza la prima data valida da una stringa concatenata con delimitatore "|".
#' @param x Vettore carattere contenente date concatenate (es. "2023-05-10|NA|10/05/2023").
#' @return Vettore `Date` contenente la prima data valida convertita; `NA` se nessuna è valida.
estrai_data_valida <- function(x) {
  x_clean <- sapply(strsplit(x, "\\|"), function(vec) {
    val <- vec[which(vec != "NA")[1]]
    if (is.na(val)) return(NA)
    return(val)
  })
  x_date <- suppressWarnings(as.Date(x_clean, format = "%Y-%m-%d"))
  idx_na <- is.na(x_date)
  if (any(idx_na)) {
    x_date[idx_na] <- suppressWarnings(as.Date(x_clean[idx_na], format = "%d/%m/%Y"))
  }
  return(x_date)
}

#' sottostringhe_uguali
#'
#' @description Verifica se tutte le sottostringhe (separate da "|") di una stringa sono identiche.
#' @param x Stringa contenente sottostringhe separate da "|".
#' @return `TRUE` se tutte le sottostringhe sono uguali, `FALSE` altrimenti.
sottostringhe_uguali <- function(x) {
  parts <- unlist(strsplit(x, "\\|"))
  all(parts == parts[1])
}

# =====================================================
# STAGE 11 - MAPPING ICD9
# VERSIONE 1.0
# Gestione di codici ICD9 concatenati, padding a 6 cifre, mapping verso ICD10 e descrizioni.
# =====================================================

#' get_icd10_code
#'
#' @description Converte una stringa di codici ICD9 (separati da "|") in codici ICD10 utilizzando la tabella di conversione in tables.
#' @param code_str Stringa con codici ICD9 concatenati (es. "74310|75650").
#' @return Stringa con codici ICD10 corrispondenti ai valori ICD9: "74310|75650", sempre separati da "|".
get_icd10_code <- function(code_str) {
  if (is.na(code_str) || code_str == "") return(NA)
  codes <- str_split(code_str, "\\|")[[1]]
  labels <- sapply(codes, function(codice) {
    codice <- str_trim(codice)
    codice_padded <- str_pad(codice, width = 6, pad = "0", side = "right")
    label <- icd_conversion_table$ICD10[icd_conversion_table$ICD9CM == codice_padded]
    if (length(label) > 0) label else "NA"
  })
  paste(labels, collapse = "|")
}

#' get_icd10_description
#'
#' @description Restituisce le descrizioni testuali (label) ICD10 corrispondenti ai codici ICD9 forniti, tramite conversione.
#' @param code_str Stringa di codici ICD9 (separati da "|").
#' @return Stringa con label ICD10 corrispondenti, separate da "|".
get_icd10_description <- function(code_str) {
  if (is.na(code_str) || code_str == "") return(NA)
  codes <- str_split(code_str, "\\|")[[1]]
  labels <- sapply(codes, function(codice) {
    codice <- str_trim(codice)
    codice_padded <- str_pad(codice, width = 6, pad = "0", side = "right")
    label <- icd_conversion_table$Descrizione[icd_conversion_table$ICD9CM == codice_padded]
    if (length(label) > 0) label else "NA"
  })
  paste(labels, collapse = "|")
}

#' map_multi_codes_interv
#'
#' @description Mappa i codici di intervento (concatenati con "|") alle rispettive descrizioni da un dizionario.
#' @param code_str Stringa di codici di intervento.
#' @param dict Data frame dizionario con colonne `codice` e `descrizione`.
#' @return Stringa contenente le descrizioni dei codici, concatenate da "|".
map_multi_codes_interv <- function(code_str, dict) {
  if (is.na(code_str) || code_str == "") return(NA)
  codes <- str_split(code_str, "\\|")[[1]]
  labels <- sapply(codes, function(codice) {
    codice <- str_trim(codice)
    label <- dict$descrizione[dict$codice == codice]
    if (length(label) > 0) label else "NA"
  })
  paste(labels, collapse = "|")
}

#' map_multi_codes_pat
#'
#' @description Mappa codici ICD9 (concatenati con "|") alle descrizioni,
#' applicando padding a 6 cifre se necessario.
#' @param code_str Stringa contenente codici di patologia.
#' @param dict Dizionario con colonne `codice` e `descrizione`.
#' @return Stringa con descrizioni associate, separate da "|".
map_multi_codes_pat <- function(code_str, dict) {
  if (is.na(code_str) || code_str == "") return(NA)
  codes <- str_split(code_str, "\\|")[[1]]
  labels <- sapply(codes, function(codice) {
    codice <- str_trim(codice)
    codice_padded <- pad_to_6_digits(codice)
    label <- dict$descrizione[dict$codice == codice_padded]
    if (length(label) > 0) label else "NA"
  })
  paste(labels, collapse = "|")
}

#' pad_to_6_digits
#'
#' @description Normalizza un codice ICD9 portandolo a 6 caratteri, aggiungendo zeri a destra (aggiunge gli zeri finali per uniformare tutti i codici a 6 cifre) .
#' @param code Codice ICD9 da formattare.
#' @return Codice ICD9 con padding fino a 6 cifre (stringa).
pad_to_6_digits <- function(code) {
  code <- as.character(code)
  code <- str_replace_all(code, "\\s+", "")
  n <- nchar(code)
  ifelse(n == 5, paste0(code, "0"),
         ifelse(n == 4, paste0(code, "00"), code))
}

# =====================================================
# CONTEGGIO MALFORMAZIONI UNICHE PER CENTRO
# 
# Conta, per ciascun centro, il numero di codici di malformazione unici,
# considerando tutte le righe e tutte le colonne di patologia.
# =====================================================

#' conta_malformazioni_per_centro
#'
#' @description Conta il numero di malformazioni uniche per ogni centro (`COD_PRES`), esplodendo codici concatenati da colonne patologia.
#' @param input_df Data frame con colonne `COD_PRES`, `COD_PAT1`, `patol2`...`patol6` e `COD_PAT1_label`.
#' @return Data frame con: `COD_PRES`, numero di malformazioni uniche, e descrizioni concatenate.
conta_malformazioni_per_centro <- function(input_df) {
  patol_cols <- c("COD_PAT1", "patol2", "patol3", "patol4", "patol5", "patol6")
  
  codici_label <- input_df %>%
    select(COD_PAT1, COD_PAT1_label) %>%
    filter(!is.na(COD_PAT1)) %>%
    distinct(COD_PAT1, COD_PAT1_label)
  
  d_long <- input_df %>%
    mutate(row_id = row_number()) %>%
    separate_rows(COD_PRES, sep = "\\|") %>%
    pivot_longer(cols = all_of(patol_cols), names_to = "patol_tipo", values_to = "malfo") %>%
    filter(!is.na(malfo) & malfo != "")
  
  d_distinct <- d_long %>%
    distinct(COD_PRES, malfo)
  
  d_final <- d_distinct %>%
    left_join(codici_label, by = c("malfo" = "COD_PAT1")) %>%
    group_by(COD_PRES) %>%
    summarise(
      tot_malformazioni = n_distinct(malfo),
      malfo_label = paste(sort(unique(COD_PAT1_label[!is.na(COD_PAT1_label)])), collapse = " | ")
    ) %>%
    arrange(desc(tot_malformazioni))
  
  return(d_final)
}

