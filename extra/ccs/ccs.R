# 1. **Verifica con estrazioni campioni:**----
selected_columns <- c("dt_amm", "dt_dim", "COD_PAT1", "patol2", "patol3", "patol4", 
                      "patol5", "patol6", "interv1", "interv2", "interv3", "interv4", 
                      "interv5", "interv6", "interv7", "interv8", "interv9", "interv10", 
                      "interv11", "cedap_linked", "SEX")

# Filtra il dataset per mantenere solo le colonne desiderate
sdo_valid_filtered <- sdo_valid[, selected_columns]
sdo_invalid_filtered <- sdo_invalid[, selected_columns]

# Estrazione casuale da valid e invalid
set.seed(123)  
sample_scan_valid <- sdo_valid_filtered[sample(nrow(sdo_valid_filtered), min(50, nrow(sdo_valid_filtered))), ]
sample_scan_invalid <- sdo_invalid_filtered[sample(nrow(sdo_invalid_filtered), min(50, nrow(sdo_invalid_filtered))), ]

# Aggiungi etichetta
sample_scan_valid$etichetta <- "valid"
sample_scan_invalid$etichetta <- "invalid"

# Unisci i campioni validi e invalidi
sample_step1 <- rbind(sample_scan_valid, sample_scan_invalid)

# Salva il campione 1
write.csv(sample_step1, "/home/imer/works/algo_sdo/export/ccs/sample_step1.csv", row.names = FALSE)





# 2. **Estrazione 2: ccs scan2.R**----


# Estrazione casuale di 50 righe da sdo_extra_valid (dato che sono tutte righe "extra")
set.seed(123)
sample_complete <- sdo_extra_valid[sample(nrow(sdo_extra_valid), min(50, nrow(sdo_extra_valid))), ]

# Salva il campione
write.csv(sample_complete, "/home/imer/works/algo_sdo/export/ccs/sample_step2.csv", row.names = FALSE)

# Mostra le prime righe del risultato per controllo
head(sample_complete)

#NON E' STATO FATTO IL CCS PER LE VALID STAGE 1 PERCHE' SONO STATE CONTROLLATE NEL PIMK STAGE DI QUESTA VERIFICA.
#CI CONCENTRIAMO SOLO SULLE EXTRA RECUPERATE DAL CSV EXTRA_CODES 





# 3. **Estrazione 3: ccs nominors_export**----

minor_codes <- read_delim("stage_3/minor_codes.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
minor_codes <- minor_codes$ICD9

sdo_stage_2_complete <- read_csv2("/home/imer/works/algo_sdo/export/sdo_stage_2_valide_all_export.csv", col_types = cols(.default = "c"))

result <- clean_minor_codes_with_removed(sdo_stage_2_complete, icd9SearchCols, minor_codes)
sdo_stage_3_complete <- result$cleaned_data
removed_rows_onlyminors <- result$removed_data  
removed_rows_onlyminors$etichetta <- "excluded"
sdo_stage_3_complete$etichetta <- "included"
write.csv2(sdo_stage_3_complete, "/home/imer/works/algo_sdo/export/sdo_stage_3_valide_all_export.csv", row.names = FALSE)
write.csv2(removed_rows_onlyminors, "/home/imer/works/algo_sdo/export/removed_rows_onlyminors.csv", row.names = FALSE)


# 4. **Estrazione 4: ccs (validation 0;1)**----
selected_columns2 <- c("PROG_PAZ","dt_amm", "dt_dim", "COD_PAT1", "patol2", "patol3", "patol4", 
                       "patol5", "patol6", "interv1", "interv2", "interv3", "interv4", 
                       "interv5", "interv6", "interv7", "interv8", "interv9", "interv10", 
                       "interv11", "cedap_linked", "SEX", "validated", "validation_type")

# Filtra le righe con validated == 0 e validated == 1
sdo_validated_0 <- sdo_stage_4_nominors_export[sdo_stage_4_nominors_export$validated == 0, selected_columns2]
sdo_validated_1 <- sdo_stage_4_nominors_export[sdo_stage_4_nominors_export$validated == 1, selected_columns2]

set.seed(123)  
sample_stage4_0 <- sdo_validated_0[sample(nrow(sdo_validated_0), min(50, nrow(sdo_validated_0))), ]
sample_stage4_1 <- sdo_validated_1[sample(nrow(sdo_validated_1), min(50, nrow(sdo_validated_1))), ]

# Aggiungi etichetta
sample_stage4_0$etichetta <- "0"
sample_stage4_1$etichetta <- "1"

# Unisci i campioni
sample_step4 <- rbind(sample_stage4_0, sample_stage4_1)

# Salva il campione 4
write.csv(sample_step4, "/home/imer/works/algo_sdo/export/ccs/sample_step4.csv", row.names = FALSE)


# 5. **Estrazione 5: ccs (validation 1;2;1|2)**
# Calcola la frequenza relativa per validation_type (solo per "1", "2", "1|2")
freq_validation_type <- table(sdo_stage_5_validated$validation_type) / nrow(sdo_stage_5_validated)

# Estrai il campione per validation_type == "2"
sample_stage5_2 <- sdo_stage_5_validated[validation_type == "2", ..selected_columns2]
sample_stage5_2 <- sample_stage5_2[sample(nrow(sample_stage5_2), size = min(100, nrow(sample_stage5_2))), ]

# Estrai il campione per validation_type == "1|2"
sample_stage5_12 <- sdo_stage_5_validated[validation_type == "1|2", ..selected_columns2]
sample_stage5_12 <- sample_stage5_12[sample(nrow(sample_stage5_12), size = min(100, nrow(sample_stage5_12))), ]

# Campionamento ponderato per validation_type == "1"
sample_stage5_1 <- sdo_stage_5_validated[validation_type == "1", ..selected_columns2]
sample_stage5_1 <- sample_stage5_1[sample(nrow(sample_stage5_1), size = min(100, nrow(sample_stage5_1))), ]

# Unisci i campioni
final_sample_5 <- rbind(sample_stage5_1, sample_stage5_2, sample_stage5_12)

# Aggiungi etichetta
final_sample_5$etichetta <- final_sample_5$validation_type  # Usa il tipo di validazione per etichetta

# Salva il campione 5
write.csv(final_sample_5, "/home/imer/works/algo_sdo/export/ccs/sample_step5.csv", row.names = FALSE)




# 6. **Estrazione 6: val.1|2|3**
# Calcola la frequenza relativa per validation_type (escludendo '0')
freq_validation_type <- table(sdo_stage_6_validated$validation_type) / 
  sum(table(sdo_stage_6_validated$validation_type)[c("1", "1|2", "1|2|3", "1|3", "2", "2|3", "3")])

# Determina il numero di righe da estrarre per ogni gruppo (proporzionato e max 100 totali)
total_sample_size <- 100
sample_sizes <- round(freq_validation_type * total_sample_size)

# Estrai i campioni in base ai pesi calcolati
sample_stage6_1 <- sdo_stage_6_validated[validation_type == "1", ..selected_columns2]
sample_stage6_1 <- sample_stage6_1[sample(nrow(sample_stage6_1), size = min(sample_sizes["1"], nrow(sample_stage6_1))), ]

sample_stage6_12 <- sdo_stage_6_validated[validation_type == "1|2", ..selected_columns2]
sample_stage6_12 <- sample_stage6_12[sample(nrow(sample_stage6_12), size = min(sample_sizes["1|2"], nrow(sample_stage6_12))), ]

sample_stage6_123 <- sdo_stage_6_validated[validation_type == "1|2|3", ..selected_columns2]
sample_stage6_123 <- sample_stage6_123[sample(nrow(sample_stage6_123), size = min(sample_sizes["1|2|3"], nrow(sample_stage6_123))), ]

sample_stage6_13 <- sdo_stage_6_validated[validation_type == "1|3", ..selected_columns2]
sample_stage6_13 <- sample_stage6_13[sample(nrow(sample_stage6_13), size = min(sample_sizes["1|3"], nrow(sample_stage6_13))), ]

sample_stage6_2 <- sdo_stage_6_validated[validation_type == "2", ..selected_columns2]
sample_stage6_2 <- sample_stage6_2[sample(nrow(sample_stage6_2), size = min(sample_sizes["2"], nrow(sample_stage6_2))), ]

sample_stage6_23 <- sdo_stage_6_validated[validation_type == "2|3", ..selected_columns2]
sample_stage6_23 <- sample_stage6_23[sample(nrow(sample_stage6_23), size = min(sample_sizes["2|3"], nrow(sample_stage6_23))), ]

sample_stage6_3 <- sdo_stage_6_validated[validation_type == "3", ..selected_columns2]
sample_stage6_3 <- sample_stage6_3[sample(nrow(sample_stage6_3), size = min(sample_sizes["3"], nrow(sample_stage6_3))), ]

# Unisci i campioni
final_sample_6 <- rbind(sample_stage6_1, sample_stage6_12, sample_stage6_123, 
                        sample_stage6_13, sample_stage6_2, sample_stage6_23, sample_stage6_3)
final_sample_6$etichetta <- final_sample_6$validation_type  # Usa il tipo di validazione per etichetta
write.csv(final_sample_6, "/home/imer/works/algo_sdo/export/ccs/sample_step6.csv", row.names = FALSE)




# 7. **Estrazione 7: ccs (stage8)**

# Carica il file cedap_plus_2023
cedap_plus_2023 <- fread(paste0(cedapDir, "/", cedapFileName))

# Aggiungi una colonna 'row_num' per numerare le righe
cedap_plus_2023 <- cedap_plus_2023 %>%
  mutate(row_num = row_number())

# Seleziona le colonne necessarie da cedap_plus_2023 per le malformazioni diagnosticate
cedap_selected_columns <- cedap_plus_2023 %>%
  select(row_num, Malformazione_diagnosticata_1, Malformazione_diagnosticata_2, Malformazione_diagnosticata_3)

# Unisci sdo_stage8 con cedap_selected_columns
sdo_stage8 <- merge(sdo_stage8, cedap_selected_columns, by.x = "cedap_linked", by.y = "row_num", all.x = TRUE)

# 7. **Estrazione 7: ccs (stage8)**

# Seleziona le colonne necessarie da sdo_stage8 per il campionamento
selected_columns3 = c("PROG_PAZ", "dt_amm", "dt_dim", "COD_PAT1", "patol2", "patol3", "patol4", 
                     "patol5", "patol6", "interv1", "interv2", "interv3", "interv4", 
                     "interv5", "interv6", "interv7", "interv8", "interv9", "interv10", 
                     "interv11", "cedap_linked", "SEX", "validated", "validation_type", 
                     "eta_gestazionale", "CIRCONFERENZA_CRANICA", "CONCEPIMENTI_PRECEDENTI", 
                     "sds_cc", "violazione_filtro", "malformazione_tipo", 
                     "Malformazione_diagnosticata_1", "Malformazione_diagnosticata_2", "Malformazione_diagnosticata_3")

# Frequenze relative solo per i gruppi da campionare (escludendo '0')
freq_validation_type <- table(sdo_stage8$validation_type) / 
  sum(table(sdo_stage8$validation_type)[!names(table(sdo_stage8$validation_type)) %in% "0"])

# Determina il numero di righe da estrarre per ogni gruppo (proporzionato e max 100 totali)
total_sample_size <- 100
sample_sizes <- round(freq_validation_type * total_sample_size)

# Imposta il seme per la riproducibilità
set.seed(123)

# Inizializza il campione finale
final_sample_stage8 <- data.table()

# Estrazione proporzionale per ogni gruppo
for (group in names(sample_sizes)) {
  sample_group <- sdo_stage8[validation_type == group, ..selected_columns3]
  
  # Se il gruppo ha meno righe del previsto, prendi tutte; altrimenti campiona in base al calcolo proporzionale
  sample_size <- min(sample_sizes[group], nrow(sample_group))
  sample_group <- sample_group[sample(nrow(sample_group), size = sample_size), ]
  
  # Unisci al campione finale
  final_sample_stage8 <- rbind(final_sample_stage8, sample_group)
}

# Aggiungi etichetta per "escluso" per il gruppo "0"
final_sample_stage8$etichetta <- ifelse(final_sample_stage8$validation_type == "0", "escluso", final_sample_stage8$validation_type)

# Salva il campione finale
write.csv(final_sample_stage8, "/home/imer/works/algo_sdo/export/ccs/sample_step8.csv", row.names = FALSE)




# 8. **Estrazione 8: ccs (stage7_sottomatrici_duplicati)**
print_duplicates_submatrices <- function(dataset, icd9SearchCols, num_samples = 20, seed = 123) {
  # Imposta il seme per la riproducibilità
  set.seed(seed)
  
  # Estrai i pazienti duplicati (quelli che hanno più di una riga)
  duplicated_patients <- dataset %>%
    group_by(PROG_PAZ) %>%
    filter(n() > 1) %>%
    pull(PROG_PAZ) %>%
    unique()
  
  # Campiona casualmente i duplicati
  sampled_patients <- sample(duplicated_patients, min(num_samples, length(duplicated_patients)))
  
  # Per ogni paziente campionato, stampa la sottomatrice
  for (patient in sampled_patients) {
    # Seleziona tutte le righe per il paziente
    patient_data <- dataset %>% filter(PROG_PAZ == patient)
    
    # Seleziona solo le colonne di interesse (aggiungiamo anche validation_type)
    patient_data <- patient_data %>% select(PROG_PAZ, validation_type, all_of(icd9SearchCols))
    
    # Stampa le righe per quel paziente
    cat("\nSottomatrice per paziente PROG_PAZ:", patient, "\n")
    print(patient_data)
  }
}

input_file <- paste0(exportDir, "/sdo_stage_6_validated_3_export.csv")
minor_codes_file <- paste0(stage_3Dir, "/minor_codes.csv")
minor_codes <- fread(minor_codes_file, header = FALSE)$V1
sdo_stage_6 <- fread(input_file, data.table = FALSE)
sdo_stage_6$PROG_PAZ <- as.numeric(sdo_stage_6$PROG_PAZ)

sdo_stage_7 <- validate_duplicates(sdo_stage_6, minor_codes, icd9SearchCols)

print_duplicates_submatrices(sdo_stage_7, icd9SearchCols, num_samples = 20, seed = 123)

output_file <- "/home/imer/works/algo_sdo/export/sdo_stage_7_submatrices.csv"
fwrite(sdo_stage_7, output_file, row.names = FALSE)




# ---- SCRIPT DI VERIFICA AUTOMATIZZATA ----

library(data.table)

# Caricamento dei campioni
sample_step1 <- fread("/home/imer/works/algo_sdo/export/ccs/sample_step1.csv")
sample_step2 <- fread("/home/imer/works/algo_sdo/export/ccs/sample_step2.csv")
sample_step3 <- fread("/home/imer/works/algo_sdo/export/ccs/sample_step3.csv")

# Codici malformazioni 740-759
malfoCodes <- as.character(740:759)

# Funzione di verifica codici ICD9
check_icd9 <- function(row, cols) {
  any(substr(as.character(row[cols]), 1, 3) %in% malfoCodes)
}

# ---- VERIFICA CAMPIONE 1 ----
check_icd9 <- function(row, cols, malfoCodes) {
  # Estrai i codici e pulisci eventuali spazi
  codes <- as.character(unlist(row[cols]))
  codes <- trimws(codes)  # Rimuove eventuali spazi vuoti
  codes <- codes[!is.na(codes)]  # Rimuovi eventuali NA
  
  # Estrai i primi 3 caratteri dei codici
  primi_tre <- substr(codes, 1, 3)
  
  # Verifica se almeno uno dei codici è valido (740-759)
  valid_code_present <- any(primi_tre %in% malfoCodes)
  
  # Logica di controllo in base all'etichetta
  if (row["etichetta"] == "valid") {
    return(valid_code_present)
  } else {
    return(!valid_code_present)
  }
}

# Applicazione del controllo
sample_step1$check <- apply(sample_step1, 1, check_icd9, 
                            cols = c("COD_PAT1", "patol2", "patol3", "patol4", "patol5", "patol6"), 
                            malfoCodes = as.character(740:759))

# Righe errate nel Campione 1
error_sample1 <- sample_step1[!sample_step1$check, ]
cat("\nCampione 1 - Righe errate:\n")
print(error_sample1)

# Riepilogo
cat("\nCampione 1 - Righe corrette:", sum(sample_step1$check), "su", nrow(sample_step1), "\n")



# ---- VERIFICA CAMPIONE 2 ----

# Carica il file extra_codes
extra_codes <- read_csv2("/home/imer/works/algo_sdo/stage_2/extra_codes.csv", col_names = FALSE)
extra_codes <- as.character(extra_codes[[1]])

# Estrazione casuale di 50 righe da sdo_extra_valid
set.seed(123)
sample_complete <- sdo_extra_valid[sample(nrow(sdo_extra_valid), min(50, nrow(sdo_extra_valid))), ]

# Funzione per verificare se almeno un codice nelle colonne COD_PAT1 a patol6 è presente nei codici extra
check_codes_in_sample <- function(row, extra_codes) {
  # Seleziona tutte le colonne di patologia da COD_PAT1 a patol6
  pathologies <- row[c("COD_PAT1", "patol2", "patol3", "patol4", "patol5", "patol6")]
  
  # Verifica se almeno uno dei codici nelle patologie è presente nei codici extra
  return(any(pathologies %in% extra_codes, na.rm = TRUE))
}

# Verifica per ogni riga del campione
valid_rows <- logical(nrow(sample_complete))
for (i in 1:nrow(sample_complete)) {
  valid_rows[i] <- check_codes_in_sample(sample_complete[i, ], extra_codes)
}

# Mostra quante righe hanno almeno un codice valido extra
valid_rows_count <- sum(valid_rows)
cat("Numero di righe con almeno un codice extra trovato:", valid_rows_count, "\n")

# Puoi anche visualizzare le righe che non hanno codici extra
invalid_rows <- sample_complete[!valid_rows, ]
cat("Righe senza codici extra:\n")
print(invalid_rows)




# ---- VERIFICA CAMPIONE 3 ----

set.seed(123)

# ---- Estrazione casuale dalle righe escluse ----
sample_excluded <- removed_rows_onlyminors[sample(nrow(removed_rows_onlyminors), min(50, nrow(removed_rows_onlyminors))), ]

# ---- Funzione di verifica che almeno uno dei codici nelle colonne di patologie è un codice minore ----
check_codes_in_sample <- function(row, minor_codes) {
  pathologies <- row[c("COD_PAT1", "patol2", "patol3", "patol4", "patol5", "patol6")]
  
  # Verifica se almeno uno dei codici nelle patologie è presente nei codici minori
  return(any(pathologies %in% minor_codes, na.rm = TRUE))
}

# ---- Verifica per ogni riga nel campione degli esclusi ----
sample_excluded$check <- apply(sample_excluded, 1, function(row) check_codes_in_sample(row, minor_codes))

# ---- Righe errate nel campione (escluse che non hanno codici minori) ----
error_sample_excluded <- sample_excluded[!sample_excluded$check, ]

# Mostra le righe errate
cat("Righe errate nel campione degli esclusi:\n")
print(error_sample_excluded)

# Riepilogo campione degli esclusi
cat("\nCampione degli esclusi - Righe corrette:", sum(sample_excluded$check), "su", nrow(sample_excluded), "\n")

# ---- Salvataggio del campione degli esclusi ----
write.csv(sample_excluded, "/home/imer/works/algo_sdo/export/ccs/sample_excluded.csv", row.names = FALSE)


