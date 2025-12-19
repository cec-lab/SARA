rm(list=ls())

# CONTROLLO VARIABILE COMPILATA DURANTE REVISIONE CLINICA
# (revcode=0 se riga da escludere; 1 se modificata; 2 se accettata)

# === LOAD CONFIG ===
source("/home/imer/works/algo_sdo/config.R", echo = TRUE)
source("/home/imer/works/algo_sdo/functions.R", echo = TRUE)
export_path <- "/home/imer/works/algo_sdo/export/revcode_analisi"
dir.create(export_path, showWarnings = FALSE, recursive = TRUE)

# === LOAD DATA ===
input_df_revcode <- read_csv2(paste0(stage_12Dir, "/sdo_stage_11b_clinical_rev_export_final.csv"))
input_df <- read_csv2(paste0(exportDir, "/sdo_stage_11b_clinical_rev_export.csv"))

# Colonne usate (definite in config.R)
cols_revcode_subset <- c(patology_code_cols_icd10, patology_label_cols_icd9, intervention_label_cols, "revcode")

# Subset revcode con indice riga
revcode_subset <- input_df_revcode[, cols_revcode_subset] %>%
  mutate(..ROWNUMBER.. = row_number())

# Dataset originale con indice riga
original_df <- input_df %>%
  mutate(..ROWNUMBER.. = row_number())


# =====================
# --- SEZIONE REVCODE 0 (RIGHE ESCLUSE) ---
# =====================

revcode_0 <- revcode_subset %>% filter(revcode == 0)

# Colonne codice + etichette da includere nel confronto riga per riga
cols_codici_label <- c(patology_code_cols_icd10, patology_label_cols_icd9, intervention_label_cols)

# Estrai solo le colonne di interesse
revcode_clean <- revcode_0 %>%
  select(all_of(cols_codici_label))

# Per ogni riga, crea una rappresentazione testuale compatta con solo le celle piene
revcode_righe_text <- revcode_clean %>%
  pmap_chr(function(...) {
    valori <- list(...)
    # Rimuove NA e stringhe vuote
    valori_filtrati <- valori[!is.na(valori) & valori != ""]
    paste(unlist(valori_filtrati), collapse = "; ")
  })

# Conta frequenze delle righe compatte
revcode_0_frequenze_compatte <- tibble(riga = revcode_righe_text) %>%
  count(riga, sort = TRUE) %>%
  mutate(
    percent = round(n / sum(n) * 100, 1),
    label_percent = paste0(percent, "%")
  )

print(revcode_0_frequenze_compatte, n = 100)

# --- Frequenze codici ICD10 con etichette corrispondenti ---

# Estrai i codici ICD10 e le relative etichette ICD9 nelle rispettive colonne
codici_etichetta_df <- revcode_0 %>%
  select(all_of(patology_code_cols_icd10), all_of(patology_label_cols_icd9)) %>%
  # Ricordiamo che patology_code_cols_icd10 e patology_label_cols_icd9 hanno la stessa lunghezza e corrispondenza posizione
  # quindi li uniamo per indice di colonna
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols = all_of(c(patology_code_cols_icd10, patology_label_cols_icd9)),
    names_to = "variabile",
    values_to = "valore"
  ) %>%
  # Separiamo codice ed etichetta in due dataframe e li uniamo dopo
  group_by(row_id) %>%
  summarise(
    codici = list(valore[variabile %in% patology_code_cols_icd10]),
    etichette = list(valore[variabile %in% patology_label_cols_icd9])
  ) %>%
  ungroup() %>%
  # Unpivot per ogni riga (zip codici-etichette in righe separate)
  tidyr::unnest(c(codici, etichette)) %>%
  filter(!is.na(codici), codici != "", codici != "0")

# Conta frequenze codice + etichetta
freq_codici_etichetta <- codici_etichetta_df %>%
  count(codici, etichette, sort = TRUE) %>%
  rename(codice = codici, etichetta = etichette)

print(freq_codici_etichetta, n = 100)

# Salva i risultati
write_csv2(revcode_0_frequenze_compatte,
           file.path(export_path, "freq_righe_eliminate_revcode_0.csv"))

write_csv2(freq_codici_etichetta,
           file.path(export_path, "freq_codici_etichetta_eliminati_revcode_0.csv"))






# =====================
# --- SEZIONE REVCODE 1 (RIGHE MODIFICATE) ---
# =====================
# Filtra righe modificate (revcode == 1)
revcode_1 <- revcode_subset %>% filter(revcode == 1)

# Prendi righe corrispondenti da original_df
orig_1 <- original_df %>% filter(..ROWNUMBER.. %in% revcode_1$..ROWNUMBER..)

# Trova colonne comuni per confronto
common_cols <- intersect(colnames(orig_1), colnames(revcode_1))

# Converto in character per confronto
orig_1_char <- orig_1 %>%
  select(all_of(common_cols)) %>%
  mutate(across(-..ROWNUMBER.., as.character)) %>%
  mutate(versione = "originale")

revcode_1_char <- revcode_1 %>%
  select(all_of(common_cols)) %>%
  mutate(across(-..ROWNUMBER.., as.character)) %>%
  mutate(versione = "modificato")

# Unisco, metto in forma lunga, ricostruisco in forma larga, filtro differenze
confronto <- bind_rows(orig_1_char, revcode_1_char) %>%
  pivot_longer(cols = -c(..ROWNUMBER.., versione), names_to = "variabile", values_to = "valore") %>%
  pivot_wider(names_from = versione, values_from = valore) %>%
  filter(originale != modificato) %>%
  select(-variabile)  # Rimuovo colonna variabile come richiesto

print(confronto)
write.csv2(confronto, file = paste0(export_path, "/confronto_revcode1.csv"), row.names = FALSE, na = "")




# =====================
# --- SEZIONE REVCODE 2 (RIGHE ACCETTATE) ---
# =====================
revcode_2 <- revcode_subset %>% filter(revcode == 2)

# Estrai codici patologia revcode 2 in formato lungo
pat_cols_revcode2 <- revcode_2 %>%
  select(starts_with("COD_PAT"), starts_with("patol")) %>%
  pivot_longer(cols = everything(), names_to = "colonna", values_to = "codice") %>%
  filter(!is.na(codice), codice != 0)

# Frequenze codici patologia revcode 2
freq_patologie_revcode2 <- pat_cols_revcode2 %>%
  count(codice, sort = TRUE)

print(freq_patologie_revcode2)

write_csv2(freq_patologie_revcode2, file.path(export_path, "freq_codici_patologia_revcode_2.csv"))


freq_patologie_revcode2_tab <- freq_patologie_revcode2 %>%
  mutate(
    percent = round(n / sum(n) * 100, 1),
    label = paste0(percent, "%")
  )

