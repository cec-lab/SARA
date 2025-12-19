# Sezione 2: Identificazione dei valori unici in `PROG_PAZ`

Prog_unici <- unique(sdo$PROG_PAZ)
print(paste("Totale dei valori unici in PROG_PAZ:", length(Prog_unici)))

# Sezione 4: Gestione dei duplicati
library(tidyr)

# Colonne da combinare
colonne <- c("COD_RG", "COD_PRES", "PROG_SDO", "COD_AZI")

# Convertire le colonne in carattere per evitare problemi di tipo
 sdo <- sdo %>%
   mutate(across(all_of(colonne), as.character))

# Generare tutte le combinazioni di colonne
combinazioni <- unlist(lapply(1:length(colonne), function(x) combn(colonne, x, simplify = FALSE)), recursive = FALSE)

# Risultati finali
duplicati_risolti <- data.frame()
duplicati_non_risolti <- data.frame()

# Ciclo per verificare tutte le combinazioni
for (combo in combinazioni) {
  # Creare una chiave composta
  chiave <- paste(combo, collapse = "_")
  sdo <- sdo %>%
    mutate(!!chiave := paste(!!!syms(combo), sep = "_"))
  
  # Controllare i duplicati in base a PROG_PAZ e la chiave attuale
  duplicati <- sdo %>%
    group_by(PROG_PAZ) %>%
    filter(n() > 1) %>%
    ungroup()
  
  duplicati_risolti_combo <- duplicati %>%
    group_by_at(c("PROG_PAZ", chiave)) %>%
    filter(n() == 1) %>%
    ungroup()
  
  duplicati_non_risolti_combo <- duplicati %>%
    group_by_at(c("PROG_PAZ", chiave)) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Aggiungere i risultati
  duplicati_risolti <- bind_rows(duplicati_risolti, duplicati_risolti_combo) %>% distinct()
  duplicati_non_risolti <- bind_rows(duplicati_non_risolti, duplicati_non_risolti_combo) %>% distinct()
}

# Trova le combinazioni che hanno risolto duplicati
combinazione_ottimale <- combinazioni[
  sapply(combinazioni, function(combo) {
    chiave <- paste(combo, collapse = "_")
    all(!duplicated(sdo[[chiave]]))
  })
]

print("Combinazione ottimale trovata:")
print(combinazione_ottimale)

print(paste("Duplicati risolti:", nrow(duplicati_risolti)))
print(paste("Duplicati non risolti:", nrow(duplicati_non_risolti)))

# Monitoraggio e tabella di frequenza
duplicati_risolti_combo <- duplicati_risolti_combo %>%
  mutate(duplicati_combinazione = duplicated(.))

# Conta delle righe duplicate
num_duplicati <- sum(duplicati_risolti_combo$duplicati_combinazione)
print(paste("Numero di duplicati rimasti in duplicati_risolti_combo:", num_duplicati))

# CONTROLLO SDO FILTRATE ----

# Verifica: controllo che ogni riga abbia almeno un codice valido
verifica_codici <- function(data, col_names, malfoCodes) {
  verifica_righe <- logical(nrow(data))
  
  for (i in seq_len(nrow(data))) {
    for (col in col_names) {
      if (!is.na(data[[col]][i]) && filtro_codici(data[[col]][i])) {
        verifica_righe[i] <- TRUE
        break
      }
    }
  }
  return(verifica_righe)
}

righe_con_codici_validi <- verifica_codici(sdo_filtrato, colonne_icd9, malfoCodes)

if (all(righe_con_codici_validi)) {
  print("Tutte le righe di sdo_filtrato contengono almeno un codice valido.")
} else {
  print("ATTENZIONE: alcune righe di sdo_filtrato NON contengono codici validi.")
  righe_problematiche <- which(!righe_con_codici_validi)
  print(paste("Numero di righe problematiche:", length(righe_problematiche)))
  print("Indice delle righe problematiche:")
  print(righe_problematiche)
}
# PROVA CHIAVE ----

table(str_length(sdo$AA_SDO))
table(str_length(sdo$COD_AZI))
table(str_length(sdo$PROG_SDO))
table(str_length(sdo$PROG_PAZ))
table(str_length(sdo$COD_PRES))

sdo$key=paste0(sdo$AA_SDO, "_", sdo$COD_PRES,"_", str_pad(sdo$PROG_SDO, width = 8, side = 'left', pad = '0', use_width = T))
length(unique(sdo$key))
length(which(duplicated(sdo$key)==T))
dupKey<-sdo[which(duplicated(sdo$key)==T),]

dupKey |> group_by(key) |> filter(n()>1) |> count()
sdo |> group_by(key) |> filter(n()>1) |> count() |> print(n=47) 



write_csv2(sdo[which(duplicated(sdo$key)==T),], file=paste0(exportDir,"/duplicati_codazi_progsdo.csv"))
