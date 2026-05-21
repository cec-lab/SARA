
transcodifica_finale_Eurocat_Imer <- read_excel("/home/imer/works/algo_sdo/transcodifica_finale_Eurocat_Imer.xlsx")
cedap_plus_dd <- read_delim("/home/imer/works/algo_sdo/tables/cedap_plus_dd.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
eurocat_data_dict <- read_excel("/home/imer/works/algo_sdo/tables/eurocat_data_dict.xlsx")
sdo_stage_11b_clinical_rev_export_final <- read_delim("/home/imer/works/algo_sdo/stage_12/sdo_stage_11b_clinical_rev_export_final.csv", 
                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
data_structure <- read_csv2(paste0(tableDir, "/sdo_cedap_dd.csv"))



# 1. Rimuovere virgolette da tutte le celle (se presenti)
transcodifica_finale_Eurocat_Imer[] <- lapply(transcodifica_finale_Eurocat_Imer, function(x) {
  if (is.character(x)) gsub('"', '', x) else x
})

# 2. Spostare righe con NA in EUROCAT_VARIABLE in fondo
transcodifica_finale_Eurocat_Imer <- transcodifica_finale_Eurocat_Imer %>%
  mutate(na_flag = is.na(EUROCAT_VARIABLE)) %>%
  arrange(na_flag) %>%
  select(-na_flag)

# 3. Ordinare secondo eurocat_data_dict$`Variable / Field Name`
# Assicurati che non ci siano NA nel vettore di ordinamento
variabili_ordine <- eurocat_data_dict$`Variable / Field Name` %>% na.omit()

# Aggiungi un fattore per forzare l'ordinamento
transcodifica_finale_Eurocat_Imer <- transcodifica_finale_Eurocat_Imer %>%
  mutate(EUROCAT_VARIABLE = trimws(EUROCAT_VARIABLE)) %>%
  mutate(order_var = match(EUROCAT_VARIABLE, variabili_ordine)) %>%
  arrange(is.na(order_var), order_var) %>%
  select(-order_var)

# 4. Variabili presenti in eurocat_data_dict ma mancanti nella transcodifica
variabili_mancanti <- setdiff(variabili_ordine, transcodifica_finale_Eurocat_Imer$EUROCAT_VARIABLE)

# Mostra le variabili mancanti
print(variabili_mancanti)





# Vettore dei valori CedAP attesi
valori_cedap <- cedap_plus_dd$value

# Filtra righe dove SDO_export è uno dei valori attesi
righe_trovate <- transcodifica_finale_Eurocat_Imer %>%
  filter(SDO_export %in% valori_cedap)

# Controlla se in tutte queste righe Origine...8 è "CedAP"
controllo_origine <- righe_trovate %>%
  filter(`Origine...8` != "CedAP")

# 1. Mostra le righe problematiche (se ci sono)
print(controllo_origine)

# 2. Controlla se tutti i valori sono mappati con Origine "CedAP"
valori_non_mappati_correttamente <- setdiff(valori_cedap, righe_trovate$SDO_export[righe_trovate$`Origine...8` == "CedAP"])
cat("Valori con Origine diversa da CedAP (o mancanti):\n")
print(valori_non_mappati_correttamente)

# 3. Controlla anche se ci sono valori di cedap_plus_dd$value del tutto assenti
valori_assenti <- setdiff(valori_cedap, transcodifica_finale_Eurocat_Imer$SDO_export)
cat("Valori non trovati in SDO_export:\n")
print(valori_assenti)






# Nomi delle colonne da controllare
variabili_sdo <- colnames(sdo_stage_11b_clinical_rev_export_final)

# Colonna di riferimento nel mapping
variabili_mappate <- transcodifica_finale_Eurocat_Imer$SDO_export

# Rimuove eventuali NA
variabili_mappate <- na.omit(variabili_mappate)

# Trova variabili non mappate
variabili_non_mappate <- setdiff(variabili_sdo, variabili_mappate)

# Stampa i risultati
cat("Variabili presenti nel dataset finale ma NON mappate in transcodifica:\n")
print(variabili_non_mappate)


transcodifica_finale_Eurocat_Imer <- dplyr::mutate(
  transcodifica_finale_Eurocat_Imer,
  dplyr::across(
    .cols = where(is.character),
    .fns = ~ gsub('[\'"`]', '', .x)
  )
)


#tracciato
# Crea il mapping da Tracciato a Origine (univoco)
origine_lookup <- setNames(data_structure$Origine, data_structure$Tracciato)

# Applica il mapping alla colonna Origine...8, mettendo NA dove non c'è match
transcodifica_finale_Eurocat_Imer$Origine...8 <- origine_lookup[transcodifica_finale_Eurocat_Imer$SDO_export]

write.xlsx(transcodifica_finale_Eurocat_Imer,file = "/home/imer/works/algo_sdo/transcodifica_finale_Eurocat_Imer.xlsx")
