# ---- CARICAMENTO DATI ----
dset <- read_csv("export/sdo_stage12_transcode.csv")

# ---- 1. CALCOLO % MANCANTI ----
missing_pct <- sapply(dset, function(x) mean(is.na(x)) * 100)

# ---- 2. FILTRO VARIABILI SOTTO SOGLIA DI MISSING ----
soglia <- 80
vars_sotto_soglia <- names(missing_pct[missing_pct < soglia])

# ---- 3. RIMOZIONE VARIABILI MALFORMAZIONI ----
malfo_pattern <- "^malfo[1-8]$|^sp_malfo[1-8]$|^premal[1-8]$|^syndrome$|^sp_syndrome$|^moanom$|^faanom$|^sibanom$|^sp_moanom$|^sp_faanom$|^sp_sibanom$|^nbrmalf$"
vars_coorte <- vars_sotto_soglia[!grepl(malfo_pattern, vars_sotto_soglia)]
vars_escluse <- setdiff(names(dset), vars_coorte)

# ---- 4. CATEGORIZZAZIONE VARIABILI CATEGORICHE ----
categorical_vars <- c("sex", "mo_smoking", "matedu", "region_res_mo", "socm", "socf")
categorical_vars <- intersect(categorical_vars, vars_coorte)
dset[categorical_vars] <- lapply(dset[categorical_vars], as.factor)

# ---- 4.1 CORREZIONE PER VALORI '9' CHE INDICANO 'NON NOTO' ----
vars_9_as_missing <- c("nbrbaby")  # Aggiungi qui eventuali altre variabili
dset_tab <- dset
for (var in vars_9_as_missing) {
  if (var %in% names(dset_tab)) {
    dset_tab[[var]][dset_tab[[var]] == 9] <- NA
  }
}

# ---- 5. TABELLA ONE (descrizione statistica) ----
tab_descrittive <- CreateTableOne(
  vars = vars_coorte,
  data = dset_tab,
  factorVars = categorical_vars,
  includeNA = TRUE
)
tab_descr_df <- print(tab_descrittive, showAllLevels = TRUE, missing = TRUE, printToggle = FALSE)
tab_descr_df_export <- data.frame(Variabile = rownames(tab_descr_df), tab_descr_df, row.names = NULL)

# ---- 6. FUNZIONE PER MISSING, 9, NORMALI ----
calcola_missing_stats <- function(dset) {
  missing_stats <- data.frame(
    Variabile = names(dset),
    Percentuale_NA = numeric(length(dset)),
    Percentuale_9 = numeric(length(dset)),
    Percentuale_Normale = numeric(length(dset))
  )
  
  for (i in seq_along(dset)) {
    var <- names(dset)[i]
    col <- dset[[var]]
    
    num_na <- sum(is.na(col))
    num_9 <- sum(col == 9, na.rm = TRUE)
    num_normali <- sum(!(is.na(col) | col == 9))
    total <- length(col)
    
    missing_stats[i, "Percentuale_NA"] <- (num_na / total) * 100
    missing_stats[i, "Percentuale_9"] <- (num_9 / total) * 100
    missing_stats[i, "Percentuale_Normale"] <- (num_normali / total) * 100
  }
  
  return(missing_stats)
}
missing_stats <- calcola_missing_stats(dset)

# ---- 7. FREQUENZE PER VARIABILI CATEGORICHE ----
frequenze_categoriche <- lapply(categorical_vars, function(var) {
  freq <- as.data.frame(table(dset[[var]], useNA = "ifany"))
  names(freq) <- c("Valore", "Frequenza")
  freq$Variabile <- var
  return(freq)
})
frequenze_categoriche_df <- do.call(rbind, frequenze_categoriche)[, c("Variabile", "Valore", "Frequenza")]

# ---- 8. SALVATAGGIO ----
write.csv2(tab_descr_df_export, "export/statistiche_var_num.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv2(missing_stats, "export/missing_stats.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(frequenze_categoriche_df, "export/statistiche_var_categoriche.csv", row.names = FALSE)

# ---- 9. GRAFICO VARIABILI ESCLUSE ----
# ---- 9.1 PREPARAZIONE DATI PER GRAFICO A 100% ----
library(tidyr)

missing_long <- missing_stats %>%
  pivot_longer(cols = c(Percentuale_NA, Percentuale_9, Percentuale_Normale),
               names_to = "Tipo",
               values_to = "Percentuale")

# Rinomina per chiarezza
missing_long$Tipo <- factor(missing_long$Tipo,
                            levels = c("Percentuale_NA", "Percentuale_9", "Percentuale_Normale"),
                            labels = c("NA", "Codice 9", "Valori validi"))

# ---- 9.2 GRAFICO A BARRE 100% STACKED ----

# Dati solo per le variabili escluse
missing_escluse <- missing_stats[missing_stats$Variabile %in% vars_escluse, ]

# Riordina per percentuale NA
missing_escluse_long <- pivot_longer(
  missing_escluse,
  cols = c("Percentuale_NA", "Percentuale_9", "Percentuale_Normale"),
  names_to = "Tipo",
  values_to = "Percentuale"
)
missing_escluse_long$Variabile <- factor(
  missing_escluse_long$Variabile,
  levels = missing_escluse[order(-missing_escluse$Percentuale_NA), "Variabile"]
)

# Grafico 1 – NA vs Valori validi
png("export/grafico_na_vs_validi.png", width = 1000, height = 800, res = 150)
ggplot(
  missing_escluse_long[missing_escluse_long$Tipo %in% c("Percentuale_NA", "Percentuale_Normale"), ],
  aes(x = Variabile, y = Percentuale, fill = Tipo)
) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("Percentuale_NA" = "firebrick", "Percentuale_Normale" = "gray80")) +
  theme_classic() +
  labs(
    title = "Composizione delle variabili escluse – NA vs Valori validi",
    x = "Variabile",
    y = "Percentuale"
  ) +
  theme(
    axis.text.y = element_text(size = 4),
    legend.title = element_blank()
  )
dev.off()

# Grafico 2 – Codici 9 vs Valori validi
png("export/grafico_9_vs_validi.png", width = 1000, height = 800, res = 150)
ggplot(
  missing_escluse_long[missing_escluse_long$Tipo %in% c("Percentuale_9", "Percentuale_Normale"), ],
  aes(x = Variabile, y = Percentuale, fill = Tipo)
) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("Percentuale_9" = "steelblue", "Percentuale_Normale" = "gray80")) +
  theme_classic() +
  labs(
    title = "Composizione delle variabili escluse – Codici 9 vs Valori validi",
    x = "Variabile",
    y = "Percentuale"
  ) +
  theme(
    axis.text.y = element_text(size = 4),
    legend.title = element_blank()
  )
dev.off()