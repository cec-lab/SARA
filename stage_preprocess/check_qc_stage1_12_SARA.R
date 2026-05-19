
#SCRIPT DI CONTROLLO QUALITA' SARA
source(paste0(baseDir,"/config.R"), echo = T)
source(paste0(baseDir,"/functions.R"), echo = T)




#check stage0 ----
source(paste0(stage_0Dir, "/stage_0.R"), echo = T)

#controllo dedup
cedap |> count(prog_paz_neo) |> filter(n > 1)


cat("\nCHECK STAGE 0\n")

# INTEGRITÀ BASE

cat("\nINTEGRITÀ BASE\n")

if (nrow(sdo) == 0) stop("SDO vuoto")
if (nrow(cedap) == 0) stop("CEDAP vuoto")

cat("OK dataset caricati\n")


# DATE

cat("\nDATE\n")

if (!inherits(sdo$dt_nasc, "Date")) stop("dt_nasc non è Date")
if (!inherits(sdo$dt_decesso, "Date")) stop("dt_decesso non è Date")

cat("OK classi Date\n")


# SPLIT dt_nasc

cat("\nSPLIT dt_nasc\n")

sdo_with_dt <- sdo |> filter(!is.na(dt_nasc))
sdo_wo_dt   <- sdo |> filter(is.na(dt_nasc))

if (nrow(sdo) != nrow(sdo_with_dt) + nrow(sdo_wo_dt)) {
  stop("Split dt_nasc NON coerente")
}

cat("OK split completo\n")


# COHORT

cat("\nCOHORT\n")

bad <- sdo_by_dt_nas |> filter(cohort != yearOfBirth)

if (nrow(bad) > 0) {
  stop("Record fuori cohort")
}

cat("OK cohort filtrata\n")


# LINK

cat("\nLINK\n")

if (any(sdo_by_dt_nas$cedap_linked < 0)) {
  stop("Indici negativi")
}

if (any(sdo_by_dt_nas$cedap_linked > nrow(cedap))) {
  stop("Indici fuori range")
}

cat("OK indici match\n")

cat("Linked:", sum(sdo_by_dt_nas$cedap_linked != 0), "\n")
cat("Non linked:", sum(sdo_by_dt_nas$cedap_linked == 0), "\n")


# FILL dt_nasc

cat("\nFILL dt_nasc\n")

n_linked <- sum(sdo_wo_dt_nas$cedap_linked != 0)
n_filled <- sum(!is.na(sdo_wo_dt_nas_cedap_linked$dt_nasc))

cat("Linked senza data:", n_linked, "\n")
cat("Date recuperate:", n_filled, "\n")

if (n_filled == 0 && n_linked > 0) {
  stop("Fill dt_nasc NON funzionante")
}

cat("OK fill\n")


# MERGE

cat("\nMERGE\n")

expected <- nrow(sdo_by_dt_nas) +
  nrow(sdo_wo_dt_nas_cedap_linked_by_dt_nas)

if (expected != nrow(sdo_1yfup)) {
  stop("Merge NON coerente")
}

cat("OK merge\n")


# COERENZA DATE

cat("\nCOERENZA DATE\n")

bad_dates <- sdo_1yfup |>
  filter(!is.na(dt_amm) & !is.na(dt_nasc) & ymd(dt_amm) < dt_nasc)

if (nrow(bad_dates) > 0) {
  stop("Ricoveri prima della nascita")
}

cat("OK timeline\n")


# NA CRITICI

cat("\nNA CRITICI\n")

if (any(is.na(sdo_1yfup$dt_nasc))) stop("NA dt_nasc")
if (any(is.na(sdo_1yfup$dt_amm))) stop("NA dt_amm")
if (any(is.na(sdo_1yfup$PROG_PAZ))) stop("NA PROG_PAZ")

cat("OK NA\n")


# DAYS

cat("\nDAYS\n")

neg <- sdo_1yfup |> filter(daysAfterDelivery < 0)

if (nrow(neg) > 0) {
  stop("daysAfterDelivery negativi")
}

cat("OK days\n")


# DUPLICATI

cat("\nDUPLICATI\n")

dup <- sdo_1yfup |> count(PROG_PAZ) |> filter(n > 1)

cat("Duplicati:", nrow(dup), "\n")


# FINALE

cat("\nSTAGE 0 OK\n")









#check stage1 ----
source(paste0(stage_1Dir, "/scan.R"), echo = T)



cat("\nCHECK STAGE 1\n")


# DIMENSIONI


cat("\nDIMENSIONI\n")

cat("Input:", nrow(sdo), "\n")
cat("Validi:", nrow(sdo_valid), "\n")
cat("Non validi:", nrow(sdo_invalid), "\n")

if (nrow(sdo) != nrow(sdo_valid) + nrow(sdo_invalid)) {
  stop("Perdita di record nello split")
}

cat("OK split completo\n")


# OVERLAP (corretto su righe, non su PROG_PAZ)


cat("\nOVERLAP\n")

overlap_n <- nrow(dplyr::inner_join(sdo_valid, sdo_invalid))

cat("Righe in overlap:", overlap_n, "\n")

if (overlap_n > 0) {
  stop("Righe presenti sia in validi che non validi")
}

cat("OK separazione\n")


# VALIDAZIONE CODICI

cat("\nVALIDAZIONE CODICI\n")

check_codes <- function(x) {
  if (is.na(x) || x == "") return(FALSE)
  
  codes <- stringr::str_split(x, "\\|")[[1]]
  codes <- stringr::str_trim(codes)
  
  num <- suppressWarnings(as.numeric(substr(codes, 1, 3)))
  
  any(num >= 740 & num <= 759, na.rm = TRUE)
}

test_validi <- apply(sdo_valid[, icd9SearchCols], 1, function(row) {
  any(sapply(row, check_codes))
})

if (!all(test_validi)) {
  stop("Ci sono righe valide senza codici 740-759")
}

cat("OK validi contengono codici corretti\n")



# CONTROLLO NON VALIDI

cat("\nCONTROLLO NON VALIDI\n")

test_invalidi <- apply(sdo_invalid[, icd9SearchCols], 1, function(row) {
  any(sapply(row, check_codes))
})

if (any(test_invalidi)) {
  stop("Ci sono righe NON valide che contengono codici 740-759")
}

cat("OK non validi puliti\n")



# DISTRIBUZIONE

cat("\nDISTRIBUZIONE\n")

perc_validi <- nrow(sdo_valid) / nrow(sdo)

cat("Percentuale validi:", round(perc_validi, 3), "\n")

if (perc_validi < 0.01) {
  warning("Troppo pochi validi → possibile errore filtro")
}

if (perc_validi > 0.9) {
  warning("Troppi validi → filtro troppo permissivo")
}


# NA CODICI

cat("\nNA CODICI\n")

na_rows <- sdo |> dplyr::filter(dplyr::if_all(dplyr::all_of(icd9SearchCols), is.na))

cat("Righe senza codici:", nrow(na_rows), "\n")



# CHECK STRUTTURA


cat("\nSTRUTTURA\n")

if (!identical(colnames(sdo), colnames(sdo_valid))) {
  stop("Colonne cambiate nei validi")
}

if (!identical(colnames(sdo), colnames(sdo_invalid))) {
  stop("Colonne cambiate nei non validi")
}

cat("OK struttura invariata\n")



# FINALE

cat("\nRISULTATO\n")

cat("STAGE 1 OK\n")



#check stage2 ----


source(paste0(stage_2Dir, "/scan_extra.R"), echo = T)


cat("\nCHECK STAGE 2\n")

# DIMENSIONI

cat("\nDIMENSIONI\n")

cat("Stage1 validi:", nrow(sdo_valid_stage1), "\n")
cat("Extra recuperati:", nrow(sdo_extra_valid), "\n")
cat("Totale finale:", nrow(sdo_stage_2_complete), "\n")

expected <- nrow(sdo_valid_stage1) + nrow(sdo_extra_valid)

if (nrow(sdo_stage_2_complete) != expected) {
  stop("Errore nel merge: dimensioni non tornano")
}

cat("OK merge dimensioni\n")


# OVERLAP VALIDI vs EXTRA

cat("\nOVERLAP VALIDI vs EXTRA\n")

overlap <- nrow(dplyr::inner_join(
  as.data.frame(sdo_valid_stage1),
  as.data.frame(sdo_extra_valid)
))

cat("Righe duplicate:", overlap, "\n")

if (overlap > 0) {
  warning("Attenzione: duplicati tra validi e extra")
} else {
  cat("OK nessun duplicato\n")
}


# VALIDAZIONE EXTRA

cat("\nVALIDAZIONE EXTRA\n")

check_extra <- function(x, extra_codes) {
  if (is.na(x) || x == "") return(FALSE)
  
  codes <- stringr::str_split(x, "\\|")[[1]]
  codes <- stringr::str_trim(codes)
  
  codes_clean <- stringr::str_pad(codes, 6, "right", "0")
  
  any(codes_clean %in% stringr::str_pad(extra_codes$ICD9, 6, "right", "0"))
}

test_extra <- apply(sdo_extra_valid[, ..icd9SearchCols], 1, function(row) {
  any(sapply(row, check_extra, extra_codes = extra_codes))
})

if (!all(test_extra)) {
  stop("Ci sono righe extra senza codici extra validi")
}

cat("OK extra validi\n")


# VALIDAZIONE CODICI COMPLETI

cat("\nVALIDAZIONE CODICI COMPLETI\n")

check_codes_all <- function(x) {
  if (is.na(x) || x == "") return(FALSE)
  
  codes <- stringr::str_split(x, "\\|")[[1]]
  codes <- stringr::str_trim(codes)
  
  num <- suppressWarnings(as.numeric(substr(codes, 1, 3)))
  
  valid_icd9 <- num >= 740 & num <= 759
  
  codes_pad <- stringr::str_pad(codes, 6, "right", "0")
  valid_extra <- codes_pad %in% stringr::str_pad(extra_codes$ICD9, 6, "right", "0")
  
  any(valid_icd9 | valid_extra, na.rm = TRUE)
}

test_all <- apply(sdo_stage_2_complete[, ..icd9SearchCols], 1, function(row) {
  any(sapply(row, check_codes_all))
})

if (!all(test_all)) {
  stop("Ci sono righe con codici non validi dopo pulizia")
}

cat("OK codici finali validi\n")


# CONTROLLO SHIFT

cat("\nCONTROLLO SHIFT\n")

empty_left <- apply(sdo_stage_2_complete[, ..icd9SearchCols], 1, function(row) {
  
  row <- as.character(row)
  
  first_na <- which(is.na(row) | row == "")[1]
  
  if (is.na(first_na)) return(FALSE)
  
  later_values <- any(!is.na(row[(first_na+1):length(row)]) &
                        row[(first_na+1):length(row)] != "")
  
  return(later_values)
})

if (any(empty_left, na.rm = TRUE)) {
  stop("Shift non corretto: buchi a sinistra")
}

cat("OK shift corretto\n")


# NA CRITICI

cat("\nNA CRITICI\n")

cat("NA PROG_PAZ:", sum(is.na(sdo_stage_2_complete$PROG_PAZ)), "\n")
cat("NA dt_nasc:", sum(is.na(sdo_stage_2_complete$dt_nasc)), "\n")

if (sum(is.na(sdo_stage_2_complete$PROG_PAZ)) > 0) {
  stop("PROG_PAZ contiene NA")
}


# STRUTTURA

cat("\nSTRUTTURA\n")

if (!identical(colnames(sdo_valid_stage1), colnames(sdo_stage_2_complete))) {
  stop("Colonne cambiate nello stage 2")
}

cat("OK struttura invariata\n")


# RISULTATO

cat("\nRISULTATO\n")

cat("STAGE 2 OK\n")



#check stage3 ----
source(paste0(stage_3Dir, "/minors_removal.R"), echo = T)

cat("\nCLASSI PATOLOGIE\n")

minor_codes_pad <- stringr::str_pad(minor_codes, 6, "right", "0")

get_codes <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  
  codes <- stringr::str_split(x, "\\|")[[1]]
  codes <- stringr::str_trim(codes)
  stringr::str_pad(codes, 6, "right", "0")
}

dt3_codes <- sdo_stage_3_complete[, icd9SearchCols, with = FALSE]


classify_row <- function(row) {
  
  get_codes <- function(x) {
    if (is.na(x) || x == "") return(character(0))
    
    codes <- stringr::str_split(x, "\\|")[[1]]
    codes <- stringr::str_trim(codes)
    codes[codes != ""]
  }
  
  # 👉 TUTTI i codici (senza filtro)
  codes_all <- unlist(lapply(row, get_codes))
  
  # 👉 VUOTO = nessun codice proprio
  if (length(codes_all) == 0) {
    return("vuoto")
  }
  
  # ora lavori SOLO su patologie 740–759
  num <- suppressWarnings(as.numeric(substr(codes_all, 1, 3)))
  in_range <- num >= 740 & num <= 759
  
  codes_range <- codes_all[in_range]
  
  # 👉 se NON ci sono patologie → NON è vuoto
  if (length(codes_range) == 0) {
    return("altro")   # oppure "no_patologie" se vuoi tracciarle
  }
  
  codes_pad <- stringr::str_pad(codes_range, 6, "right", "0")
  
  is_minore <- codes_pad %in% minor_codes_pad
  
  if (all(is_minore)) return("solo_minori")
  if (!any(is_minore)) return("solo_maggiori")
  
  return("minori_e_maggiori")
}
row_types <- apply(as.data.frame(dt3_codes), 1, classify_row)

counts <- table(row_types)
print(counts)
solo_minori <- sdo_stage_3_complete[row_types == "solo_minori", ]
solo_maggiori <- sdo_stage_3_complete[row_types == "solo_maggiori", ]
minori_e_maggiori <- sdo_stage_3_complete[row_types == "minori_e_maggiori", ]
vuoto <- sdo_stage_3_complete[row_types == "vuoto", ]


#check stage4_7 ----
cat("\nCHECK VALIDAZIONE STAGE 4-7\n")

check_validation <- function(sdo, stage_name) {
  
  cat("\n============================\n")
  cat("CHECK", stage_name, "\n")
  cat("============================\n")
  
  # MATRICE
  tab <- table(
    validated = sdo$validated,
    validation_type = sdo$validation_type
  )
  
  print(tab)
  
  # 1. validated binario
  if (!all(sdo$validated %in% c(0,1))) {
    stop(paste(stage_name, "- validated non binario"))
  }
  cat("OK validated\n")
  
  # 2. validation_type valido
  valid_types <- c(
    "0",
    "1","2","3","4",
    "1|2","1|3","1|4","2|3","2|4","3|4",
    "1|2|3","1|2|4","1|3|4","2|3|4",
    "1|2|3|4"
  )
  
  if (!all(sdo$validation_type %in% valid_types)) {
    bad <- unique(sdo$validation_type[!sdo$validation_type %in% valid_types])
    print(bad)
    stop(paste(stage_name, "- validation_type non valido"))
  }
  cat("OK validation_type\n")
  
  # 3. coerenza logica
  if (any(sdo$validated == 0 & sdo$validation_type != "0")) {
    stop(paste(stage_name, "- incoerenza validated=0"))
  }
  
  if (any(sdo$validated == 1 & sdo$validation_type == "0")) {
    stop(paste(stage_name, "- incoerenza validated=1"))
  }
  
  cat("OK coerenza\n")
  
  # 4. formato stringa
  check_format <- function(x) {
    if (x == "0") return(TRUE)
    parts <- unlist(strsplit(x, "\\|"))
    all(parts %in% c("1","2","3","4")) &&
      length(unique(parts)) == length(parts)
  }
  
  format_ok <- sapply(sdo$validation_type, check_format)
  
  if (!all(format_ok)) {
    print(unique(sdo$validation_type[!format_ok]))
    stop(paste(stage_name, "- formato non valido"))
  }
  
  cat("OK formato\n")
  cat(">>>", stage_name, "OK\n")
}

# STAGE 4
source(paste0(stage_4Dir, "/validation_1.R"), echo = TRUE)
check_validation <- function(sdo, stage_name) {
  
  cat("\n============================\n")
  cat("CHECK", stage_name, "\n")
  cat("============================\n")
  
  # MATRICE
  tab <- table(
    validated = sdo$validated,
    validation_type = sdo$validation_type
  )
  
  print(tab)
  
  # 1. validated binario
  if (!all(sdo$validated %in% c(0,1))) {
    stop(paste(stage_name, "- validated non binario"))
  }
  cat("OK validated\n")
  
  # 2. validation_type valido
  valid_types <- c(
    "0",
    "1","2","3","4",
    "1|2","1|3","1|4","2|3","2|4","3|4",
    "1|2|3","1|2|4","1|3|4","2|3|4",
    "1|2|3|4"
  )
  
  if (!all(sdo$validation_type %in% valid_types)) {
    bad <- unique(sdo$validation_type[!sdo$validation_type %in% valid_types])
    print(bad)
    stop(paste(stage_name, "- validation_type non valido"))
  }
  cat("OK validation_type\n")
  
  # 3. coerenza logica
  if (any(sdo$validated == 0 & sdo$validation_type != "0")) {
    stop(paste(stage_name, "- incoerenza validated=0"))
  }
  
  if (any(sdo$validated == 1 & sdo$validation_type == "0")) {
    stop(paste(stage_name, "- incoerenza validated=1"))
  }
  
  cat("OK coerenza\n")
  
  # 4. formato stringa
  check_format <- function(x) {
    if (x == "0") return(TRUE)
    parts <- unlist(strsplit(x, "\\|"))
    all(parts %in% c("1","2","3","4")) &&
      length(unique(parts)) == length(parts)
  }
  
  format_ok <- sapply(sdo$validation_type, check_format)
  
  if (!all(format_ok)) {
    print(unique(sdo$validation_type[!format_ok]))
    stop(paste(stage_name, "- formato non valido"))
  }
  
  cat("OK formato\n")
  cat(">>>", stage_name, "OK\n")
}
check_validation(sdo_stage_4_nominors_export, "STAGE 4")

# STAGE 5
source(paste0(stage_5Dir, "/validation_2.R"), echo = TRUE)
check_validation <- function(sdo, stage_name) {
  
  cat("\n============================\n")
  cat("CHECK", stage_name, "\n")
  cat("============================\n")
  
  # MATRICE
  tab <- table(
    validated = sdo$validated,
    validation_type = sdo$validation_type
  )
  
  print(tab)
  
  # 1. validated binario
  if (!all(sdo$validated %in% c(0,1))) {
    stop(paste(stage_name, "- validated non binario"))
  }
  cat("OK validated\n")
  
  # 2. validation_type valido
  valid_types <- c(
    "0",
    "1","2","3","4",
    "1|2","1|3","1|4","2|3","2|4","3|4",
    "1|2|3","1|2|4","1|3|4","2|3|4",
    "1|2|3|4"
  )
  
  if (!all(sdo$validation_type %in% valid_types)) {
    bad <- unique(sdo$validation_type[!sdo$validation_type %in% valid_types])
    print(bad)
    stop(paste(stage_name, "- validation_type non valido"))
  }
  cat("OK validation_type\n")
  
  # 3. coerenza logica
  if (any(sdo$validated == 0 & sdo$validation_type != "0")) {
    stop(paste(stage_name, "- incoerenza validated=0"))
  }
  
  if (any(sdo$validated == 1 & sdo$validation_type == "0")) {
    stop(paste(stage_name, "- incoerenza validated=1"))
  }
  
  cat("OK coerenza\n")
  
  # 4. formato stringa
  check_format <- function(x) {
    if (x == "0") return(TRUE)
    parts <- unlist(strsplit(x, "\\|"))
    all(parts %in% c("1","2","3","4")) &&
      length(unique(parts)) == length(parts)
  }
  
  format_ok <- sapply(sdo$validation_type, check_format)
  
  if (!all(format_ok)) {
    print(unique(sdo$validation_type[!format_ok]))
    stop(paste(stage_name, "- formato non valido"))
  }
  
  cat("OK formato\n")
  cat(">>>", stage_name, "OK\n")
}
check_validation(sdo_stage_5_validated, "STAGE 5")

# STAGE 6
source(paste0(stage_6Dir, "/validation_3.R"), echo = TRUE)
check_validation <- function(sdo, stage_name) {
  
  cat("\n============================\n")
  cat("CHECK", stage_name, "\n")
  cat("============================\n")
  
  # MATRICE
  tab <- table(
    validated = sdo$validated,
    validation_type = sdo$validation_type
  )
  
  print(tab)
  
  # 1. validated binario
  if (!all(sdo$validated %in% c(0,1))) {
    stop(paste(stage_name, "- validated non binario"))
  }
  cat("OK validated\n")
  
  # 2. validation_type valido
  valid_types <- c(
    "0",
    "1","2","3","4",
    "1|2","1|3","1|4","2|3","2|4","3|4",
    "1|2|3","1|2|4","1|3|4","2|3|4",
    "1|2|3|4"
  )
  
  if (!all(sdo$validation_type %in% valid_types)) {
    bad <- unique(sdo$validation_type[!sdo$validation_type %in% valid_types])
    print(bad)
    stop(paste(stage_name, "- validation_type non valido"))
  }
  cat("OK validation_type\n")
  
  # 3. coerenza logica
  if (any(sdo$validated == 0 & sdo$validation_type != "0")) {
    stop(paste(stage_name, "- incoerenza validated=0"))
  }
  
  if (any(sdo$validated == 1 & sdo$validation_type == "0")) {
    stop(paste(stage_name, "- incoerenza validated=1"))
  }
  
  cat("OK coerenza\n")
  
  # 4. formato stringa
  check_format <- function(x) {
    if (x == "0") return(TRUE)
    parts <- unlist(strsplit(x, "\\|"))
    all(parts %in% c("1","2","3","4")) &&
      length(unique(parts)) == length(parts)
  }
  
  format_ok <- sapply(sdo$validation_type, check_format)
  
  if (!all(format_ok)) {
    print(unique(sdo$validation_type[!format_ok]))
    stop(paste(stage_name, "- formato non valido"))
  }
  
  cat("OK formato\n")
  cat(">>>", stage_name, "OK\n")
}
check_validation(sdo_stage_6_validated, "STAGE 6")

# STAGE 7
source(paste0(stage_7Dir, "/validation_4.R"), echo = TRUE)
check_validation <- function(sdo, stage_name) {
  
  cat("\n============================\n")
  cat("CHECK", stage_name, "\n")
  cat("============================\n")
  
  # MATRICE
  tab <- table(
    validated = sdo$validated,
    validation_type = sdo$validation_type
  )
  
  print(tab)
  
  # 1. validated binario
  if (!all(sdo$validated %in% c(0,1))) {
    stop(paste(stage_name, "- validated non binario"))
  }
  cat("OK validated\n")
  
  # 2. validation_type valido
  valid_types <- c(
    "0",
    "1","2","3","4",
    "1|2","1|3","1|4","2|3","2|4","3|4",
    "1|2|3","1|2|4","1|3|4","2|3|4",
    "1|2|3|4"
  )
  
  if (!all(sdo$validation_type %in% valid_types)) {
    bad <- unique(sdo$validation_type[!sdo$validation_type %in% valid_types])
    print(bad)
    stop(paste(stage_name, "- validation_type non valido"))
  }
  cat("OK validation_type\n")
  
  # 3. coerenza logica
  if (any(sdo$validated == 0 & sdo$validation_type != "0")) {
    stop(paste(stage_name, "- incoerenza validated=0"))
  }
  
  if (any(sdo$validated == 1 & sdo$validation_type == "0")) {
    stop(paste(stage_name, "- incoerenza validated=1"))
  }
  
  cat("OK coerenza\n")
  
  # 4. formato stringa
  check_format <- function(x) {
    if (x == "0") return(TRUE)
    parts <- unlist(strsplit(x, "\\|"))
    all(parts %in% c("1","2","3","4")) &&
      length(unique(parts)) == length(parts)
  }
  
  format_ok <- sapply(sdo$validation_type, check_format)
  
  if (!all(format_ok)) {
    print(unique(sdo$validation_type[!format_ok]))
    stop(paste(stage_name, "- formato non valido"))
  }
  
  cat("OK formato\n")
  cat(">>>", stage_name, "OK\n")
}
check_validation(sdo_stage_7, "STAGE 7")


#check stage8 ----
source(paste0(stage_8Dir, "/filters3.R"), echo = T)
cat("\nCHECK FILTRO\n")

# fix tipo dopo fread
if (!is.numeric(sdo_stage8$sds_cc)) {
  sdo_stage8$sds_cc <- as.numeric(gsub(",", ".", sdo_stage8$sds_cc))
}

flag_check <- rep(FALSE, nrow(sdo_stage8))

for(i in 1:nrow(filter)){
  
  icd = filter$ICD9[i]
  eg = filter$eg[i]
  ricgg = filter$ricgg[i]
  isolata = filter$isolata[i]
  sds = filter$sds[i]
  
  cond <- rep(TRUE, nrow(sdo_stage8))
  
  if (eg > 0) {
    cond <- cond & (sdo_stage8$eta_gestazionale < eg)
  }
  
  if (ricgg > 0) {
    cond <- cond & (sdo_stage8$GG_DEG < ricgg)
  }
  
  if (isolata == 1) {
    cond <- cond & (sdo_stage8$malformazione_tipo == "isolata")
  }
  
  if (sds == 1) {
    cond <- cond & (!is.na(sdo_stage8$sds_cc) &
                      sdo_stage8$sds_cc < SDS_circonferenza_cranica_cutoff)
  }
  
  icd_match <- (
    sdo_stage8$COD_PAT1 == icd |
      sdo_stage8$patol2 == icd |
      sdo_stage8$patol3 == icd |
      sdo_stage8$patol4 == icd |
      sdo_stage8$patol5 == icd |
      sdo_stage8$patol6 == icd
  )
  
  flag_check <- flag_check | (cond & icd_match)
}

mismatch <- which(flag_check != sdo_stage8$violazione_filtro)

cat("Mismatch filtro:", length(mismatch), "\n")

if (length(mismatch) > 0) {
  print(head(sdo_stage8[mismatch], 10))
  stop("Errore filtro NON coerente")
}

cat("OK filtro\n")

source(paste0(stage_8Dir, "/score.R"), echo = T)
cat("\nCHECK SCORE\n")

if (!is.numeric(sdo_stage8$sds_cc)) {
  sdo_stage8$sds_cc <- as.numeric(gsub(",", ".", sdo_stage8$sds_cc))
}

if (!("score" %in% colnames(sdo_stage8))) {
  stop("Errore score mancante")
}

if (any(is.na(sdo_stage8$score))) {
  stop("Errore score contiene NA")
}

cat("OK score presente\n")


cat("\nCHECK SCORE VALIDATI\n")

bad_score <- sdo_stage8[
  validated == 0 & score != 0
]

cat("Non validati con score:", nrow(bad_score), "\n")

if (nrow(bad_score) > 0) {
  stop("Errore score su non validati")
}

cat("OK score validati\n")


cat("\nCOERENZA SCORE\n")

vtMatrix_check <- as_tibble(
  stringr::str_split_fixed(sdo_stage8$validation_type, "\\|", n = Inf)
) |> dplyr::mutate(dplyr::across(everything(), as.numeric))

expected_score <- apply(vtMatrix_check, 1, scoreCalculation)

mismatch <- which(expected_score != sdo_stage8$score)

cat("Mismatch score:", length(mismatch), "\n")

if (length(mismatch) > 0) {
  print(head(sdo_stage8[mismatch, c("validation_type","score")], 10))
  stop("Errore score non coerente")
}

cat("OK score coerente\n")


cat("\nDISTRIBUZIONE SCORE\n")
print(table(sdo_stage8$score))


cat("\nCHECK RANK\n")

if (exists("sdo_stage8_wscore_top_rank")) {
  if (any(sdo_stage8_wscore_top_rank$score < 4)) {
    stop("Errore top rank")
  }
}

if (exists("sdo_stage8_wscore_low_rank")) {
  if (any(sdo_stage8_wscore_low_rank$score >= 4)) {
    stop("Errore low rank")
  }
}

cat("OK ranking\n")


cat("\nRISULTATO\n")
cat("STAGE 8 OK\n")


#check stage9 ----
source(paste0(stage_9Dir, "/collapse.R"), echo = T)

cat("\nCHECK STAGE 9\n")

# check numero record

cat("\nCHECK NUMERO RECORD\n")

cat("Stage8:", nrow(sdo_stage8), "\n")
cat("Unique:", nrow(sdo_stage9_unique), "\n")
cat("Dup review:", nrow(sdo_stage9_dup_review), "\n")
cat("Collapsed:", nrow(sdo_stage9_collapsed), "\n")

# check corretto
if(nrow(sdo_stage9_unique) + nrow(sdo_stage9_dup_review) != nrow(sdo_stage8)){
  stop("Errore perdita o duplicazione record tra unique e dup_review")
}

cat("OK ricostruzione dataset originale\n")

cat("\nCHECK COLLASSO\n")

n_dup_paz <- nrow(dupProgPaz)

cat("Pazienti duplicati:", n_dup_paz, "\n")
cat("Righe collassate:", nrow(sdo_stage9_collapsed), "\n")

if(nrow(sdo_stage9_collapsed) != n_dup_paz){
  stop("Errore collasso: numero righe non coerente")
}

cat("OK collasso\n")


# check duplicati dopo collasso

cat("\nCHECK DUPLICATI\n")

dup_after <- sdo_stage9_collapsed |>
  dplyr::group_by(PROG_PAZ) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::filter(n > 1)

cat("Duplicati dopo collasso:", nrow(dup_after), "\n")

if(nrow(dup_after) > 0){
  stop("Errore duplicati ancora presenti dopo collasso")
}

cat("OK duplicati\n")


# funzione estrazione malformazioni

extract_pat <- function(df){
  unique(na.omit(as.vector(as.matrix(
    df[, c("COD_PAT1","patol2","patol3","patol4","patol5","patol6")]
  ))))
}


# check malformazioni

cat("\nCHECK MALFORMAZIONI\n")

mismatch <- 0

for(i in 1:nrow(dupProgPaz)){
  
  id <- dupProgPaz$PROG_PAZ[i]
  
  original <- sdo_stage8 |> dplyr::filter(PROG_PAZ == id)
  
  collapsed <- sdo_stage9_collapsed |>
    dplyr::filter(stringr::str_detect(PROG_PAZ, paste0("\\b", id, "\\b")))
  
  pat_orig <- sort(unique(na.omit(as.vector(as.matrix(
    original[, c("COD_PAT1","patol2","patol3","patol4","patol5","patol6")]
  )))))
  
  pat_coll <- sort(unique(na.omit(as.vector(as.matrix(
    collapsed[, c("COD_PAT1","patol2","patol3","patol4","patol5","patol6")]
  )))))
  
  if(!identical(pat_orig, pat_coll)){
    mismatch <- mismatch + 1
  }
}

cat("Mismatch malformazioni:", mismatch, "\n")
cat("Mismatch malformazioni:", mismatch, "\n")

if(mismatch > 0){
  stop("Errore perdita malformazioni nel collasso")
}

cat("OK malformazioni\n")


# check sds

cat("\nCHECK SDS\n")

mismatch_sds <- 0

for(i in 1:nrow(dupProgPaz)){
  
  id <- dupProgPaz$PROG_PAZ[i]
  
  original <- sdo_stage8 |> dplyr::filter(PROG_PAZ == id)
  
  if(length(unique(original$sds_cc)) > 1){
    mismatch_sds <- mismatch_sds + 1
  }
}

cat("Pazienti con SDS multipli:", mismatch_sds, "\n")
cat("Nota SDS prende il primo valore nel collasso\n")


# risultato finale

cat("\nRISULTATO\n")
cat("STAGE 9 OK\n")
