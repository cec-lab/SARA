setwd("/home/imer/works/DI/coorti/2024/SARA-SARAv1.0.1")

baseDir <- getwd()
source(file.path(baseDir,"config.R"), echo = TRUE)

library(readr)
library(dplyr)
library(lubridate)
library(stringr)


# ALIGN TO GOLD ----


auto_align_to_gold <- function(df_test, df_gold, name_test, name_gold) {
  
  cat("\n==============================\n")
  cat("AUTO ALIGN:", name_test, "→", name_gold, "\n")
  cat("==============================\n")
  
  # allinea colonne
  df_test <- df_test %>%
    select(any_of(colnames(df_gold))) %>%
    select(all_of(colnames(df_gold)))
  
  for (col in names(df_gold)) {
    
    gold_col <- df_gold[[col]]
    test_col <- df_test[[col]]
    
    gold_class <- class(gold_col)[1]
    test_class <- class(test_col)[1]
    

    # DATE

    if (inherits(gold_col, "Date")) {
      
      if (!inherits(test_col, "Date")) {
        
        cat("🛠 DATE:", col, "\n")
        
        parsed <- suppressWarnings(dmy(test_col))
        
        idx <- is.na(parsed) & !is.na(test_col)
        if (any(idx)) {
          parsed[idx] <- suppressWarnings(ymd(test_col[idx]))
        }
        
        df_test[[col]] <- parsed
      }
      
      next
    }
    

    # NUMERIC

    if (is.numeric(gold_col)) {
      
      if (!is.numeric(test_col)) {
        
        converted <- suppressWarnings(as.numeric(test_col))
        bad <- sum(is.na(converted) & !is.na(test_col))
        
        if (bad == 0) {
          df_test[[col]] <- converted
          cat("🛠 NUMERIC:", col, "\n")
        } else {
          cat("⚠️ NUMERIC SKIPPED:", col, bad, "\n")
        }
      }
      
      next
    }
    

    # CHARACTER (SAFE)

    if (is.character(gold_col)) {
      
      # forza sempre character
      if (!is.character(test_col)) {
        df_test[[col]] <- as.character(test_col)
        cat("🛠 CHAR:", col, "\n")
      }
      
      gold_vals <- na.omit(gold_col)
      
      is_numeric_code <- length(gold_vals) > 0 &&
        all(grepl("^[0-9]+$", gold_vals))
      
      if (is_numeric_code) {
        
        lengths <- nchar(gold_vals)
        
        if (length(unique(lengths)) == 1) {
          
          target_len <- unique(lengths)
          
          test_vals <- df_test[[col]]
          test_len  <- nchar(na.omit(test_vals))
          
          if (any(test_len != target_len)) {
            
            cat("🛠 PAD:", col, "→", target_len, "\n")
            
            df_test[[col]] <- ifelse(
              is.na(test_vals),
              NA,
              str_pad(test_vals, target_len, pad = "0")
            )
          }
        }
      }
      
      next
    }
    

    # FALLBACK

    if (gold_class != test_class) {
      cat("❌ TYPE MISMATCH:", col, test_class, "vs", gold_class, "\n")
    }
  }
  
  cat("\n✅ ALIGN COMPLETATO\n")
  return(df_test)
}


# FIX FINALE ----

 
force_types_from_gold <- function(df_test, df_gold) {
  
  for (col in names(df_gold)) {
    
    gold_col <- df_gold[[col]]
    
    if (is.character(gold_col)) {
      df_test[[col]] <- as.character(df_test[[col]])
    }
    
    if (is.numeric(gold_col)) {
      df_test[[col]] <- suppressWarnings(as.numeric(df_test[[col]]))
    }
    
    if (inherits(gold_col, "Date")) {
      df_test[[col]] <- as.Date(df_test[[col]])
    }
  }
  
  return(df_test)
}


# CHECK FINALE ----

check_after_align <- function(df_test, df_gold, name) {
  
  cat("\n==============================\n")
  cat("CHECK:", name, "\n")
  cat("==============================\n")
  
  same_cols <- identical(colnames(df_test), colnames(df_gold))
  
  types_test <- sapply(df_test, class)
  types_gold <- sapply(df_gold, class)
  
  diff <- data.frame(
    col = names(types_gold),
    gold = types_gold,
    test = types_test
  ) %>% filter(gold != test)
  
  if (same_cols) cat("✅ Colonne OK\n") else cat("❌ Colonne diverse\n")
  if (nrow(diff)==0) cat("✅ Tipi OK\n") else print(diff)
  
  ok <- same_cols && nrow(diff)==0
  
  if (ok) cat("🎯 PRONTO\n") else cat("🚫 NON CONFORME\n")
  
  return(ok)
}


# LOAD ----


sdo_1yfup_2023 <- read_csv2(
  "/home/imer/works/DI/pipeline_2023/algo_sdo/sdo/sdo_1yfup_2023.csv",
  trim_ws = TRUE
)

cedap_plus_2023 <- read_csv2(
  "/home/imer/works/DI/pipeline_2023/DARIO/cedap/cedap_plus_2023.csv",
  trim_ws = TRUE
)

sdo_1yfup_2024 <- read_csv2(
  file.path(baseDir,"sdo","sdo_1yfup_2024.csv"),
  trim_ws = TRUE
)

cedap_plus_2024_dedup <- read_csv2(
  file.path(baseDir,"cedap","cedap_plus_2024_dedup.csv"),
  trim_ws = TRUE
)

# PIPELINE CEDAP ----

cedap_fixed <- auto_align_to_gold(
  cedap_plus_2024_dedup,
  cedap_plus_2023,
  "CEDAP 2024",
  "GOLD"
)

cedap_fixed <- force_types_from_gold(cedap_fixed, cedap_plus_2023)

ok_cedap <- check_after_align(cedap_fixed, cedap_plus_2023, "CEDAP")

if (ok_cedap) {
  write_delim(
    cedap_fixed,
    file.path(baseDir,"cedap","cedap_plus_2024_dedup.csv"),
    delim=";",
    na=""
  )
  cat("💾 CEDAP salvato\n")
}

# PIPELINE SDO ----

sdo_fixed <- auto_align_to_gold(
  sdo_1yfup_2024,
  sdo_1yfup_2023,
  "SDO 2024",
  "GOLD"
)

sdo_fixed <- force_types_from_gold(sdo_fixed, sdo_1yfup_2023)

ok_sdo <- check_after_align(sdo_fixed, sdo_1yfup_2023, "SDO")

if (ok_sdo) {
  write_delim(
    sdo_fixed,
    file.path(baseDir,"sdo","sdo_1yfup_2024.csv"),
    delim=";",
    na=""
  )
  cat("💾 SDO salvato\n")
}

# DEBUG ----

cat("\n--- STRUTTURA FINALE ---\n")
str(sdo_fixed)
str(cedap_fixed)

cat("\n--- PROBLEMS ---\n")
problems(sdo_1yfup_2024)
problems(cedap_plus_2024_dedup)