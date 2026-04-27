rm(list = ls())

baseDir=getwd()
source(paste0(baseDir,"/config.R"), echo = TRUE)
source(paste0(baseDir,"/functions.R"), echo = TRUE)


eurocatData <- read.csv2(paste0(exportDir, "/eurocatData.csv"))
eurocatData <- transcode_complete(eurocatData, eurocat_vars_list)
icd_conversion_table <- read_excel("tables/icd_conversion_table.xlsx")
syndromes <- read_delim("tables/syndromes.csv", delim = ";", trim_ws = TRUE)
redcap_test_data <- read_delim("redcap_dataset/redcap_test_data.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
sdo_1yfup_2023 <- read_delim("stage_0/sdo_2023_all.csv", 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DARIO ----

align_to_redcap <- function(df_sim, df_real){
  
  missing_cols <- setdiff(names(df_real), names(df_sim))
  
  for(v in missing_cols){
    df_sim[[v]] <- NA
  }
  
  df_sim <- df_sim[, names(df_real)]
  
  return(df_sim)
}


generate_eurocat_test <- function(eurocatData, redcap_test_data, n_sim = 1000){
  
  seed <- as.integer(Sys.time()) + sample.int(1e6,1)
  set.seed(seed)
  message("Seed usato: ", seed)
  
  # ---------------- BASE ----------------
  df <- redcap_test_data[rep(1, n_sim), ]
  df[,] <- NA
  
  # ---------------- DATE ----------------
  rand_date <- function(){
    as.Date("2000-01-01") + sample(0:9000, n_sim, TRUE)
  }
  
  df$birth_date <- rand_date()
  df$datemo     <- df$birth_date + sample(0:2, n_sim, TRUE)
  df$agefa      <- rand_date()
  
  death_flag <- runif(n_sim) < 0.1
  df$death_date <- as.Date(NA)
  
  idx_death <- which(death_flag)
  if(length(idx_death) > 0){
    df$death_date[idx_death] <- df$datemo[idx_death] +
      sample(0:100, length(idx_death), TRUE)
  }
  
  # ---------------- ID ----------------
  df$record_id <- paste0("ID", sample(100000:999999, n_sim, TRUE))
  df$redcap_data_access_group <- sample(paste0("group", 1:14), n_sim, TRUE)
  df$centre <- 18
  
  # ---------------- NOMI ----------------
  df$mother_name <- sample(
    c("MARIA","ANNA","GIULIA","FRANCESCA","LAURA","CHIARA"),
    n_sim, TRUE
  )
  
  df$mother_surname <- sample(
    c("ROSSI","BIANCHI","VERDI","NERI","GIALLO","FERRARI","ESPOSITO"),
    n_sim, TRUE
  )
  
  # ---------------- ALTRE VAR (PRIMA) ----------------
  
  vars_protette_base <- c(
    "record_id","redcap_data_access_group","centre",
    "birth_date","datemo","death_date","agefa",
    "mother_name","mother_surname"
  )
  
  vars_other <- setdiff(names(df), vars_protette_base)
  
  for(v in vars_other){
    
    if(!(v %in% names(eurocatData))) next
    
    pool <- eurocatData[[v]]
    pool_no_na <- pool[!is.na(pool)]
    
    if(length(pool_no_na) == 0) next
    
    df[[v]] <- sample(pool_no_na, n_sim, replace = TRUE)
  }
  
  # ---------------- SDO NUMBER ----------------
  
  if("sdo_number" %in% names(df)){
    
    pool <- eurocatData$sdo_number
    pool <- pool[!is.na(pool) & pool != ""]
    
    df$sdo_number <- sample(pool, n_sim, replace = TRUE)
  }
  
  # ---------------- BLOCCHI COERENTI (ULTIMO STEP 🔥) ----------------
  
  blocchi <- list(
    
    c("diagnosis_syndrome","presyn",
      paste0("diagnosis_malformation_",1:8),
      paste0("premal",1:8)),
    
    c(paste0("malfo",1:8),
      paste0("malfo",1:8,"_desc_detail"),
      paste0("sp_malfo",1:8)),
    
    c(paste0("drugs",1:5),
      "sp_ifnotlisted_medication",
      paste0("sp_ifnotlisted_medication_",2:5),
      paste0("sp_drugs", c("", "_2","_3","_4","_5")))
  )
  
  idx_blocco <- sample(1:nrow(redcap_test_data), n_sim, replace = TRUE)
  
  for(vars_blocco in blocchi){
    
    vars_blocco <- intersect(vars_blocco, names(redcap_test_data))
    
    df[, vars_blocco] <- redcap_test_data[idx_blocco, vars_blocco]
  }
  
  # ---------------- NA REALISTICI ----------------
  
  na_prop <- colMeans(is.na(redcap_test_data))
  
  no_na_vars <- c(
    "record_id","sdo_number","mother_name","mother_surname"
  )
  
  for(v in names(df)){
    
    if(v %in% no_na_vars) next
    if(!(v %in% names(na_prop))) next
    
    p <- na_prop[v]
    if(p == 0) next
    
    n_na <- round(p * n_sim)
    
    if(n_na > 0){
      idx_na <- sample(1:n_sim, n_na)
      df[[v]][idx_na] <- NA
    }
  }
  
  # ---------------- ORDINE ----------------
  
  df <- df[, names(redcap_test_data)]
  
  return(df)
}

apply_real_na_counts <- function(df_sim, df_real, tolerance = 30){
  
  n <- nrow(df_sim)
  na_real <- colSums(is.na(df_real))
  
  # 🔒 colonne che NON devono avere NA
  no_na_vars <- c("sdo_number","record_id","numloc")
  
  for(v in names(df_sim)){
    
    #SALTA colonne protette
    if(v %in% no_na_vars) next
    
    if(!(v %in% names(na_real))) next
    
    target_real <- na_real[v]
    
    target <- round(runif(1,
                          max(0, target_real - tolerance),
                          min(n, target_real + tolerance)))
    
    values <- df_sim[[v]]
    
    na_idx <- sample(1:n, target)
    values[na_idx] <- NA
    
    df_sim[[v]] <- values
  }
  
  return(df_sim)
}

redcap_test <- generate_eurocat_test(
  eurocatData,
  redcap_test_data,
  n_sim = 700
)

write_csv2(redcap_test,"~/Desktop/git_hub/dataset_test_casuali/redcap_test.csv")




























#SARA----

create_empty_sdo <- function(n = 1000){
  
  cols <- c(
    "COD_RG","COD_PRES","AA_SDO","PROG_SDO","AA_DIM","COD_AZI",
    "COM_RES","dt_amm","dt_dim","COD_DISD","DRG_RG","TIPO_DRG",
    "ETA","ETA_GG","GG_DEG","GG_DEGOP","MOD_DIM","MPR",
    "NEO_TRASF","COD_PAT1","FLAG_PAT","PESO_GR","PROV_RES",
    "REGIME_R","RG_RES","SEX","SUB_COD","PROG_PAZ",
    "patol2","patol3","patol4","patol5","patol6",
    "interv1","interv2","interv3","interv4","interv5",
    "interv6","interv7","interv8","interv9","interv10","interv11",
    "ospedale","dt_nasc","dt_decesso","cohort",
    "cedap_linked","daysAfterDelivery"
  )
  
  df <- as.data.frame(matrix(NA, nrow = n, ncol = length(cols)))
  colnames(df) <- cols
  
  return(df)
}

get_levels <- function(df, max_show = 10){
  
  res <- lapply(names(df), function(v){
    
    x <- df[[v]]
    
    vals <- unique(x)
    vals <- vals[!is.na(vals)]
    
    list(
      var = v,
      n_unique = length(vals),
      sample_values = paste(head(vals, max_show), collapse = " | ")
    )
  })
  
  res <- do.call(rbind, lapply(res, as.data.frame))
  rownames(res) <- NULL
  
  return(res)
}


get_levels_full <- function(df, max_show = 10){
  
  res <- lapply(names(df), function(v){
    
    x <- df[[v]]
    
    vals <- unique(x)
    vals <- vals[!is.na(vals)]
    
    data.frame(
      var = v,
      class = class(x)[1],
      n_unique = length(vals),
      sample_values = paste(head(vals, max_show), collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
  
  res <- do.call(rbind, res)
  rownames(res) <- NULL
  
  return(res)
}


#ANALISI LIVELLI ----
sdo_test <- create_empty_sdo(70000)

levels_sdo <- get_levels_full(sdo_1yfup_2023)



#CREAZIONE DATASET TEST ----
generate_sdo_test <- function(n_sim = 1000){
  
  seed <- as.integer(Sys.time()) + sample.int(1e6,1)
  set.seed(seed)
  message("Seed usato: ", seed)
  
  # ---------------- BASE ----------------
  df <- create_empty_sdo(n_sim)
  
  # RIMUOVI cedap_linked
  df$cedap_linked <- NULL
  
  # ---------------- BLOCCO REALE (CHIAVE 🔥) ----------------
  # campioni righe reali → tutto coerente
  idx <- sample(1:nrow(sdo_1yfup_2023), n_sim, replace = TRUE)
  
  df$PROG_PAZ   <- sdo_1yfup_2023$PROG_PAZ[idx]
  df$dt_nasc    <- sdo_1yfup_2023$dt_nasc[idx]
  df$dt_amm     <- sdo_1yfup_2023$dt_amm[idx]
  df$dt_dim     <- sdo_1yfup_2023$dt_dim[idx]
  df$dt_decesso <- sdo_1yfup_2023$dt_decesso[idx]
  
  df$GG_DEG <- sdo_1yfup_2023$GG_DEG[idx]
  df$ETA_GG <- sdo_1yfup_2023$ETA_GG[idx]
  
  # ---------------- BLOCCO CLINICO ----------------
  vars_blocco <- c(
    "COD_PAT1","patol2","patol3","patol4","patol5","patol6",
    paste0("interv",1:11)
  )
  
  vars_blocco <- intersect(vars_blocco, names(sdo_1yfup_2023))
  df[, vars_blocco] <- sdo_1yfup_2023[idx, vars_blocco]
  
  # ---------------- ALTRE VAR ----------------
  vars_protette <- c(
    "dt_nasc","dt_amm","dt_dim","dt_decesso",
    "GG_DEG","ETA_GG",
    vars_blocco,
    "PROG_PAZ"
  )
  
  vars_other <- setdiff(names(df), vars_protette)
  
  for(v in vars_other){
    
    if(!(v %in% names(sdo_1yfup_2023))) next
    
    pool <- sdo_1yfup_2023[[v]]
    pool <- pool[!is.na(pool)]
    
    if(length(pool) == 0) next
    
    df[[v]] <- sample(pool, n_sim, replace = TRUE)
  }
  
  # ---------------- NA REALISTICI ----------------
  no_na_vars <- names(colSums(is.na(sdo_1yfup_2023)))[
    colSums(is.na(sdo_1yfup_2023)) == 0
  ]
  
  vars_no_na_allowed <- c(
    "dt_nasc","dt_amm","dt_dim","dt_decesso",
    "GG_DEG","ETA_GG"
  )
  
  for(v in names(df)){
    
    if(v %in% no_na_vars) next
    if(v %in% vars_no_na_allowed) next
    
    p <- runif(1,0.02,0.1)
    n_na <- floor(p * n_sim)
    
    if(n_na > 0){
      idx_na <- sample(1:n_sim, n_na)
      df[[v]][idx_na] <- NA
    }
  }
  
  # ---------------- NA dt_nasc (per Stage 0) ----------------
  
  n_na <- floor(0.05 * n_sim)
  
  idx_linkable <- which(df$PROG_PAZ %in% cedap$prog_paz_neo)
  
  n_linkable_na <- min(length(idx_linkable), floor(n_na * 0.5))
  
  idx_na_linkable <- sample(idx_linkable, n_linkable_na)
  
  idx_all <- setdiff(1:n_sim, idx_na_linkable)
  idx_na_nonlinkable <- sample(idx_all, n_na - n_linkable_na)
  
  idx_na <- c(idx_na_linkable, idx_na_nonlinkable)
  
  df$dt_nasc[idx_na] <- NA
  
  # forza alcuni linkabili
  idx_na_all <- which(is.na(df$dt_nasc))
  n_force <- floor(length(idx_na_all) * 0.5)
  
  if(n_force > 0){
    idx_force <- sample(idx_na_all, n_force)
    df$PROG_PAZ[idx_force] <- sample(cedap$prog_paz_neo, n_force, replace = TRUE)
  }
  
  # ---------------- ORDINE ----------------
  df <- df[, colnames(sdo_1yfup_2023)[colnames(sdo_1yfup_2023) != "cedap_linked"]]
  
  return(df)
}

sdo_test <- generate_sdo_test(70000)

#controllo per cedap_linked: table(sdo_test$PROG_PAZ %in% cedap$prog_paz_neo)


write_csv2(sdo_test,"~/Desktop/git_hub/dataset_test_casuali/sdo_test.csv")

