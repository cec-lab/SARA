baseDir=getwd()
source(paste0(baseDir,"/config.R"), echo = TRUE)
source(paste0(baseDir,"/functions.R"), echo = TRUE)


eurocatData <- read.csv2(paste0(exportDir, "/eurocatData.csv"))
icd_conversion_table <- read_excel("tables/icd_conversion_table.xlsx")
syndromes <- read_delim("tables/syndromes.csv", delim = ";", trim_ws = TRUE)
redcap_test_data <- read_delim("redcap_dataset/redcap_test_data.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
sdo_1yfup_2023 <- read_delim("sdo/sdo_1yfup_2023.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DARIO ----

align_to_redcap <- function(df_sim, df_real){
  
  # aggiunge colonne mancanti
  missing_cols <- setdiff(names(df_real), names(df_sim))
  
  for(v in missing_cols){
    df_sim[[v]] <- NA
  }
  
  # tiene solo e nell'ordine corretto
  df_sim <- df_sim[, names(df_real)]
  
  return(df_sim)
}
generate_eurocat_test <- function(eurocatData, redcap_test_data, n = 1000, tolerance = 30){
  
  # -------- SEED VARIABILE --------
  seed <- as.integer(Sys.time()) + sample.int(1e6,1)
  set.seed(seed)
  message("Seed usato: ", seed)
  
  nn <- n
  
  # -------- BASE REALISTICA --------
  # campiona dal reale → già struttura buona
  df <- eurocatData[sample(1:nrow(eurocatData), nn, replace = TRUE), ]
  
  # -------- FUNZIONI --------
  rand_choice <- function(values, probs = NULL){
    sample(values, nn, TRUE, prob = probs)
  }
  
  rand_date <- function(){
    as.Date("2000-01-01") + sample(0:9000, nn, TRUE)
  }
  
  # ---------------- ID ----------------
  df$record_id <- paste0("ID", sample(100000:999999, nn, TRUE))
  df$redcap_data_access_group <- sample(paste0("group", 1:14), nn, TRUE)
  df$centre <- 18
  
  # ---------------- DATE ----------------
  df$birth_date <- rand_date()
  df$datemo <- rand_date()
  df$death_date <- rand_date()
  df$agefa <- rand_date()
  
  # ---------------- DEMOGRAFIA ----------------
  df$sex <- rand_choice(c(1,2,3), c(0.49,0.49,0.02))
  df$civreg <- rand_choice(c(1,2,3), c(0.85,0.1,0.05))
  df$mocitizenship <- sample(100:300, nn, TRUE)
  df$resmo <- sprintf("%06d", sample(1:999999, nn, TRUE))
  df$extra_er_resmo <- sample(c("ER1","ER2"), nn, TRUE)
  
  # ---------------- GRAVIDANZA ----------------
  df$gestlength <- sample(20:42, nn, TRUE)
  df$nbrbaby <- rand_choice(c(1,2,3), c(0.9,0.08,0.02))
  
  df$sp_twin <- ifelse(df$nbrbaby > 1,
                       sample(c("MCDA","DCDA"), nn, TRUE),
                       "NO")
  
  df$nbrmalf <- ifelse(df$nbrbaby > 1,
                       sample(c("1","2","3"), nn, TRUE),
                       "0")
  
  # ---------------- OUTCOME ----------------
  df$type <- rand_choice(c(1,2,3,4), c(0.8,0.1,0.05,0.05))
  df$survival <- rand_choice(c(1,2,3), c(0.7,0.2,0.1))
  
  # ---------------- CLINICO ----------------
  df$weight <- sample(500:4500, nn, TRUE)
  df$bmi <- sample(18:40, nn, TRUE)
  df$totpreg <- sample(0:6, nn, TRUE)
  
  df$agedisc <- sample(c("20","22","25","30"), nn, TRUE)
  df$sp_firstpre <- sample(c("info1","info2"), nn, TRUE)
  df$sp_karyo <- sample(c("46XX","46XY"), nn, TRUE)
  df$sp_gentest <- sample(c("array","NGS"), nn, TRUE)
  
  # ---------------- MALFORMAZIONI ----------------
  for(i in 1:6){
    
    keep <- runif(nn) < ifelse(i == 1, 0.85, 0.35)
    
    df[[paste0("malfo", i)]] <- ifelse(keep, sample(1:800, nn, TRUE), 0)
    
    df[[paste0("premal", i)]] <- ifelse(keep, sample(c(1,2), nn, TRUE), 0)
  }
  
  # ---------------- NOMI FAKE ----------------
  df$mother_name <- sample(c("MARIA","ANNA","LUCA","GIULIA","MARCO"), nn, TRUE)
  df$mother_surname <- sample(c("ROSSI","BIANCHI","VERDI","NERI","GIALLO"), nn, TRUE)
  
  # ---------------- ALIGN --------
  df <- align_to_redcap(df, redcap_test_data)
  
  # ---------------- FILL REAL LEVELS --------
  fill_from_real_levels <- function(df_sim, df_real){
    
    for(v in names(df_sim)){
      
      if(v %in% names(df_real)){
        
        real_values <- df_real[[v]]
        real_values <- real_values[!is.na(real_values)]
        
        if(length(real_values) == 0) next
        
        na_idx <- which(is.na(df_sim[[v]]))
        
        if(length(na_idx) > 0){
          df_sim[[v]][na_idx] <- sample(real_values, length(na_idx), TRUE)
        }
      }
    }
    
    return(df_sim)
  }
  
  df <- fill_from_real_levels(df, redcap_test_data)
  
  # ---------------- NA CONTROLLO FINALE --------
  apply_real_na_counts <- function(df_sim, df_real, tolerance){
    
    n <- nrow(df_sim)
    na_real <- colSums(is.na(df_real))
    
    for(v in names(df_sim)){
      
      if(!(v %in% names(na_real))) next
      
      target_real <- na_real[v]
      
      target <- round(runif(1,
                            max(0, target_real - tolerance),
                            min(n - 1, target_real + tolerance)))
      
      # reset
      values <- df_sim[[v]]
      
      # applica NA
      na_idx <- sample(1:n, target)
      values[na_idx] <- NA
      
      df_sim[[v]] <- values
    }
    
    return(df_sim)
  }
  
  df <- apply_real_na_counts(df, redcap_test_data, tolerance)
  
  return(df)
}



apply_real_na_counts <- function(df_sim, df_real, tolerance = 30){
  
  n <- nrow(df_sim)
  na_real <- colSums(is.na(df_real))
  
  for(v in names(df_sim)){
    
    if(!(v %in% names(na_real))) next
    
    target_real <- na_real[v]
    
    # target con tolleranza
    target <- round(runif(1,
                          max(0, target_real - tolerance),
                          min(n, target_real + tolerance)))
    
    # reset colonna (importante!)
    values <- df_sim[[v]]
    
    # scegli dove mettere NA
    na_idx <- sample(1:n, target)
    
    values[na_idx] <- NA
    
    df_sim[[v]] <- values
  }
  
  return(df_sim)
}



redcap_test <- generate_eurocat_test(
  eurocatData,
  redcap_test_data,
  n = 700
)

write_csv2(redcap_test, paste0(exportDir, "/redcap_test.csv"))















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
generate_sdo_test <- function(n = 1000){
  
  # -------- SEED VARIABILE --------
  seed <- as.integer(Sys.time()) + sample.int(1e6,1)
  set.seed(seed)
  message("Seed usato: ", seed)
  
  df <- create_empty_sdo(n)
  
  # -------- DATE --------
  df$dt_nasc <- as.Date("2023-01-01") + sample(0:364, n, TRUE)
  df$dt_amm  <- df$dt_nasc + sample(0:2, n, TRUE)
  df$dt_dim  <- df$dt_amm + sample(0:15, n, TRUE)
  
  death_flag <- runif(n) < 0.05
  df$dt_decesso <- as.Date(NA)
  df$dt_decesso[death_flag] <- df$dt_dim[death_flag]
  
  # -------- DERIVATE --------
  df$GG_DEG <- as.numeric(df$dt_dim - df$dt_amm)
  df$GG_DEGOP <- ifelse(runif(n) < 0.3,
                        pmin(df$GG_DEG, sample(0:5, n, TRUE)),
                        NA)
  
  df$ETA <- 0
  df$ETA_GG <- as.numeric(df$dt_amm - df$dt_nasc)
  df$daysAfterDelivery <- as.numeric(df$dt_dim - df$dt_nasc)
  
  # -------- ANAGRAFICA --------
  df$COD_RG <- sample(sprintf("%03d", c(30,50,70,80,90,100,110,120,130,140)), n, TRUE)
  df$COD_PRES <- paste0("08", sprintf("%04d", sample(1:9999, n, TRUE)))
  
  df$AA_SDO <- 2024
  df$AA_DIM <- 2024
  
  df$PROG_SDO <- sample(1:500000, n, TRUE)
  df$PROG_PAZ <- sample(1:500000, n, TRUE)
  
  df$COD_AZI <- paste0("08", sprintf("%04d", sample(1:9999, n, TRUE)))
  df$COM_RES <- sprintf("%06d", sample(1:999999, n, TRUE))
  
  df$SEX <- sample(c("M","F"), n, TRUE)
  
  # -------- CLINICO --------
  df$PESO_GR <- round(rnorm(n, 3200, 600))
  df$PESO_GR[df$PESO_GR < 500] <- 500
  df$PESO_GR[df$PESO_GR > 6000] <- 6000
  
  df$DRG_RG <- sprintf("%03d", sample(1:500, n, TRUE))
  df$TIPO_DRG <- sample(c("M","C"), n, TRUE)
  
  df$MOD_DIM <- sample(sprintf("%03d", 1:9), n, TRUE)
  df$REGIME_R <- sample(c("001","002"), n, TRUE)
  df$RG_RES <- sample(sprintf("%03d", 1:200), n, TRUE)
  
  # -------- DIAGNOSI --------
  icd_pool <- c(
    "7455","7470","7793","V3000","7750",
    "53081","76527","7608","V290","7742",
    "769","51881","4659"
  )
  
  df$COD_PAT1 <- sample(icd_pool, n, TRUE)
  
  fill_secondary <- function(){
    x <- sample(icd_pool, n, TRUE)
    x[runif(n) < 0.7] <- NA
    x
  }
  
  df$patol2 <- fill_secondary()
  df$patol3 <- fill_secondary()
  df$patol4 <- fill_secondary()
  df$patol5 <- fill_secondary()
  df$patol6 <- fill_secondary()
  
  # -------- PROCEDURE --------
  proc_pool <- c("3899","8872","9396","8965","9059","9921","897")
  
  for(i in 1:11){
    df[[paste0("interv", i)]] <- ifelse(runif(n) < 0.2,
                                        sample(proc_pool, n, TRUE),
                                        NA)
  }
  
  # -------- ALTRE VAR --------
  df$FLAG_PAT <- sample(c("00","005"), n, TRUE)
  df$PROV_RES <- sample(sprintf("%03d", 1:110), n, TRUE)
  df$SUB_COD <- paste0("08", sample(10000:99999, n, TRUE))
  
  df$NEO_TRASF <- 1
  df$MPR <- sample(c("4259","5312","5349"), n, TRUE)
  
  df$ospedale <- sample(c(
    "IRCCS AOU BOLOGNA",
    "OSP. MODENA",
    "OSP. PARMA",
    "OSP. REGGIO",
    "NON DEFINITO"
  ), n, TRUE)
  
  df$cohort <- 2024
  df$cedap_linked <- sample(1:30000, n, TRUE)
  
  # -------- NA CASUALI (CORRETTI) --------
  apply_random_na <- function(df, no_na_vars){
    
    n <- nrow(df)
    
    for(v in names(df)){
      
      if(v %in% no_na_vars) next
      
      x <- df[[v]]
      
      if(inherits(x, "Date")){
        p <- runif(1, 0.01, 0.05)
      } else if(is.numeric(x)){
        p <- runif(1, 0.02, 0.08)
      } else {
        p <- runif(1, 0.05, 0.15)
      }
      
      n_na <- floor(p * n)
      
      if(n_na > 0){
        idx <- sample(1:n, n_na)
        df[[v]][idx] <- NA
      }
    }
    
    return(df)
  }
  
  no_na_vars <- c("COD_PAT1","dt_nasc","dt_amm","dt_dim","SEX","COD_PRES","PROG_SDO")
  
  df <- apply_random_na(df, no_na_vars)
  
  return(df)
}


sdo_test <- generate_sdo_test(70000)

write_csv2(sdo_test, paste0(exportDir, "/sdo_test.csv"))

