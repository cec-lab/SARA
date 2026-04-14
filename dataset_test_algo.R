baseDir=getwd()
source(paste0(baseDir,"/config.R"), echo = TRUE)
source(paste0(baseDir,"/functions.R"), echo = TRUE)


eurocatData <- read.csv2(paste0(exportDir, "/eurocatData.csv"))
icd_conversion_table <- read_excel("tables/icd_conversion_table.xlsx")
syndromes <- read_delim("tables/syndromes.csv", delim = ";", trim_ws = TRUE)
redcap_test_data <- read_delim("redcap_dataset/redcap_test_data.csv", 
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
generate_eurocat_test <- function(eurocatData, n = 1000){
  
  set.seed(123)
  
  df <- eurocatData[rep(1, n), ]
  df[] <- NA
  
  nn <- n
  
  rand_date <- function(p_na = 0.1){
    d <- as.Date("2000-01-01") + sample(0:9000, nn, TRUE)
    d[runif(nn) < p_na] <- NA
    d
  }
  
  rand_choice <- function(values, probs){
    sample(values, nn, TRUE, prob = probs)
  }
  
  # ---------------- ID ----------------
  df$record_id <- paste0("ID", sample(100000:999999, nn, TRUE))
  df$redcap_data_access_group <- sample(paste0("group", 1:14), nn, TRUE)
  df$centre <- 18
  df$numloc <- NA
  
  # ---------------- DATE ----------------
  df$birth_date <- rand_date(0)
  df$datemo <- rand_date(0.2)
  df$death_date <- rand_date(0.7)  # 🔧 meno estremo (prima 0.95 rompeva tutto)
  df$agefa <- rand_date(0.3)
  
  # ---------------- DEMOGRAFIA ----------------
  df$sex <- rand_choice(c(1,2,3), c(0.49,0.49,0.02))
  df$civreg <- rand_choice(c(1,2,3), c(0.85,0.1,0.05))
  df$mocitizenship <- sample(100:300, nn, TRUE)
  df$resmo <- sprintf("%06d", sample(1:999999, nn, TRUE))
  df$extra_er_resmo <- sample(c("ER1","ER2", NA), nn, TRUE)
  
  # ---------------- GRAVIDANZA ----------------
  df$gestlength <- sample(20:42, nn, TRUE)
  df$nbrbaby <- rand_choice(c(1,2,3), c(0.9,0.08,0.02))
  
  df$sp_twin <- ifelse(df$nbrbaby > 1,
                       sample(c("MCDA","DCDA", NA), nn, TRUE),
                       NA)
  
  df$nbrmalf <- ifelse(df$nbrbaby > 1,
                       sample(c("1","2","3", NA), nn, TRUE),
                       NA)
  
  # ---------------- OUTCOME ----------------
  df$type <- rand_choice(c(1,2,3,4), c(0.8,0.1,0.05,0.05))
  df$survival <- rand_choice(c(1,2,3), c(0.7,0.2,0.1))
  df$baby_and_mother_complete <- rand_choice(c(1,2), c(0.8,0.2))
  
  # ---------------- CORE CLINICO ----------------
  df$weight <- sample(500:4500, nn, TRUE)
  df$bmi <- sample(18:40, nn, TRUE)
  df$totpreg <- sample(0:6, nn, TRUE)
  
  df$whendisc <- sample(c(1:7,10), nn, TRUE)
  df$agedisc <- sample(c("20","22","25","30", NA), nn, TRUE)
  df$condisc <- sample(c(1,2), nn, TRUE)
  
  df$firstpre <- sample(1:10, nn, TRUE)
  df$sp_firstpre <- sample(c("info1","info2", NA), nn, TRUE)
  
  df$karyo <- sample(c(1,2,3,4,8), nn, TRUE)
  df$sp_karyo <- sample(c("46XX","46XY", NA), nn, TRUE)
  
  df$gentest <- sample(c(1,2,3), nn, TRUE)
  df$sp_gentest <- sample(c("array","NGS", NA), nn, TRUE)
  
  df$surgery <- sample(c(0,1), nn, TRUE)
  
  df$pm <- sample(c(1,2,3,4), nn, TRUE)
  df$pm_notes <- sample(c("note1","note2", NA), nn, TRUE)
  
  df$presyn <- sample(c(1,2), nn, TRUE)
  df$imer_key <- sample(1:5, nn, TRUE)
  
  # ---------------- SINDROME ----------------
  has_syn <- runif(nn) < 0.15
  idx <- sample(1:nrow(syndromes), nn, TRUE)
  
  df$syndrome <- ifelse(has_syn, sample(1:100, nn, TRUE), NA)
  df$syndrome_desc_detail <- ifelse(has_syn, syndromes$Syndrome[idx], NA)
  df$omim <- ifelse(has_syn, sample(100000:999999, nn, TRUE), NA)
  df$orpha <- ifelse(has_syn, sample(1:1000, nn, TRUE), NA)
  
  # ---------------- MALFORMAZIONI ----------------
  for(i in 1:6){
    
    keep <- runif(nn) < ifelse(i == 1, 0.85, 0.35)
    
    df[[paste0("malfo", i)]] <- ifelse(keep, sample(1:800, nn, TRUE), NA)
    
    df[[paste0("malfo", i, "_desc_detail")]] <- ifelse(
      keep,
      sample(icd_conversion_table$Descrizione, nn, TRUE),
      NA
    )
    
    df[[paste0("premal", i)]] <- ifelse(
      keep,
      sample(c(1,2), nn, TRUE),
      NA
    )
  }
  
  # malfo1 sempre piena
  df$malfo1[is.na(df$malfo1)] <- sample(1:800, sum(is.na(df$malfo1)), TRUE)
  
  # ---------------- ESPOSIZIONI ----------------
  df$assconcept <- rand_choice(c(0,1,NA), c(0.8,0.1,0.1))
  df$occupmo <- sample(c(1000:9999, NA), nn, TRUE)
  df$matdiab <- rand_choice(c(1:7, NA), rep(1,8))
  df$illbef1 <- rand_choice(c(1:4, NA), rep(1,5))
  df$illdur1 <- rand_choice(c(1:3, NA), rep(1,4))
  df$folic_g14 <- rand_choice(c(1:5, NA), rep(1,6))
  df$firsttri <- rand_choice(c(1:5, NA), rep(1,6))
  df$drugs1 <- sample(c("N03AF01","A10BA02", NA), nn, TRUE)
  
  # ---------------- SOCIO ----------------
  df$socf <- rand_choice(c(1:9, NA), rep(1,10))
  df$migrant <- sample(c(TRUE, FALSE, NA), nn, TRUE)
  
  # ---------------- FLAGS ----------------
  df$valid_case <- rand_choice(c(0,1), c(0.2,0.8))
  df$diagnosis_complete <- rand_choice(c(1,2,NA), c(0.7,0.2,0.1))
  df$malformations_complete <- rand_choice(c(0,2), c(0.3,0.7))
  df$exposure_complete <- rand_choice(c(1,2,NA), c(0.7,0.2,0.1))
  df$family_history_complete <- rand_choice(c(1,2,NA), c(0.7,0.2,0.1))
  df$record_validation <- rand_choice(c(1,2), c(0.7,0.3))
  df$record_validation_complete <- rand_choice(c(1,2,NA), c(0.7,0.2,0.1))
  
  # ---------------- ALIGN ----------------
  df <- align_to_redcap(df, redcap_test_data)
  
  fill_from_real_levels <- function(df_sim, df_real){
    
    n <- nrow(df_sim)
    
    for(v in names(df_sim)){
      
      if(v %in% names(df_real)){
        
        real_values <- df_real[[v]]
        real_values <- real_values[!is.na(real_values)]
        
        if(length(real_values) == 0) next
        
        # 🔥 RIEMPI anche se quasi vuota (non solo tutta NA)
        na_idx <- which(is.na(df_sim[[v]]))
        
        if(length(na_idx) > 0){
          df_sim[[v]][na_idx] <- sample(real_values, length(na_idx), TRUE)
        }
      }
    }
    
    return(df_sim)
  }
  
  df <- fill_from_real_levels(df, redcap_test_data)
  
  # ---------------- RIEMPI MINIMO ----------------
  fake_names <- c("MARIA","ANNA","LUCA","GIULIA","MARCO")
  fake_surnames <- c("ROSSI","BIANCHI","VERDI","NERI","GIALLO")
  
  df$mother_name <- sample(fake_names, nn, TRUE)
  df$mother_surname <- sample(fake_surnames, nn, TRUE)
  
  
  # ---------------- NA REALI ----------------
  df <- apply_real_na_counts(df, redcap_test_data)
  
  return(df)
}

apply_real_na_counts <- function(df_sim, df_real, tolerance = 30){
  
  n <- nrow(df_sim)
  na_counts <- colSums(is.na(df_real))
  
  for(v in names(na_counts)){
    
    if(v %in% names(df_sim)){
      
      target <- na_counts[v]
      
      # 🔴 CASO 1: 100% NA nel reale → lascia tutto NA
      if(target == n){
        df_sim[[v]] <- NA
        next
      }
      
      # 🔵 CASO 2: normale → applica NA realistici
      
      target <- round(runif(1,
                            max(0, target - tolerance),
                            min(n - 5, target + tolerance)))  # evita 100%
      
      current <- sum(is.na(df_sim[[v]]))
      
      if(current >= target) next
      
      to_add <- target - current
      
      available <- which(!is.na(df_sim[[v]]))
      
      if(length(available) == 0) next
      
      idx <- sample(available, min(to_add, length(available)))
      
      df_sim[[v]][idx] <- NA
    }
  }
  
  return(df_sim)
}

redcap_test <- generate_eurocat_test(eurocatData, n = 700)

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

#FUNZIONI PER CAPIRE TUTTI I LIVELLI DI SDO VERO ----
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
  
  set.seed(123)
  
  df <- create_empty_sdo(n)
  
  rand_na <- function(x, p_na = 0.1){
    x[sample(1:length(x), size = floor(p_na * length(x)))] <- NA
    x
  }
  
  rand_date <- function(p_na = 0.05){
    d <- as.Date("2023-01-01") + sample(0:364, n, TRUE)
    d[runif(n) < p_na] <- NA
    d
  }
  
  df$dt_nasc <- as.Date("2023-01-01") + sample(0:364, n, TRUE)
  df$dt_amm <- df$dt_nasc + sample(0:2, n, TRUE)
  df$dt_dim <- df$dt_amm + sample(0:15, n, TRUE)
  
  death_flag <- runif(n) < 0.05
  df$dt_decesso <- as.Date(NA)
  df$dt_decesso[death_flag] <- df$dt_dim[death_flag]
  
  df$GG_DEG <- as.numeric(df$dt_dim - df$dt_amm)
  df$GG_DEGOP <- ifelse(runif(n) < 0.3,
                        pmin(df$GG_DEG, sample(0:5, n, TRUE)),
                        NA)
  
  df$ETA <- 0
  df$ETA_GG <- as.numeric(df$dt_amm - df$dt_nasc)
  df$daysAfterDelivery <- as.numeric(df$dt_dim - df$dt_nasc)
  
  df$COD_RG <- sample(sprintf("%03d", c(30,50,70,80,90,100,110,120,130,140)), n, TRUE)
  df$COD_PRES <- paste0("08", sprintf("%04d", sample(1:9999, n, TRUE)))
  
  df$AA_SDO <- 2024
  df$AA_DIM <- 2024 #CAMBIARE----
  
  df$PROG_SDO <- sample(1:500000, n, TRUE)
  df$PROG_PAZ <- sample(1:500000, n, TRUE)
  
  df$COD_AZI <- paste0("08", sprintf("%04d", sample(1:9999, n, TRUE)))
  df$COM_RES <- sprintf("%06d", sample(1:999999, n, TRUE))
  
  df$SEX <- sample(c("M","F"), n, TRUE)
  
  df$PESO_GR <- round(rnorm(n, mean = 3200, sd = 600))
  df$PESO_GR[df$PESO_GR < 500] <- 500
  
  df$DRG_RG <- sprintf("%03d", sample(1:500, n, TRUE))
  df$TIPO_DRG <- sample(c("M","C"), n, TRUE)
  
  df$MOD_DIM <- sample(sprintf("%03d", 1:9), n, TRUE)
  df$REGIME_R <- sample(c("001","002"), n, TRUE)
  df$RG_RES <- sample(sprintf("%03d", 1:200), n, TRUE)
  
  icd_pool <- c(
    "7455","7470","7793","V3000","7750",
    "53081","76527","7608","V290","7742",
    "769","51881","4659"
  )
  
  df$COD_PAT1 <- sample(icd_pool, n, TRUE)
  
  fill_secondary <- function(){
    x <- sample(c(icd_pool, NA), n, TRUE)
    x[runif(n) < 0.6] <- NA
    x
  }
  
  df$patol2 <- fill_secondary()
  df$patol3 <- fill_secondary()
  df$patol4 <- fill_secondary()
  df$patol5 <- fill_secondary()
  df$patol6 <- fill_secondary()
  
  proc_pool <- c("3899","8872","9396","8965","9059","9921","897")
  
  for(i in 1:11){
    df[[paste0("interv", i)]] <- sample(c(proc_pool, NA), n, TRUE)
  }
  
  df$FLAG_PAT <- sample(c("00","005"), n, TRUE)
  df$PROV_RES <- sample(sprintf("%03d", 1:110), n, TRUE)
  df$SUB_COD <- paste0("08", sample(10000:99999, n, TRUE))
  
  df$NEO_TRASF <- 1
  
  df$MPR <- sample(c("4259","5312","5349", NA), n, TRUE)
  
  df$ospedale <- sample(c(
    "IRCCS AOU BOLOGNA",
    "OSP. MODENA",
    "OSP. PARMA",
    "OSP. REGGIO",
    "NON DEFINITO"
  ), n, TRUE)
  
  df$cohort <- 2024 #CAMBIARE ----
  df$cedap_linked <- sample(1:30000, n, TRUE)
  
  no_na_vars <- c("COD_PAT1","dt_nasc","dt_amm","dt_dim","SEX","COD_PRES","PROG_SDO")
  
  df <- as.data.frame(lapply(names(df), function(v){
    
    x <- df[[v]]
    
    if(v %in% no_na_vars){
      return(x)
    }
    
    if(is.numeric(x)){
      x[sample(1:n, floor(0.05*n))] <- NA
    } else {
      x[sample(1:n, floor(0.1*n))] <- NA
    }
    
    return(x)
  }))
  
  names(df) <- colnames(create_empty_sdo(1))
  
  return(df)
}


sdo_test <- generate_sdo_test(70000)

write_csv2(sdo_test, paste0(exportDir, "/sdo_test.csv"))

