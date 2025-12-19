


#Data cleaning
#stage0 ----
#stage1 ----
#Stage scan_extra ----








#funzioni pulizia colonne usate in transcode.R (stage_12)

convert_single_date <- function(x) {
  if (is.na(x) || x == "" || tolower(x) == "na") return(NA_character_)
  
  x <- str_trim(x)
  
  # prova conversione Excel numeric
  num_x <- suppressWarnings(as.numeric(x))
  if (!is.na(num_x)) {
    return(as.Date(num_x, origin = "1899-12-30"))
  }
  
  # prova dd/mm/yyyy
  d1 <- suppressWarnings(dmy(x))
  if (!is.na(d1)) return(d1)
  
  # prova yyyy-mm-dd
  d2 <- suppressWarnings(ymd(x))
  if (!is.na(d2)) return(d2)
  
  warning(paste("Data non riconosciuta:", x))
  return(NA_character_)
}

convert_dates_column <- function(col_data) {
  sapply(col_data, function(val) {
    if (is.na(val) || val == "" || tolower(val) == "na") return(NA_character_)
    
    if (grepl("\\|", val)) {
      parts <- unlist(strsplit(val, "\\|"))
      parts_converted <- sapply(parts, function(p) {
        d <- convert_single_date(p)
        if (inherits(d, "Date") && !is.na(d)) {
          format(d, "%Y-%m-%d")
        } else {
          "NA"
        }
      })
      paste(parts_converted, collapse = "|")
    } else {
      d <- convert_single_date(val)
      if (inherits(d, "Date") && !is.na(d)) {
        format(d, "%Y-%m-%d")
      } else {
        NA_character_
      }
    }
  }, USE.NAMES = FALSE)
}

date_cols <- c("dt_amm", "dt_dim", "dt_decesso", "dt_nas_m")

for (col in date_cols) {
  input_df[[col]] <- as.character(input_df[[col]])
  input_df[[col]] <- convert_dates_column(input_df[[col]])
}

# dt_nasc gestione
input_df$dt_nasc <- as.character(input_df$dt_nasc)
input_df$dt_nasc <- sub(" UTC", "", input_df$dt_nasc)
input_df$dt_nasc <- as.Date(input_df$dt_nasc, format = "%Y-%m-%d")

# Controlla i risultati
head(input_df[, c("dt_nasc", date_cols)])








