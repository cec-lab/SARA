# Caricamento dei dati ----

sdo_stage8 <- fread(paste0(exportDir, "/sdo_stage_8_validated_filters_export.csv"))

# Creazione della colonna 'score' ----

vtMatrix <- as_tibble(str_split_fixed(sdo_stage8$validation_type, "\\|", n=Inf)) |> mutate_if(is.character, as.numeric)

sdo_stage8$score <- apply(vtMatrix, 1, scoreCalculation)

# sdo_stage8_wscore_3 <- sdo_stage8 |> filter(grepl("1", validation_type) | grepl("2", validation_type)) |> mutate(score = 3)
# 
# sdo_stage8_wscore_2 <- sdo_stage8 |> filter(grepl("3", validation_type) & !grepl("1", validation_type) & !grepl("2", validation_type)) |> mutate(score = 2)
# 
# sdo_stage8_wscore_1 <- sdo_stage8 |> filter(grepl("4", validation_type) & !grepl("1", validation_type) & !grepl("2", validation_type) & !grepl("3", validation_type)) |> mutate(score = 1)

sdo_stage8_wscore_nvalid <- sdo_stage8 |> filter(validated == 0)

# sdo_stage8_wscore <- bind_rows(sdo_stage8_wscore_1, sdo_stage8_wscore_2, sdo_stage8_wscore_3)

sdo_stage8_wscore <- sdo_stage8 |> filter(validated == 1)

ft <- as.data.frame.matrix(table(sdo_stage8_wscore$score, sdo_stage8_wscore$validation_type))

hist(sdo_stage8_wscore$score)

sdo_stage8_wscore_top_rank <- sdo_stage8_wscore |> filter(score >= 4)
sdo_stage8_wscore_low_rank <- sdo_stage8_wscore |> filter(score < 4)

# OUT ----

write_csv2(sdo_stage8_wscore, file = paste0(exportDir, "/sdo_stage_8_validated_score_export.csv"))
write_csv2(sdo_stage8_wscore_nvalid, file = paste0(exportDir, "/sdo_stage_8_nvalidated_score_export.csv"))
write_csv2(sdo_stage8_wscore_top_rank, file = paste0(exportDir, "/sdo_stage_8_validated_top_ranked_export.csv"))
write_csv2(sdo_stage8_wscore_low_rank, file = paste0(exportDir, "/sdo_stage_8_validated_low_ranked_export.csv"))
write.csv2(ft, file = paste0(exportDir, "/scores.table_export.csv"), row.names = T)
