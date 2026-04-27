
#elimina le righe che hanno lo stesso prog_paz_neo

cedap <- Cedap_plus_2023 <- read_excel("cedap/Cedap_plus_2023.xlsx")


cedap <- cedap |>
  filter(!is.na(prog_paz_neo))


cedap <- cedap |>
  group_by(prog_paz_neo) |>
  filter(n() == 1) |>
  ungroup()


write_csv2(cedap, paste0(cedapDir, "/cedap_plus_2023_dedup.csv"))



