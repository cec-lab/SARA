
#elimina le righe che hanno lo stesso prog_paz_neo

cedap <- read.csv2("/home/imer/works/database/cedap/cedap_plus_2024.csv")


cedap <- cedap |>
  filter(!is.na(prog_paz_neo))


cedap <- cedap |>
  group_by(prog_paz_neo) |>
  filter(n() == 1) |>
  ungroup()


write_csv2(cedap, paste0(cedapDir, "/cedap_plus_2024_dedup.csv"))



