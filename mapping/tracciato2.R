# =============================
# STEP 1 - MAPPING BASE MANUALE
# =============================
rm(list=ls())

# LOAD CONFIG ---
source("/home/imer/works/algo_sdo/config.R", echo = T)
source("/home/imer/works/algo_sdo/functions.R", echo = T)

#LOAD DATA ----

sdo_cedap_dd <-read_csv2(paste0(tableDir,"/sdo_cedap_dd.csv"))
eurocat_data_dict <- read_excel(paste0(tableDir,"/eurocat_data_dict.xlsx"))
cedap_dict <- read_excel(paste0(tableDir,"/cedap_tabella_standardizzata.xlsx"))
eurocat_sdo_mapping <-read_excel(paste0(tableDir,"/eurocat_sdo_mapping.xlsx"))

mapping_data <- tribble(
  ~EUROCAT_VARIABLE, ~SDO_CEDAP_DD, ~RICODIFICARE, ~CALCOLARE, ~NOTE,
  "sex", "SEX", 1, 0, "Ricodificare, vedere nel dd",
  "birth_date", "dt_nasc", 0, 0, "Mapping diretto",
  "weight", "PESO_GR", 0, 0, "Mapping diretto",
  "prog_paz", "PROG_PAZ", 0, 0, "Mapping diretto",
  "sds_cc", "sds_cc", 0, 0, "Mapping diretto",
  "agedisc", "ETA_GESTAZIONALE_ALLA_DIAGNOSI", 0, 0, "Mapping diretto",
  "mocitizenship", "CITTADINANZA_M", 0, 0, "Mapping diretto",
  "consang", "CONSANGUINEITA", 1, 0, "guardare nel cedap",
  "agefa", "dt_nas_p", 0, 1, "Da calcolare: differenza tra anno corrente e data_nascita_padre",
  "mo_smoking", "ABITUDINE_AL_FUMO", 0, 0, "Codifica come cedap",
  "malfo1", "COD_PAT1", 0, 0, "Mapping diretto",
  "malfo2", "patol2", 0, 0, "Mapping diretto",
  "malfo3", "patol3", 0, 0, "Mapping diretto",
  "malfo4", "patol4", 0, 0, "Mapping diretto",
  "malfo5", "patol5", 0, 0, "Mapping diretto",
  "malfo6", "patol6", 0, 0,"Mapping diretto",
  "totpreg", "CONCEPIMENTI_PRECEDENTI", 1, 0, "in eurocat, se sono 3 o piu scrive sempre 3",
  "circ_cran_neo", "CIRCONFERENZA_CRANICA", 0, 0, "Mapping diretto",
  "region_res_mo", "RG_RES", 0, 0, "Mapping diretto(extra_codes del cedap)",
  
  "socm", "CONDIZIONE_PROF_MADRE", 1, 0, "ricodifica dal cedap:|cifre
1.occupata
2. disoccupata
3. in cerca di prima occupazione
4.studentessa
5.casalinga
6.altra condizione (ritirata dal lavoro, inabile, ecc)
|| cifra (se occupata):
1.imprenditrice o Ibera professionista
2. altra lavoratrice autonoma
3. lavoratrice dipendente: dirigente o direttiva
4.lavoratrice dipendente: impiegata
5.lavoratrice dipendente: operaia
6. altra lavoratrice dipendente

||| cifra (se occupata):
1. agricoltura, caccia e pesca
2. industria
3.commercio, pubblici servizi, alberghi
4. pubblica amministrazione
5.altri servizi privati",
  
  "sp_karyo", "CARIOTIPO_DEL_NATO", 0, 0, "Mapping diretto",
  "karyo", NA, 0, 1, "Da calcolare: 1 se presente cariotipo, altrimenti not known",
  "illbef1", "MALATTIA_PRINCIPALE_DELLA_MADRE", 0, 0, "",
  "illbef2", "ALTRA_MALATTIA_DELLA_MADRE", 0, 0, "",
  "illdur1", "MALATTIE_INSORTE_IN_GRAVIDANZA_1", 0, 0, "",
  "illdur2", "MALATTIE_INSORTE_IN_GRAVIDANZA_2", 0, 0, "",
  "moanom", "MALFORMAZIONI_PARENTI_MADRE", 1, 0, "se è no mettiamo no, altrimenti not known.fine",
  "faanom", "MALFORMAZIONI_PARENTI_PADRE", 1, 0, "se è no mettiamo no, altrimenti not known.fine",
  "sibanom", "MALFORMAZIONI_FRATELLI_SORELLE", 1, 0, "se è no mettiamo no, altrimenti not known",
  "sdo_number", "SDO_NEO", 0, 0, "",
  "agemo", "dt_nas_m", 0, 1, "Da calcolare: età madre da data_nascita_madre",
  "datemo", "dt_nas_m", 0, 0, "",
  "resmo", "COM_RES", 0, 0, "",
  "centre", "18", 0, 1, "Valore fisso 18",
  "type", "VITALITA", 1, 0, "mapping diretto, ma solo per 1(nato vivo) e 2(nato morto).",
  "survival", NA, 0, 1, "Da calcolare:numero di giorni da data di nascita e data decesso e se data decesso prima di 7 giorni allora NO, altrimenti SI",
  "whendisc", "ETA_GESTAZIONALE_ALLA_DIAGNOSI", 1, 0, "Da calcolare da età gestazionale",
  "condisc", NA, 0, 1, "Default = 'Alive'. Condizione alla dimissione",
  "firstpre", NA, 0, 1, "Lasciare vuoto",
  "gentest", NA, 0, 1, "Inizializzare come 'non performed",
  "surgery", NA, 1, 0, "compilare alla fine, se la chirurgia è stata validata",
  "assconcept", "metodi_PMA", 1, 0, "Prendiamo il codice dal corso di dicembre",
  "socf", "CONDIZIONE_PROF_PADRE", 1, 0, "ricodifica dal cedap. fare lo stesso per la madre.",
  "cod_pres", "COD_PRES", 0, 0, "",
  "source", "2", 0, 1, "Valore fisso",
  "nbrbaby", NA, 0, 1, "Calcolato: somma nati_femmine + nati_maschi(indica numero di bambini per ogni parto).",
  "civreg", NA, 0, 1,"=1 se VITALITA è 1, ALTRIMENTI NOT KNOWN",
  "bmi",NA,0,1,"calcolare",
  "pm","riscontro_autoptico", 1, 0, "compilare con i casi dei referenti, per gli altri solo se vitalita=2",
  "presyn", NA, 0, 0, "mettere Not Known nel tracciato",
  "premal1", NA, 0, 0, "mettere Not Known nel tracciato",
  "premal2", NA, 0, 0, "mettere Not Known nel tracciato",
  "premal3", NA, 0, 0, "mettere Not Known nel tracciato",
  "premal4", NA, 0, 0, "mettere Not Known nel tracciato",
  "premal5", NA, 0, 0, "mettere Not Known nel tracciato",
  "premal6", NA, 0, 0, "mettere Not Known nel tracciato",
  "premal7", NA, 0, 0, "mettere Not Known nel tracciato",
  "premal8", NA, 0, 0, "mettere Not Known nel tracciato",
  "syndrome", NA, 1, 0, "codice scritto in label.R, non c'è nel cedap. Bisogna vedere se tra i codici di malfo c'è uno che appartiene alle sindromi(tabelle eurocat)",
  "sp_syndrome", NA, 1, 0, " da syndromes.csv nello script label.R",
  "sp_malfo1", "COD_PAT1_label", 0, 0, "etichetta già presente, inserita nello step label.R(caricarlo prima)",
  "sp_malfo2", "patol2_label", 0, 0, "etichetta già presente, inserita nello step label.R(caricarlo prima)",
  "sp_malfo3","patol3_label", 0, 0, "etichetta già presente, inserita nello step label.R(caricarlo prima)",
  "sp_malfo4","patol4_label", 0, 0, "etichetta già presente, inserita nello step label.R(caricarlo prima)",
  "sp_malfo5","patol5_label", 0, 0, "etichetta già presente, inserita nello step label.R(caricarlo prima)",
  "sp_malfo6","patol6_label", 0, 0, "etichetta già presente, inserita nello step label.R(caricarlo prima)",
  "matdiab", NA,1,0,"vedere se illbef1 o 2 contengono il codice icd9 del diabete ",
  "matedu", "TITOLO_DI_STUDIO_MADRE", 1, 0, "ricodificare dal cedap",
  "gestlength","eta_gestazionale", 0, 0, "mapping diretto",
  "NA", "nati_maschi", 0,1, "usate per calcolare nbrbaby",
  "NA", "nati_femmine", 0,1, "usate per calcolare nbrbaby",
  "NA", "NEO_TRASF", 0,0, "tenerlo nel tracciato, serve anche per evitare duplicati",
  "NA", "mo_alcohol", 0,0, "tenere perché abbiamo i dati. Inserirla come variabile su Redcap",
  "death_date", "dt_decesso",0,0,"mapping diretto",
  "assoconcept", "pma", 1,0, "ricodifica da cedap"

  #aggiungere colonna "origine" se proviene da sdo o cedap.  aggiungere la colonna di "codifica di destinazione"(prendere dd di eurocat_data_dic) e la "codifica di origine" dalla circolare cedap
  #lavorare con le etichette per recuperare la ricodifica di eurocat. per il cedap fare manualmente
#aggiungere l'id di eurocat come è fatto in eurocat_sdo_mapping. valutare alla fine

#NB. CONTROLLARE EXTRA VARIABLES CON MARCO LUNEDI
  
  
)

# =============================
# STEP 2 - MAPPING COMPLETO
# =============================

# Tutte le variabili SDO disponibili
sdo_vars <- c(
  "cedap_linked", "COD_STAB", "COD_RG", "COD_PRES", "AA_SDO", "PROG_SDO", "SDO_MADRE", "SDO_NEO", "AA_DIM", "COD_AZI",
  "COM_RES", "dt_amm", "dt_dim", "COD_DISD", "DRG_RG", "TIPO_DRG", "ETA", "ETA_GG", "GG_DEG", "GG_DEGOP", "MOD_DIM",
  "MPR", "NEO_TRASF", "COD_PAT1", "FLAG_PAT", "PESO_GR", "PROV_RES", "REGIME_R", "RG_RES", "SEX", "SUB_COD", "PROG_PAZ",
  "prog_paz_m", "patol2", "patol3", "patol4", "patol5", "patol6", "interv1", "interv2", "interv3", "interv4", "interv5",
  "interv6", "interv7", "interv8", "interv9", "interv10", "interv11", "ospedale", "dt_nasc", "cohort", "daysAfterDelivery",
  "validated", "validation_type", "eta_gestazionale", "CIRCONFERENZA_CRANICA", "CONCEPIMENTI_PRECEDENTI", "sds_cc",
  "violazione_filtro", "malformazione_tipo", "score", "nati_femmine", "nati_maschi", "riscontro_autoptico", 
  "Genere_del_parto", "data_nascita_padre", "ALTRA_MALATTIA_DELLA_MADRE", "STATO_CIVILE_MADRE",
  "MALATTIA_PRINCIPALE_DELLA_MADRE", "metodi_PMA", "ECOGRAFIA_OLTRE22SETTIMANE", "FETOSCOPIA", "VILLOCENTESI",
  "AMNIOCENTESI", "TEST_COMBINATO", "CITTADINANZA_M", "VITALITA", "NUMERO_ECOGRAFIE", "CONSANGUINEITA", "dt_nas_m",
  "PESO_MADRE_AL_PARTO", "PESO_MADRE_PREGRAVIDICO", "ALTEZZA_MADRE", "ABITUDINE_AL_FUMO", "MALATTIE_INSORTE_IN_GRAVIDANZA_1",
  "MALATTIE_INSORTE_IN_GRAVIDANZA_2", "MALFORMAZIONI_MADRE", "MALFORMAZIONI_PADRE", "MALFORMAZIONI_GENITORI_MADRE",
  "MALFORMAZIONI_GENITORI_PADRE", "MALFORMAZIONI_PARENTI_MADRE", "MALFORMAZIONI_PARENTI_PADRE", 
  "MALFORMAZIONI_FRATELLI_SORELLE", "ETA_GESTAZIONALE_ALLA_DIAGNOSI", "CARIOTIPO_DEL_NATO",
  "CONDIZIONE_PROF_PADRE", "TITOLO_DI_STUDIO_PADRE", "CONDIZIONE_PROF_MADRE", "TITOLO_DI_STUDIO_MADRE", "datemo", "eta_gestazionale"
)

# Tutte le variabili EUROCAT attese
eurocat_vars_complete <- unique(c(
  mapping_data$EUROCAT_VARIABLE,
  "record_id", "centre", "numloc", "birth_date", "sdo_number", "gestlength", "nbrbaby", "sp_twin", "nbrmalf",
  "sex", "type", "civreg", "weight", "survival", "death_date", "mocitizenship", "resmo", "extra_er_resmo",
   "agemo", "bmi", "totpreg", "whendisc", "agedisc", "condisc", "firstpre", "sp_firstpre", "karyo",
   "gentest", "sp_gentest", "surgery", "pm", "presyn", "premal1", "premal2", "premal3", "premal4",
  "premal5", "premal6", "premal7", "premal8", "imer_key", "syndrome", "sp_syndrome", "omim", "orpha", "malfo1",
  "sp_malfo1", "malfo2", "sp_malfo2", "malfo3", "sp_malfo3", "malfo4", "sp_malfo4", "malfo5", "sp_malfo5",
  "malfo6", "sp_malfo6", "malfo7", "sp_malfo7", "malfo8", "sp_malfo8", "assconcept", "agefa", "socf", "occupmo",
  "matdiab", "illbef1", "illbef2", "illdur1", "illdur2", "folic_g14", "extra_drugs", "firsttri", "drugs1",
  "sp_drugs", "drugs2", "sp_drugs_2", "drugs3", "sp_drugs_3", "drugs4", "sp_drugs_4", "drugs5", "sp_drugs_5",
  "inf_cov_test", "imm_cov_test", "oth_cov_test", "sp_oth_cov_test", "start_cov", "cov_severity", "consang",
  "sp_consang", "sibanom", "sp_sibanom", "prevsib", "sib1", "sib2", "sib3", "moanom", "sp_moanom", "faanom",
  "sp_faanom", "matedu", "socm", "migrant", "genrem", "prog_paz", "region_res_mo", "cod_pres", "lung_neo",
  "circ_cran_neo", "sds_cc", "pre_sa", "pre_topfa", "pre_live", "pre_still", "mo_smoking", "mo_alcohol", "source"
))

# Variabili SDO non utilizzate (escludendo quelle mappate)
sdo_unmapped_df <- tibble(SDO_CEDAP_DD = sdo_vars) %>%
  filter(!SDO_CEDAP_DD %in% mapping_data$SDO_CEDAP_DD) %>%
  mutate(
    EUROCAT_VARIABLE = NA_character_,
    RICODIFICARE = NA_integer_,
    CALCOLARE = NA_integer_,
    NOTE = "Non mappata"
  )

# Variabili EUROCAT non utilizzate (escludendo quelle mappate)
eurocat_non_mapped_df <- tibble(EUROCAT_VARIABLE = eurocat_vars_complete) %>%
  filter(!EUROCAT_VARIABLE %in% mapping_data$EUROCAT_VARIABLE) %>%
  mutate(
    SDO_CEDAP_DD = NA_character_,
    RICODIFICARE = NA_integer_,
    CALCOLARE = NA_integer_,
    NOTE = "Non mappata"
  )


# Mappate ma con SDO mancanti
eurocat_na_sdo_df <- mapping_data %>%
  filter(is.na(SDO_CEDAP_DD)) %>%
  mutate(NOTE = ifelse(NOTE == "", "Non mappata (NA in SDO)", NOTE))

# Mapping finale
final_mapping <- bind_rows(
  mapping_data %>% filter(!is.na(SDO_CEDAP_DD)),
  eurocat_na_sdo_df,
  eurocat_non_mapped_df,
  sdo_unmapped_df
)


# Lavoro 1 - aggiungi origine da sdo_cedap_dd
final_mapping <- final_mapping %>%
  left_join(
    sdo_cedap_dd %>%
      select(SDO_CEDAP_DD = Tracciato, Origine),
    by = "SDO_CEDAP_DD"
  )

# Lavoro 2 - recupero codifica di destinazione da eurocat_data_dict
final_mapping <- final_mapping %>%
  left_join(eurocat_data_dict %>%
              select(`Variable / Field Name`, `Choices, Calculations, OR Slider Labels`) %>%
              rename(EUROCAT_VARIABLE = `Variable / Field Name`,
                     `Codifica di destinazione` = `Choices, Calculations, OR Slider Labels`),
            by = "EUROCAT_VARIABLE")


#Lavoro3- recupero codifica cedap da circolarer 2015

# === 2. Crea dizionario di mapping manuale: variabile -> etichetta CAMPO ===
mappa_descrizioni <- tribble(
  ~SDO_CEDAP_DD, ~`codifica di origine`,
  "SEX", "Indicare il sesso del neonato in base ai genitali esterni: 1 = maschio, 2 = femmina.3 = indeterminato",
  
  "dt_nasc", "Formato gammaaaa (giorno mese, anno)",
  
  "PESO_GR", "Indicare il peso del neonato in grammi.",
  
  "PROG_PAZ", "Il campo deve contenere un progressivo che rappresenta l'identificativo univoco del neonato nell'ambito del numero scheda.",
  
  "ETA_GESTAZIONALE_ALLA_DIAGNOSI", "Età gestazionale al momento della diagnosi della malformazione, espressa in settimane compiute.",
  
  "CITTADINANZA_M", "Codice ISTAT a 3 cifre dello Stato di cittadinanza (*).
Per le cittadine italiane codificare 100.
Per le apolidi codificare 999.",
  
  "CONSANGUINEITA", "Nel caso di consanguineità tra i genitori indicare se: 1.sono parenti di 4' grado; 2. sono parenti di 5' grado, sono parenti di 6' grado. in caso di non consang. compilare a spazi",
  
  "dt_nas_p", "Data di nascita del padre(ggmmaaaa).",
  
  "ABITUDINE_AL_FUMO", "Codice a 2 caratteri per individuare l'abitudine al fumo della donna prima della gravidanza (I cifra) e l'eventuale modifica di tale abitudine durante la gravidanza (II cifra).
La II cifra va compilata solo nei casi in cui la I cifra sia 1 (Sì).
I cifra – Abitudine al fumo nei 5 anni precedenti la gravidanza:
Sì
No
Se Sì, specificare (II cifra):
Ha smesso prima della gravidanza
Ha smesso a inizio gravidanza
Ha continuato a fumare in gravidanza",
  
  "COD_PAT1", "Codice ICD-9-CM della malattia o condizione morbosa principale del feto.",
  "patol2", "Seconda malformazione diagnosticata (ICD-9-CM).",
  "patol3", "Terza malformazione diagnosticata (ICD-9-CM).",
  "patol4", "Quarta malformazione diagnosticata (ICD-9-CM).",
  "patol5", "Quinta malformazione diagnosticata (ICD-9-CM).",
  "patol6", "Sesta malformazione diagnosticata (ICD-9-CM).",
  
  "CONCEPIMENTI_PRECEDENTI", "Indicare se la donna ha avuto, prima del presente parto, precedenti concepimenti: 1.Sì, 2.No",
  
  "CIRCONFERENZA_CRANICA", "Indicare la circonferenza cranica del neonato in centimetri.",
  
  "RG_RES", "Codice ISTAT del comune di residenza.
Nel caso di madre residente in Paese straniero, indicare 999 seguito dalle tre cifre del codice dello Stato estero (*).
Nel caso di madre senza fissa dimora, codificare 999999.
Obbligatorio, salvo il caso di donna che non vuole fornire dati (privacy – 203).",
  
  "CONDIZIONE_PROF_MADRE", "Condizione professionale/non professionale della madre. Codifiche da manuale CEDAP.",
  
  "CARIOTIPO_DEL_NATO", "se effettuata, specificare per esteso la diagnosi citogenetica.Scrivere normale in caso di assenza di anomalie cromosomiche",
  
  "MALATTIA_PRINCIPALE_DELLA_MADRE", "Codice ICD-9 della patologia principale della madre.",
  "ALTRA_MALATTIA_DELLA_MADRE", "Codice ICD-9 di eventuali altre malattie della madre.",
  
  "MALATTIE_INSORTE_IN_GRAVIDANZA_1", "Codice della  malattia rilevante insorta durante la gravidanza.",
  
  "MALATTIE_INSORTE_IN_GRAVIDANZA_2", "Codice altra malattia insorta durante la gravidanza.",
  
  "MALFORMAZIONI_PARENTI_MADRE", "Eventuali malformazioni presenti nei parenti della madre.1 = SI, 2 = NO",
  
  "MALFORMAZIONI_PARENTI_PADRE", "Eventuali malformazioni presenti nei parenti del padre.1 = SI, 2 = NO",
  
  "MALFORMAZIONI_FRATELLE_SORELLE", "Eventuali malformazioni presenti in fratelli o sorelle.1 = SI, 2 = NO",
  
  "SDO_NEO", "Indicare il no della Scheda di Dimissione Ospeda!iera del neonato per il ricovero relativo alla nascita (2 cifre identificative dell'anno + 6 cifre del progressivo).
Obbligatorio se il parto è awenuto in un istituto di cura Vitalità =  
.",
  
  "dt_nas_m", "Data di nascita della madre.",
  
  "COM_RES", "Comune di residenza della madre (codice ISTAT).",
  
  "VITALITA", "Condizione alla nascita: 1 = nato vivo, 2 = nato morto.3 = nato vivo e deceduto subito dopo la nascita",
  
  "metodi_PMA", "Nei casc il concepimento sia avvenuto tramite l' utilizzo di tecniche di procreazione medico-assistita, specificare metodo seguito:
1.	solo trattamento farmacologico per induzione dell'ovulazione;
2.	'U' Ontra Uterine insemination)
	S.	GIFT (Gamete Intra Fallopian Transfer)
4.	FIVET (Fertilization in Vitro and Ernb:yo Transfer)
5.	'CSI (Intra Cytoplasmatic Sperm Iniection)
6.	altre tecniche.

",
  "CONDIZIONE_PROF_PADRE", "Codice a 3 caratteri per individuare: ia condizione professionale (l cifra), la posizione nella professione (!l cifra) ed il ramo di attività economica (III cifra). La II e la Citra vanno compilate sob nei caso in cui la cifra sia (occupato).
I cifra:
occupato
2.	disoccupato
3.	in cerca di prima occupazione
4.	studente
5.	casalingo
6.	altra condizione (ritirato dai lavoro, inabile, ecn) Il cifra (se occupato):
1.	imprenditore o libero professionista
2.	altro lavoratore antonomo lavoratore dipendente: dirigente o direttivo
4.	lavoratore Gipendente: impiegato
51	lavoratore dipendente: openio
9.	altro lavoratore dipendente III cifra (se occupato):
1 .	agricoltura, caccia e pesca
2.	industria commercio: pubblici servizi, alberghi
4.	pubblica amministrazione
5.	altri servizi rivati
.",
  
  "COD_PRES", "Codice SP 11, base ad Anagrafe regionale delle Strutture sanitarie e sociosanitarie. Nel caso di parto a dom',cilio (programmato) o in casa di maternità5 in sostituzione del codice residio si ri orti il codice 080999.",
  "riscontro_autoptico", "Se nato morto, indicare se:
1.	'a causa di morte individuata è stata confermata dall'autopsia;
2.	il risultato delilautopsia sarà disponibile in seguito;
3.	'butopsia non è stata richiesta;
Obbli atorio se vitalità -2 nato morto
.",
  "TITOLO_DI_STUDIO_MADRE", "Vaiori ammessi: laurea
2.	diploma universitario
3.	diploma di scuola media superiore 4. diplome di scuola .nedia inferiore licer,z2 elementare o nessun titolo
")

# === 3. Join e aggiorna final_mapping ===
final_mapping <- final_mapping %>%
  left_join(mappa_descrizioni, by = "SDO_CEDAP_DD")





#calcolare
#1. 35 gia da trasferire
#2. dalla 37 alla 46 da ricodificare con operazioni di calcolo
#3.inserire lunghezza in eurocat
#4. le variabili devono essere tutte quelle di > eurocat_sdo_mapping$EUROCAT_Variable
#5. 






# Salva file finale ----
write_csv2(final_mapping, paste0(tableDir,"/final_mapping.csv"))











