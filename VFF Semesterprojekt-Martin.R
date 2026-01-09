pacman::p_load(httr, lubridata, jsonlite, tidyverse, rlist, rjstat, rjson, rtools, rvest, Rcrawler, usethis, janitor, RSQlite, DBI, slider)
library(rvest)
library(slider)


# henter hjemmesiden
url <- "https://superstats.dk/program?season="
url
# tjek om html kan læses
html <- read_html(url)
html
# tom liste
seasons <- list()
# her ser vi alle runderne for 2003-2025 sæson.
for (i in 2003:2025) {
  html <- read_html(paste0("https://superstats.dk/program?season=", i), encoding = "UTF-8")

  tables <- html |> 
  html_element("#content") |> 
  html_elements("table") |> 
  html_table()

  seasons[[as.character(i)]] <- tables
}

tables
# slet tables
tables <- NULL
# navngiver sæsoner
names(seasons) <- 2003:2025
for(season_år in names(seasons)){
  seasons[[season_år]] <- lapply(seasons[[season_år]], clean_names)
}
# laver to ny variabler. en for sæson år og en for runde nummer
for(season_år in names(seasons)) {
  seasons[[season_år]] <- lapply(seasons[[season_år]], 
                                 \(df) mutate(df, season_year = season_år,
                                              round = parse_number(names(df[1]))))
}

# navngiver tabeller med ordentlige variabelnavne, sådan at de kan samles i
# en enkelt dataframe
for(season_år in names(seasons)) {
  seasons[[season_år]] <- lapply(seasons[[season_år]],
                                 \(df) df |> 
                                   setNames(c("ugedag", "dato_tid", "kamp", "resultat", "tilskuere", 
                                              "dommer", "tv", "sæson_år", "runde_nr"))
  )
}

# samler tabellerne i en enkelt dataframe
seasons_all <- seasons |> 
  flatten() |> 
  bind_rows()

# Ugedage og kamp er logical, tvinges til at være characters med as.character
# kør ovenstående igen
for(season_år in names(seasons)) {
  seasons[[season_år]] <- lapply(seasons[[season_år]],
                                 \(df) df |> mutate(ugedag = as.character(ugedag),
                                                    kamp = as.character(kamp))
  )
}
# view(seasons_all)

# uploader superstatsdata til SQLite database.
library(DBI)
library(RSQLite)
con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

# navngiver og tjekker om tabellen eksisterer, hvis den gør overwrites den.
dbWriteTable(con, "db_seasons_all", seasons_all, overwrite = TRUE)
# tjekker om der er uploadet noget til databsen. Vi får db_fcidk, db_seasons_all og db_vff
dbListTables(con)
# husk at disconnect
dbDisconnect(con)


# *******************************************************************
# fjerner duplicates, sætter overstående sammen for at få det til at fungerer
vff_hjemme <- seasons_all |> 
  # finder VFF hjemmekampe
  filter(str_detect(kamp, "VFF-")) |> 
  # fjerner kampe uden resultater
  filter(!is.na(resultat), resultat != "Optakt") |> 
  # fjerner duplicates
  distinct(kamp, dato_tid, .keep_all = TRUE) |> 
  separate_wider_delim(resultat, delim = "-", names = c("hjemme_mål", "ude_mål"), cols_remove = FALSE) |>
  mutate(
    hjemme_mål = as.numeric(hjemme_mål),
    ude_mål = as.numeric(ude_mål),
    point = case_when(
      hjemme_mål > ude_mål ~ 3,
      hjemme_mål == ude_mål ~ 1,
      hjemme_mål < ude_mål ~ 0
    ),
    # ganger tilskuertal op så de står i 1000
    tilskuere = parse_number(as.character(tilskuere)) * 1000,
    # samlet datoer
    samlet_dato = paste(dato_tid, sæson_år), 
    datotid = dmy_hm(str_replace(dato_tid, " ", paste0(" ", sæson_år, " ")), 
                     tz = "Europe/Copenhagen")
  ) |> 
  arrange(datotid) |> 
  # her vælger vi hvad der skal ses i view()
  select(datotid, sæson_år, runde_nr, tilskuere, kamp, resultat, hjemme_mål, ude_mål, point, dommer)
# view(vff_hjemme)

# **************************************************************************
vff_kampe <- seasons_all |> 
  filter(str_detect(kamp, "VFF-") | str_detect(kamp, "-VFF")) |> 
  separate_wider_delim(kamp, delim ="-", names = c("hjemmehold", "udehold")) |> 
  separate_wider_delim(resultat, "-", names = c("hjemme_mål", "ude_mål")) |>
  mutate(
    hjemme_mål = as.numeric(hjemme_mål),
    ude_mål = as.numeric(ude_mål),
   vff_point = case_when(
      hjemmehold == "VFF" & hjemme_mål > ude_mål ~ 3, 
      hjemmehold == "VFF" & hjemme_mål == ude_mål ~ 1, 
      hjemmehold == "VFF" & hjemme_mål < ude_mål ~ 0, 
      udehold == "VFF" & ude_mål > hjemme_mål ~ 3, 
      udehold == "VFF" & ude_mål == hjemme_mål ~ 1,
      udehold == "VFF" & ude_mål < hjemme_mål ~ 0,
    )
  ) |> 
  group_by(sæson_år) |> 
  arrange(sæson_år, runde_nr) |> 
  mutate(
    point_alle_kampe = cumsum(vff_point),
    point_lag_alle_kampe = lag(point_alle_kampe)
  ) |> 
  ungroup() |> 
  select(sæson_år, runde_nr, point_alle_kampe, point_lag_alle_kampe)

# view(vff_kampe)


# *******************************************************************
# placeringer, ligner tidligere kode men point skal bruges igen. Ved ikke hvordan jeg kan trække fra tidligere??
# spørg bjarne evt
placering <- seasons_all |> 
  filter(!is.na(resultat), resultat != "Optakt") |> 
  separate_wider_delim(kamp, delim = "-", names = c("hjemmehold", "udehold")) |> 
  separate_wider_delim(resultat, "-", names = c("hjemme_mål", "ude_mål")) |>
  mutate(
    hjemme_mål = as.numeric(hjemme_mål),
    ude_mål = as.numeric(ude_mål)
  ) |> 
  # laver data til langt format - en række per hold per kamp
  pivot_longer(
    cols = c(hjemmehold, udehold),
    names_to = "type",
    values_to = "hold"
  ) |> 
  # beregn point for hvert hold
  mutate(
    point = case_when(
      type == "hjemmehold" & hjemme_mål > ude_mål ~ 3, 
      type == "hjemmehold" & hjemme_mål == ude_mål ~ 1, 
      type == "hjemmehold" & hjemme_mål < ude_mål ~ 0, # hvorfor kan den ikke køre uden type == ???????
      type == "udehold" & ude_mål > hjemme_mål ~ 3,   
      type == "udehold" & ude_mål == hjemme_mål ~ 1,  
      type == "udehold" & ude_mål < hjemme_mål ~ 0   
    )
  ) |> 
  # gruppér efter sæson og hold
  group_by(sæson_år, hold) |> 
  arrange(sæson_år, runde_nr) |> 
  #ligger point sammen gennem sæsonen
  mutate(
    total_point = cumsum(point)
  ) |> 
  ungroup() |> 
  # beregner point for hver runde
  group_by(sæson_år, runde_nr) |> 
  mutate(
    placering = rank(-total_point, ties.method = "min")
  ) |> 
  ungroup() |> 
  select(sæson_år, runde_nr, hold, total_point, placering)

# view(placering)

# nu skal vi finde tilskuere ved sidste kamp med samme modstander
# *******************************************************************
vff_hjemme <- vff_hjemme |> 
  arrange(sæson_år, runde_nr) |> 
  group_by(kamp) |> 
  mutate(
    tilskuere_sidste_vs_modstander = lag(tilskuere, n = 1)
  ) |> 
  ungroup() |> 
  # split kamp for at få modstanderens navn
  separate_wider_delim(kamp, delim = "-", names = c("temp_vff", "modstander"), cols_remove = FALSE) |> 
  # left join VFF placering
  left_join(
    placering |> 
      filter(hold == "VFF") |> 
      select(sæson_år, runde_nr, vff_placering = placering),
    by = c("sæson_år", "runde_nr")
  ) |> 
  # left join modstander placering
  left_join(
    placering |> 
      select(sæson_år, runde_nr, hold, modstander_placering = placering),
    by = c("sæson_år", "runde_nr", "modstander" = "hold")
  ) |> 
  # Fjern temp kolonne
  select(-temp_vff)
# view(vff_hjemme)
# *******************************************************************

# nu finder vi mål i seneste 3 hjemmekampe
library(slider)

vff_hjemme <- vff_hjemme |> 
  mutate(
    mål_sidste_3_hjemme = slide_dbl(
      hjemme_mål,
      .f = ~sum(.x, na.rm = TRUE),
      .before = 3, # kig på 3 rækker før
      .after = -1, # bruger -1 for at ekskludere nuværende kamp, vi vil have de 3 før
      .complete = FALSE # hvis kun 2 kampe før, summer den alligevel TAK CHAT
    )
  )
# view(vff_hjemme)

# *******************************************************************
# nu skal vi finde point for de sidste 3 kampe (hjemme + ude)
vff_hjemme <- vff_hjemme |> 
  left_join(
    vff_kampe |> select(sæson_år, runde_nr, point_lag_alle_kampe),
    by = c("sæson_år", "runde_nr")
  )
# view(vff_hjemme) 

# PRØVEDE DETTE MEN DET DUER IKKE ????
# *******************************************************************
# prøver en længere kode
vff_alle_kampe <- seasons_all |> 
  filter(str_detect(kamp, "VFF-") | str_detect(kamp, "-VFF")) |> 
  filter(!is.na(resultat), resultat != "Optakt") |> 
  separate_wider_delim(kamp, delim ="-", names = c("hjemmehold", "udehold")) |> 
  separate_wider_delim(resultat, delim ="-", names = c("hjemme_mål", "ude_mål")) |> 
   mutate(
    hjemme_mål = as.numeric(hjemme_mål),
    ude_mål = as.numeric(ude_mål),
   vff_point = case_when(
      hjemmehold == "VFF" & hjemme_mål > ude_mål ~ 3, 
      hjemmehold == "VFF" & hjemme_mål == ude_mål ~ 1, 
      hjemmehold == "VFF" & hjemme_mål < ude_mål ~ 0, 
      udehold == "VFF" & ude_mål > hjemme_mål ~ 3, 
      udehold == "VFF" & ude_mål == hjemme_mål ~ 1,
      udehold == "VFF" & ude_mål < hjemme_mål ~ 0,
    )
  ) |> 
  arrange(sæson_år, runde_nr) |> 
  group_by(sæson_år) |> 
  mutate(
    point_sidste_3 = slide_dbl(
      vff_point,
      .f = ~sum(.x, na.rm = TRUE),
      .before = 3, 
      .after = -1, 
      .complete = FALSE 
    )
  ) |> 
  ungroup() |> 
  select(sæson_år, runde_nr, point_sidste_3)

vff_hjemme <- vff_hjemme |> 
  left_join(
    vff_alle_kampe,
    by = c("sæson_år", "runde_nr")
  )
view(vff_hjemme)

# tid til at joine alle datasæt til SQLite database
con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

# uploader alle datasæt fra tidligere kode
dbWriteTable(con, "db_seasons_all", vff_hjemme, overwrite = TRUE)

# tjekker om alt er det, men har lavet den fejl jeg først uploadede flere datasæt, frem for at merge det ind i 
# et enkelt data sæt, så vi har nu left_join i koden og så kun opdaterer vff_hjemme
dbListTables(con)

dbDisconnect(con)

# ****************************************************************
# DMI, Bjarnes kode med få rettelse af mit ikke fungerede. Har gemt gammel DMI længere nede.
install.packages("lubridata")
pacman::p_load(httr, tidyverse, rjson, lubridata)
vff_hjemmekampe <- read_rds("data/datoer_hjemme")

hent_dmi_parametre <- function(dato_tid){
  # 1. Formatér dato korrekt med 'Z' (UTC)
  dato_tid_formatted <- format(dato_tid, '%Y-%m-%dT%H:%M:%SZ', tz = 'CET')
  # 2. Byg parametre uden parameterId (hent alle, filtrér bagefter)
  params <- list(
    stationId = '06060',
    datetime =  dato_tid_formatted, #'2024-02-16T17:00:00Z', 
    `api-key` = Sys.getenv("MY_API_KEY")
  )
  
  response <- GET('https://dmigw.govcloud.dk/v2/metObs/collections/observation/items', query = params)
  raadata <- content(response, as = "text", encoding = "UTF-8")
  parsed_data <- fromJSON(raadata)
  # xx <- parsed_data$features[[1]]$properties
  
  properties_list <- lapply(parsed_data$features, function(xxx) xxx$properties)
  properties_df <- bind_rows(properties_list)
  
  target_params <- c("precip_past1h", "precip_dur_past1h", "wind_speed_past1h", "cloud_cover", "temp_dry")
  udvalgt_data <- properties_df |> 
    select(observed, parameterId, value) |> 
    filter(parameterId %in% target_params) |> 
    arrange(desc(observed)) 
  
  parametre_data <- udvalgt_data |> 
    pivot_wider(names_from = parameterId, values_from = value)
  
  return(parametre_data)
}

# hent DMI data for alle hjemmekampe
resultater <- list()
for (i in seq_along(vff_hjemmekampe$dato_tid)) {
  dato_tid_raw <- vff_hjemmekampe$dato_tid[i]
  dato_tid <- floor_date(as.POSIXct(dato_tid_raw), "hour") - hours(2)
  
  cat("Processing", i, "of", nrow(vff_hjemmekampe), "- Dato:", as.character(dato_tid), "\n")
  # udvælger variabler vi vil se
  parametre <- hent_dmi_parametre(dato_tid = dato_tid)
      resultater[[length(resultater) + 1]] <- tibble(
      original_dato_tid = dato_tid_raw,
      dato_tid = dato_tid,
      temp_dry = parametre$temp_dry,
      precip_past1h = parametre$precip_past1h,
      precip_dur_past1h = parametre$precip_dur_past1h,
      wind_speed_past1h = parametre$wind_speed_past1h
    )
}
resultater <- bind_rows(resultater)
# view(resultater)


# *******************************************************************
# helligedage 
pacman::p_load("tidyverse", "lubridate", "rjson", "httr")

aar_super <- c(2003, 2004, 2005, 2006, 2007, 2013, 2015, 2016, 2021, 2022, 2023, 2024, 2025) # præcise år

# https://date.nager.at/api/v3/PublicHolidays/2025/DK

# konstanterne i URL'en
BASE_URL <- 'https://date.nager.at/api/v3/PublicHolidays/'
LAND_URL <- '/DK'

# datastruktur til helligdage
helligdage <- tibble(
  navn = character(),
  dato = as.Date(character())
)

for(aar in aar_super) {
  url <- paste0(BASE_URL, aar, LAND_URL)
  response <- GET(url)
  if (status_code(response) != 200) {
    stop("Fejl ved hentning af data: ", status_code(response))
  }
  raadata <- content(response, as = "text", encoding = "UTF-8")
  
  parsed_data <- fromJSON(raadata)
  for (helligdag in parsed_data) {
    helligdag_dato <- as.Date(helligdag$date)
    helligdage <- add_row(
                          helligdage,
                          navn = helligdag$localName,
                          dato = helligdag_dato
                          )
  }
}

# view(helligdage)

vff_hjemme <- vff_hjemme |> 
  mutate(
    kamp_dato = as_date(datotid)
  ) |> 
  left_join(
    helligdage,
    by = c("kamp_dato" = "dato")
  ) |> 
  mutate(
    er_helligdag = !is.na(navn),
    helligdag_navn = navn
  ) |> 
  select(-navn)

# view(vff_hjemme)

# problemer med NA i alle mine DMI i vff_hjemme_final, selvom den burde vise fra 2013 og frem. 
# prøver at sørge for samme tidszone, same tjekker med str() hvad de første 5 er.
resultater <- resultater |> 
  mutate(original_dato_tid = as.POSIXct(original_dato_tid, tz = "Europe/Copenhagen"))

vff_hjemme <- vff_hjemme |> 
  mutate(datotid = as.POSIXct(datotid, tz = "Europe/Copenhagen"))

str(vff_hjemme$datotid[1:5])
str(resultater$original_dato_tid[1:5])


# NU har vi også helligdage i vores dataframe vff_hjemme hvor vi kan se alle de dage der er spillet fodbold
# på en helligdag.
# joiner hjemmekampe, DMI data og helligdage sammen i et datasæt
vff_hjemme_final <- vff_hjemme |> 
  left_join(
    resultater |> 
      select(original_dato_tid, temp_dry, precip_past1h, 
        precip_dur_past1h, wind_speed_past1h),
        by = c("datotid" ="original_dato_tid")
      ) |> 
  mutate(kamp_dato = as_date(datotid)) |> 
  left_join(helligdage,
  by = c("kamp_dato" = "dato")
  ) |> 
  mutate(er_helligdag = !is.na(navn),
helligdag_navn = navn
  ) |> 
  select(-navn)

view(vff_hjemme_final)

con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")
# får NA i alle mine DMI data?
# tidszoner passer ikke? UTC vs CET? men har europe/copenhagen og CET
# burde være det samme

# *******************************************************************
# NOTER OG HJÆLP
# *******************************************************************
# til check om det er spillet i 2002 eller 2003 fx
 mutate(
    real_år = if_else(str_detect(dato_tid, "/(07|08|09|10|11|12) "), as.numeric(sæson_år) - 1, as.numeric(sæson_år))
  )
# *******************************************************************

# henter superstats data, laver nye variable, ligge op i databasen, 
# så hente igen som RDS fil for at bruger datoer til DMI
# gør det samme med DMI og helligdage og så samle til sidst med 
# 3,7,10 dage før vffkort01 data fra bjarne.

# *******************************************************************
fcidk <- readRDS("data/fcidk.rds")
view(fcidk)

vffkort01 <- readRDS("data/vffkort01.rds")
view(vffkort01)
# *******************************************************************
# her samler vi alt det tidligere



# *******************************************************************
# SQL
library(DBI)
library(odbc)
install.packages("usethis")
library(usethis)
edit_r_environ()

readRenviron("~/.Renviron")

con <- dbConnect(odbc(), Driver = "ODBC Driver 18 for SQL Server", 
                 Server = "mmlsql.database.windows.net", Database = "Dania", 
                 Port = "1433", UID = Sys.getenv("usr"), PWD = Sys.getenv("pwd"))
dbDisconnect(con)

# SQL query
vff_all <- dbGetQuery(con,
  "SELECT s.*, r.temp_dry, r.wind_speed, r.precip_past1h, g.helligdage, 
   t.d10_tilskuere, t.d7_tilskuere, t.d3_tilskuere
   FROM db_seasons_all AS s
   LEFT JOIN db_resultater AS r
   ON s.dmi_dato = r.observed
   LEFT JOIN db_helligdage AS g
   ON s.dato = g.dato
   LEFT JOIN db_vff AS t
   ON s.real_år = t.år
   AND s.runde_nr = t.runde
   WHERE s.kamp LIKE '%VFF-%'"
)
dbDisconnect(con)
