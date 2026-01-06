#------------------- vff data - superstats---------------------
# Load packages
pacman::p_load(tidyverse, rvest, janitor, lubridate, dplyr, DBI, lubridate, RSQLite)

# Link til superstats, uden sæson år (kommer i loop funktion)
url <- "https://superstats.dk/program?season="

# Laver en tom liste som tabellerne skal ligge i
seasons <- list()

# Loop hvor hver sæson kommer som en liste, med runder som tabeller
# alle listerne sættes i den tomme liste
for (i in 2002:2026) {
  html <- read_html(paste0(url, i), encoding = "UTF-8")
  
  tables <- html |>
    html_nodes("table") |>
    html_table(fill = TRUE)
  
  seasons[[as.character(i)]] <- tables
}

# Navngivning af hver sæson
names(seasons) <- 2002:2026

# Omdøb kolonner i alle tabeller og tving datatyper
for(season_år in names(seasons)) {
  seasons[[season_år]] <- lapply(seasons[[season_år]],
    \(df) {
      # Spring over tomme tabeller FØRST
      if (nrow(df) == 0 || ncol(df) < 7) {
        return(df)
      }
      
      df <- df |> 
        setNames(c("ugedag", "dato_tid", "kamp", "resultat", "tilskuere", 
                   "dommer", "tv"))
      
      # Tving alle kolonner til character for at undgå datatype-konflikter
      df <- df |>
        mutate(
          ugedag = as.character(ugedag),
          dato_tid = as.character(dato_tid),
          kamp = as.character(kamp),
          resultat = as.character(resultat),
          tilskuere = as.character(tilskuere),
          dommer = as.character(dommer),
          tv = as.character(tv)
        )
      
      return(df)
    }
  )
}

# Kombinér alle tabeller fra alle sæsoner
all_data <- list()

for (season_year in names(seasons)) {
  tables_in_season <- seasons[[season_year]]
  
  for (table_idx in seq_along(tables_in_season)) {
    table_data <- tables_in_season[[table_idx]]
    
    # Spring over hvis tabellen er tom ELLER mangler kolonner
    if (nrow(table_data) == 0 || ncol(table_data) < 7) {
      next
    }
    
    # Tilføj metadata: sæson og runde-information
    table_data <- table_data |>
      mutate(
        season_year = as.numeric(season_year),
        season = paste0(season_year - 1, "/", as.numeric(season_year)),
        runde = table_idx
      )
    
    all_data[[length(all_data) + 1]] <- table_data
  }
}

# Kombinér til én tibble
kampe_raw <- bind_rows(all_data)

# Ryd op efter loop (Punkt 5)
all_data <- NULL
seasons <- NULL

# GEM RÅDATA I DATABASE FØRST (Punkt 1)
con <- dbConnect(RSQLite::SQLite(), "fodbolddata.sqlite")
dbWriteTable(con, "db_kampe_raw", kampe_raw, row.names = FALSE, overwrite = TRUE)
dbDisconnect(con)

# -------------- Beregn akkumuleret point for ALLE kampe ------------------------------
vff_alle_kampe <- kampe_raw |>
  filter(str_detect(kamp, "^VFF-") | str_detect(kamp, "-VFF$")) |>
  # Fjern duplikater FØRST (samme kamp i samme runde/sæson)
  distinct(season_year, runde, kamp, .keep_all = TRUE) |>
  separate(kamp, into = c("hjemmehold", "udehold"), sep = "-", remove = FALSE) |>
  separate(resultat, into = c("hjemme_mål", "ude_mål"), sep = "-", remove = FALSE) |>
  mutate(
    hjemme_mål = as.numeric(hjemme_mål),
    ude_mål = as.numeric(ude_mål),
    vff_maal = if_else(hjemmehold == "VFF", hjemme_mål, ude_mål),
    vff_point = case_when(
      hjemmehold == "VFF" & hjemme_mål > ude_mål ~ 3,
      hjemmehold == "VFF" & hjemme_mål == ude_mål ~ 1,
      hjemmehold == "VFF" & hjemme_mål < ude_mål ~ 0,
      udehold == "VFF" & ude_mål > hjemme_mål ~ 3,
      udehold == "VFF" & ude_mål == hjemme_mål ~ 1,
      udehold == "VFF" & ude_mål < hjemme_mål ~ 0,
      TRUE ~ 0
    )
  ) |>
  group_by(season_year) |>
  arrange(season_year, runde) |>
  mutate(
    # Akkumulerede point gennem sæsonen
    akk_point_alle_kampe = cumsum(vff_point),
    # Point før nuværende kamp (lagged værdi)
    akk_point_lag_alle_kampe = lag(akk_point_alle_kampe),
    # Mål scoret i de seneste 3 kampe
    maal_sidste_3_kampe = lag(vff_maal, 1) + lag(vff_maal, 2) + lag(vff_maal, 3)
  ) |>
  ungroup() |>
  select(season_year, runde, kamp, akk_point_alle_kampe, akk_point_lag_alle_kampe, maal_sidste_3_kampe)

# ----------------------------------- Filtrer til KUN hjemmekampe -----------------------------------
vff_hjemme <- kampe_raw |>
  filter(str_detect(kamp, "^VFF-")) |>
  # Fjern duplikater FØRST
  distinct(season_year, runde, kamp, .keep_all = TRUE) |>
  separate(kamp, into = c("hjemmehold", "udehold"), sep = "-", remove = FALSE) |>
  left_join(vff_alle_kampe, by = c("season_year", "runde", "kamp")) |>
  group_by(udehold) |>
  arrange(season_year, runde) |>
  mutate(
    tilskuere_sidste_møde = lag(tilskuere)
  ) |>
  ungroup() |>
  separate(resultat, into = c("vff_mål", "modstander_mål"), sep = "-", remove = FALSE) |>
  mutate(
    vff_mål = as.numeric(vff_mål),
    modstander_mål = as.numeric(modstander_mål)
  ) |>
  # Konverter tilskuertal fra tusinder til faktisk antal
  mutate(
    tilskuere = as.numeric(tilskuere) * 1000,
    tilskuere_sidste_møde = as.numeric(tilskuere_sidste_møde) * 1000
  ) |>
  # Opbyg komplet datetime objekt fra dato og tid
  separate(dato_tid, into = c("dato", "tid"), sep = " ", remove = FALSE) |>
  separate(tid, into = c("hour", "minute"), sep = ":", remove = FALSE) |>
  separate(dato, into = c("day", "month"), sep = "/", remove = FALSE) |>
  mutate(
    month = as.numeric(month),
    day = as.numeric(day),
    hour = as.numeric(hour),
    minute = as.numeric(minute),
    # År bestemt ud fra om kampen er før eller efter sommerpause
    year = if_else(month >= 7, season_year-1, season_year),
    kampdag = make_datetime(year, month, day, hour, minute),
    kampdag_dato = as.Date(kampdag),
    # Afrund til nærmeste time for match med DMI data
    kampdag = floor_date(kampdag, unit = "hour"),
    kampdag = format(kampdag, "%Y-%m-%dT%H:%M:%SZ")
  ) |>
  # Kategoriser kampens tidspunkt på dagen
  mutate(
    tid_kat = case_when(
      hour >= 6  & hour <= 11 ~ "formiddag",
      hour >= 12 & hour <= 17 ~ "eftermiddag",
      hour >= 18 & hour <= 23 ~ "aften",
      TRUE ~ NA_character_
    )
  ) |>
  select(
    season, kamp, resultat, tilskuere, tilskuere_sidste_møde,
    akk_point_lag_alle_kampe, maal_sidste_3_kampe, runde, kampdag, kampdag_dato, tid_kat, ugedag
  )

# Rydder op
kampe_raw <- NULL

#--------------------DMI merge med VFF kampdag ----------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(lubridate)
library(purrr)
# DMI API setup
base_url <- "https://dmigw.govcloud.dk/v2/"
info_url <- "metObs/collections/observation/items?"
station_id <- "06060"
api_key <- Sys.getenv("MY_API_KEY")

kampdatoer <- vff_hjemme$kampdag
dmi_list <- list()
# Henter vejrdata for hver kampdag
for (kampdato in kampdatoer) {
  kampdato <- ymd_hms(kampdato, tz = "UTC")
  kampdato <- floor_date(kampdato, unit = "hour")
  
  start_iso <- format(kampdato , "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") 
  end_iso <- start_iso
  # Bygger API request URL
  req_url <- paste0(
    "stationId=", station_id,
    "&datetime=", start_iso, "/", end_iso,
    "&limit=1000"
  )
  full_url <- paste0(base_url, info_url, req_url, "&api-key=", api_key)

  res <- httr::GET(full_url)
  if (res$status_code != 200) next

  api_json <- jsonlite::fromJSON(rawToChar(res$content))

  if (!is.null(api_json$features) && length(api_json$features) > 0) {
    
    features_list <- api_json$features
    if (!is.list(features_list[[1]])) {
      features_list <- list(features_list)
    }
    # Udtrækker vejrparametre og pivoter til wide format
    temp <- map_dfr(features_list, function(f) {
      tibble(
        Observationstidspunkt = f$properties$observed,
        parameter = f$properties$parameterId,
        value = f$properties$value
      )
    }) %>%
      pivot_wider(
        id_cols = Observationstidspunkt,
        names_from = parameter,
        values_from = value
      )
    
    dmi_list <- append(dmi_list, list(temp))
  }
}
# Kombiner alle vejrobservationer
dmi_data <- bind_rows(dmi_list) %>%
  select(
    kampdag = Observationstidspunkt,
    regn = precip_past1h,
    temperatur = temp_dry,
    vindstyrke = wind_speed
  )

# Ryder op 
dmi_list <- NULL

# Gemmer DMI rådata i database
con <- dbConnect(RSQLite::SQLite(), "fodbolddata.sqlite")
dbWriteTable(con, "db_dmi_raw", dmi_data, row.names = FALSE, overwrite = TRUE)
dbDisconnect(con)

#------------------------Helligdage----------------------------------------------------------------

pacman::p_load("tidyverse", "lubridate", "rjson", "httr")

aar_super <- 2003:2026
# Nager.Date API til danske helligdage
BASE_URL <- 'https://date.nager.at/api/v3/PublicHolidays/'
LAND_URL <- '/DK'

helligdage <- tibble(
  helligdag_navn = character(),
  dato = as.Date(character())
)
# Henter helligdage for alle relevante år
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
                          helligdag_navn = helligdag$localName,
                          dato = helligdag_dato
                          )
  }
}

# Gemmer helligdage i database
con <- dbConnect(RSQLite::SQLite(), "fodbolddata.sqlite")
dbWriteTable(con, "db_helligdage_raw", helligdage, row.names = FALSE, overwrite = TRUE)
dbDisconnect(con)

#---------------------------- billetsalg 3, 7 og 10 dage før kamp -----------------------------------
# Indlæser eksisterende billetsalgsdata
vff_3_7_10 <- readRDS("vffkort01.rds")
view(vff_3_7_10)
vff_billetsalg_3_7_10 <- vff_3_7_10 %>%
  select(sæson, runde, d10_tilskuere, d7_tilskuere, d3_tilskuere)

#------------------------------   UPLOAD TIL DATABASE -------------------------------------------------------

# Opretter forbindelse til database
con <- dbConnect(RSQLite::SQLite(), "fodbolddata.sqlite")

# Uploader alle datasæt til databasen
dbWriteTable(con, "vff_hjemme", vff_hjemme, row.names = FALSE, overwrite = TRUE)
dbWriteTable(con, "dmi_data", dmi_data, row.names = FALSE, overwrite = TRUE)
dbWriteTable(con, "helligdage", helligdage, row.names = FALSE, overwrite = TRUE)
dbWriteTable(con, "vff_billetsalg", vff_billetsalg_3_7_10, row.names = FALSE, overwrite = TRUE)

# Join alle tabeller i databasen til ét komplet datasæt
vff_komplet <- dbGetQuery(con,
  "SELECT v.*, 
          d.regn, 
          d.temperatur, 
          d.vindstyrke, 
          h.helligdag_navn,
          b.d10_tilskuere,
          b.d7_tilskuere,
          b.d3_tilskuere
   FROM vff_hjemme AS v
   LEFT JOIN dmi_data AS d
   ON v.kampdag = d.kampdag
   LEFT JOIN helligdage AS h
   ON v.kampdag_dato = h.dato
   LEFT JOIN vff_billetsalg AS b
   ON v.season = b.sæson
   AND v.runde = b.runde"
)

# Disconnecter 
dbDisconnect(con)

# Konverter tilbage til tibble
vff_komplet <- as_tibble(vff_komplet)

# Konverter kampdag_dato tilbage til Date format
vff_komplet <- vff_komplet |>
  mutate(kampdag_dato = as.Date(kampdag_dato, origin = "1970-01-01"))

# Håndter manglende værdier
vff_komplet <- vff_komplet |>
  mutate(
    # helligdage omdannes til dummy variable 
    helligdag_navn = if_else (!is.na(helligdag_navn), 1, 0)
  ) |>
  # Fjerner rækker hvor vejrdata mangler 
  filter(!is.na(temperatur))

# Konverter kategoriske variable til faktorer
vff_komplet <- vff_komplet |>
  mutate(
    tid_kat = as.factor(tid_kat),
    ugedag = as.factor(ugedag),
    season = as.factor(season)
  )

# ----------------------------------- Viser resultater ---------------------------------------------------------
vff_komplet |> 
  select(kampdag_dato, kamp, resultat, temperatur, helligdag_navn) |>
  head(20)

vff_komplet |>
  count(season)

view(vff_komplet)

# Gemmer færdigt datasæt
write_csv(vff_komplet, "vff_komplet_sql.csv")
saveRDS(vff_komplet, "vff_komplet_sql.rds")

