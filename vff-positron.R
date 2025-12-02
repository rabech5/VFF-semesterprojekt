#------------------- vff data - superstats---------------------
# Load packages
pacman::p_load(tidyverse, rvest, janitor, lubridate, dplyr, DBI, lubridate, RSQLite)

# Link til superstats, uden sæson år (kommer i loop funktion)
url <- "https://superstats.dk/program?season="

# Laver en tom liste som tabellerne skal ligge i
seasons <- list()

# Loop hvor hver sæson kommer som en liste, med runder som tabeller
# alle listerne sættes i den tomme liste
for (i in 2002:2025) {
  html <- read_html(paste0(url, i), encoding = "UTF-8")
  
  tables <- html |>
    html_nodes("table") |>
    html_table(fill = TRUE)
  
  seasons[[as.character(i)]] <- tables
}

# Navngivning af hver sæson
names(seasons) <- 2002:2025

# Omdøb kolonner i alle tabeller
for(season_år in names(seasons)) {
  seasons[[season_år]] <- lapply(seasons[[season_år]],
    \(df) {
      df <- df |> 
        setNames(c("ugedag", "dato_tid", "kamp", "resultat", "tilskuere", 
                   "dommer", "tv"))
      
      # Konverter alle kolonner til character
      df <- df |>
        mutate(across(everything(), as.character))
      
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
    
    # Spring over hvis tabellen er tom
    if (nrow(table_data) == 0) {
      next
    }
    
    # Tilføj metadata
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

# ==================== Beregn akkumuleret point for ALLE kampe ====================
vff_alle_kampe <- kampe_raw |>
  filter(str_detect(kamp, "^VFF-") | str_detect(kamp, "-VFF$")) |>
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
    akk_point_alle_kampe = cumsum(vff_point),
    akk_point_lag_alle_kampe = lag(akk_point_alle_kampe),
    maal_sidste_3_kampe = lag(vff_maal, 1) + lag(vff_maal, 2) + lag(vff_maal, 3)
  ) |>
  ungroup() |>
  select(season_year, runde, kamp, akk_point_alle_kampe, akk_point_lag_alle_kampe, maal_sidste_3_kampe)

# ==================== Filtrer til KUN hjemmekampe ====================
vff_hjemme <- kampe_raw |>
  filter(str_detect(kamp, "^VFF-")) |>
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
  mutate(
    tilskuere = as.numeric(tilskuere) * 1000,
    tilskuere_sidste_møde = as.numeric(tilskuere_sidste_møde) * 1000
  ) |>
  separate(dato_tid, into = c("dato", "tid"), sep = " ", remove = FALSE) |>
  separate(tid, into = c("hour", "minute"), sep = ":", remove = FALSE) |>
  separate(dato, into = c("day", "month"), sep = "/", remove = FALSE) |>
  mutate(
    month = as.numeric(month),
    day = as.numeric(day),
    hour = as.numeric(hour),
    minute = as.numeric(minute),
    year = if_else(month >= 7, season_year-1, season_year),
    kampdag = make_datetime(year, month, day, hour, minute),
    kampdag_dato = as.Date(kampdag),
    kampdag = floor_date(kampdag, unit = "hour"),
    kampdag = format(kampdag, "%Y-%m-%dT%H:%M:%SZ")
  ) |>
  mutate(
    tid_kat = case_when(
      hour >= 6  & hour <= 11 ~ "formiddag",
      hour >= 12 & hour <= 17 ~ "eftermiddag",
      hour >= 18 & hour <= 23 ~ "aften",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    abc_kamp = case_when(
      udehold %in% c("FCK", "BIF", "FCM") ~ "A",
      udehold %in% c("AGF", "AaB", "OB", "SIF", "FCN", "RFC", "SJF") ~ "B",
      TRUE ~ "C"
    )
  ) |>
  select(
    season, kamp, abc_kamp, resultat, tilskuere, tilskuere_sidste_møde,
    akk_point_lag_alle_kampe, maal_sidste_3_kampe, runde, kampdag, kampdag_dato, tid_kat, ugedag
  )



# ==================== Vis resultater ====================
vff_hjemme |> 
  select(kampdag_dato, kamp, resultat, maal_sidste_3_kampe) |>
  head(20)

vff_hjemme |>
  count(season)

view(vff_hjemme)

# Gem rå data
write_csv(vff_hjemme, "vff_hjemme_raa_data_2002_2025.csv")
saveRDS(vff_hjemme, "vff_hjemme_raa_data_2002_2025.rds")

#--------------------DMI merge med VFF kampdag ----------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(lubridate)
library(purrr)

base_url <- "https://dmigw.govcloud.dk/v2/"
info_url <- "metObs/collections/observation/items?"
station_id <- "06060"
api_key <- Sys.getenv("MY_API_KEY")

kampdatoer <- vff_hjemme$kampdag #Trækker kampdage ud af vff_hjemme 
dmi_list <- list() # jeg laver en tom liste til DMI data 

#laver et loop gennem alle kampdagene 
for (kampdato in kampdatoer) {
  kampdato <- ymd_hms(kampdato, tz = "UTC")  #laver tidspunktsformat 
  kampdato <- floor_date(kampdato, unit = "hour") #runder ned til nærmeste time 
  
  start_iso <- format(kampdato , "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") 
  end_iso <- start_iso #slutter samme time som start 

  req_url <- paste0(
    "stationId=", station_id,
    "&datetime=", start_iso, "/", end_iso,
    "&limit=1000"
  )
  full_url <- paste0(base_url, info_url, req_url, "&api-key=", api_key)

  res <- httr::GET(full_url)
  if (res$status_code != 200) next

  api_json <- jsonlite::fromJSON(rawToChar(res$content))

  # Tjek om features eksisterer
  if (!is.null(api_json$features) && length(api_json$features) > 0) {
    
    # Sørg for, at det altid er en liste
    features_list <- api_json$features
    if (!is.list(features_list[[1]])) {
      features_list <- list(features_list)
    }
    # Træk observationer ud til tibble
    temp <- map_dfr(features_list, function(f) {
      tibble(
        Observationstidspunkt = f$properties$observed,# Træk observationer ud til tibble
        parameter = f$properties$parameterId,# Tidspunkt for observation
        value = f$properties$value# Parameter type
      )
    }) %>%
      pivot_wider(
        id_cols = Observationstidspunkt, # Pivotér så hver parameter bliver kolonne
        names_from = parameter,
        values_from = value
      )
    
    dmi_list <- append(dmi_list, list(temp)) #gemmer liste
  }
}


dmi_data <- bind_rows(dmi_list) # her kombineres alle tibbles 

#------------------- Konverter tid til POSIXct og dansk tid -------------------
  #henter tid fra JSON som tekst, og vil lave den om til POSIXct
dmi_data <- dmi_data %>%
mutate(
datotid_utc = ymd_hms(Observationstidspunkt, tz = "UTC"), # UTC tid
datotid_dk = with_tz(datotid_utc, tzone = "Europe/Copenhagen") # Dansk tid
)

dmi_data <- bind_rows(dmi_list) %>%
  select(
    kampdag = Observationstidspunkt,  # Omdøb så den matcher VFF
    regn = precip_past1h,
    temperatur = temp_dry,
    vindstyrke = wind_speed
  )

view(dmi_data)
str(dmi_data)



#------------------------Helligdage----------------------------------------------------------------

pacman::p_load("tidyverse", "lubridate", "rjson", "httr")

aar_super <- 2003:2025 # Kan gøres mere præcist - med kun de år, hvor VFF har været i superliga

# https://date.nager.at/api/v3/PublicHolidays/2025/DK

# Konstanterne i URL'en
BASE_URL <- 'https://date.nager.at/api/v3/PublicHolidays/'
LAND_URL <- '/DK'

# Datastruktur til helligdage
helligdage <- tibble(
  helligdag_navn = character(),
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
                          helligdag_navn = helligdag$localName,
                          dato = helligdag_dato
                          )
  }
}

view(helligdage)



#---------------------------- billetsalg 3, 7 og 10 dage før kamp -----------------------------------
vff_3_7_10 <- readRDS("vffkort01.rds")

con <- dbConnect(RSQLite::SQLite(), "fodbolddata.sqlite")

vff_billetsalg_3_7_10 <- vff_3_7_10 %>%
  select(sæson, runde, d10, d7, d3, d10_tilskuere, d7_tilskuere, d3_tilskuere)
dbWriteTable(con, "vffkort01_udvalgte", vff_billetsalg_3_7_10, row.names = FALSE, overwrite = TRUE)

view(vff_billetsalg_3_7_10)

dbDisconnect(con)

#------------------------------   Alt samlet -------------------------------------------------------

#Samler alt i en dataframe 
vff_komplet <- vff_hjemme |>
  # Join DMI vejrdata på kampdag (præcist tidspunkt)
  left_join(dmi_data, by = "kampdag") |>
  # Join helligdage på kampdag_dato (kun dato)
  left_join(helligdage, by = c("kampdag_dato" = "dato")) |>
  # Join vff_3_7_10 (antager den har en join-key - hvilken?)
  left_join(vff_billetsalg_3_7_10, by = c("season" = "sæson", "runde"))

view(vff_komplet)







