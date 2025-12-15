# load packages

pacman::p_load(tidyverse, rvest, janitor, RSQLite, slider)

# superstats data --------------------------------------------------------

# link til superstats, uden sæson år (kommer i loop funktion)

url <- "https://superstats.dk/program?season="

# laver en tom liste som tabellerne skal ligge i
seasons <- list()

# loop hvor hver sæson kommer som en liste, med runder som tabeller
# alle listerne sættes i den tomme liste

for (i in 2002:2026) {
  html <- read_html(paste0(url, i), encoding = "UTF-8")
  
  tables <- html |>
    html_element("#content") |>
    html_elements("table") |>
    html_table()

  seasons[[as.character(i)]] <- tables
}

# sletter tables

tables <- NULL

# renser navne på tabellerne med janitor for at kunne arbejde videre med dem

for(season_år in names(seasons)) {
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
  purrr::flatten() |> 
  bind_rows()

# får fejl da det ikke er alle observationer i ugedag og kamp der er samme type
# alle observationer i ugedag og kamp tvinges derfor til at være character
# kør ovenstående igen

for(season_år in names(seasons)) {
  seasons[[season_år]] <- lapply(seasons[[season_år]],
    \(df) df |> mutate(ugedag = as.character(ugedag),
                       kamp = as.character(kamp))
      )
}

# samler tabellerne i en enkelt dataframe

seasons_all <- seasons |> 
  flatten() |> 
  bind_rows()

# tilskuere blive i ovenstående funktion lavet til en double, men står ikke længere i 1000'er
# tilskuere variabel laves derfor i 1000'er i stedet for

seasons_all <- seasons_all |> 
  mutate(tilskuere = tilskuere * 1000)

# uploader alle superstatsdata til SQLite database, i tilfælde af at flere skal bruges

con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

dbWriteTable(con, "db_seasons_all", seasons_all, overwrite = TRUE)

# tjekker om den er blevet uploadet

dbListTables(con)

dbDisconnect(con)

# først laves en række nye variabler ud fra de fulde datasæt for superligaen, sådan at
# variabler som f.eks antal mål i sidste kamp, blive den reele seneste kamp, og ikke seneste hjemmekamp,
# og så der kan laves totale aggregerede point for hver sæson

# henter data alle superliga kampe, ikke kun vff hjemmekampe, fra database

con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

seasons_all <- dbReadTable(con, "db_seasons_all")

dbDisconnect(con)

# finder alle VFF's kampe, både hjemme og ude

seasons_all <- seasons_all |> 
  filter(str_detect(kamp, "VFF-") | str_detect(kamp, "-VFF"))

# opdeler resultat variablen i to, sådan at hjemmeholdets mål og udeholdets mål
# står for sig selv

seasons_all <- seasons_all |>
  separate_wider_delim(
    resultat,
    delim = "-",
    names = c("hjemme_mål", "ude_mål"),
    too_few = "debug"
  )
  
seasons_all <- seasons_all |>
  mutate(
    vff_mål = as.numeric(if_else(str_detect(kamp, "VFF-"), hjemme_mål, ude_mål)),
    .after = kamp
  ) |> 
  mutate(
    mål_sidste_kamp = as.numeric(lag(vff_mål)),
    .after = vff_mål
  )

# ny variabel for mål i de sidste tre kampe, ved at bruge slider package, og lagged version

seasons_all <- seasons_all |> 
  mutate(
    mål_sidste_tre = slide_dbl(vff_mål, sum, .before = 2, .complete = TRUE)
  )

seasons_all <- seasons_all |> 
  mutate(
    mål_sidste3_lagged = lag(mål_sidste_tre)
  )

# laver resultat og lagged resultat variabel

seasons_all <- seasons_all |> 
  mutate(
    vff_resultat = if_else(str_detect(kamp, "-VFF") & hjemme_mål > ude_mål, "tabt", NA),
    .after = kamp,
    vff_resultat = if_else(str_detect(kamp, "-VFF") & hjemme_mål < ude_mål, "vundet", vff_resultat), 
    vff_resultat = if_else(str_detect(kamp, "-VFF") & hjemme_mål == ude_mål, "uafgjort", vff_resultat),
    vff_resultat = if_else(str_detect(kamp, "VFF-") & hjemme_mål > ude_mål, "vundet", vff_resultat),
    vff_resultat = if_else(str_detect(kamp, "VFF-") & hjemme_mål < ude_mål, "tabt", vff_resultat),
    vff_resultat = if_else(str_detect(kamp, "VFF-") & hjemme_mål == ude_mål, "uafgjort", vff_resultat),
    vff_resultat_lagged = lag(vff_resultat)
  )

# laver ny variabel for antal point, der skal bruges til at lave variabel for
# point i de sidste tre kampe, og aggregered point for sæsonen

seasons_all <- seasons_all |> 
  mutate(
    point_kamp = if_else(str_detect(vff_resultat, "tabt"), 0, NA ),
    .after = vff_resultat_lagged,
    point_kamp = if_else(str_detect(vff_resultat, "uafgjort"), 1, point_kamp),
    point_kamp = if_else(str_detect(vff_resultat, "vundet"), 3, point_kamp)
  )

# ny variabel for antal point i de sidste tre kampe, ved at bruge slider package, og lagged version

seasons_all <- seasons_all |> 
  mutate(
    point_sidste3 = slide_dbl(point_kamp, sum, .before = 2, .complete = TRUE),
    .after = point_kamp
  )

seasons_all <- seasons_all |> 
  mutate(
    point_sidste3_lagged = lag(point_sidste3)
  )

# ny variabel for aggregerede point for hver sæson, og lagged version
# igen bruges slider package

seasons_all <- seasons_all |> 
  group_by(sæson_år) |> 
  mutate(
    point_sæson = slide_dbl(point_kamp, sum, .before = Inf, .complete = FALSE)
  ) |> 
  ungroup()

seasons_all <- seasons_all |> 
  mutate(
    point_sæson_lagged = lag(point_sæson)
  )

# datotids variabler
# inden den samlede datotid variabel laves, skal det reele år kampene blev spillet først laves,
# i stedet for sæsonåret. derefter laves tidsvariablen, som også konverteres til en ny variabel
# hvor tidszonen er UTC, som skal bruges til at få data fra DMI

seasons_all <- seasons_all |> 
 mutate(
    real_år = if_else(str_detect(dato_tid, "/(07|08|09|10|11|12) "), as.numeric(sæson_år) - 1, as.numeric(sæson_år))
  ) |> 
  mutate(datotid = ydm_hm(paste(real_år, dato_tid), tz = "Europe/Copenhagen")) |> 
  mutate(datotid_utc = with_tz(datotid, tzone = "UTC"))

# laver en ny variabel for hvilket tidspunkt på dagen det er, midag, efftermiddag, eller aften

seasons_all <- seasons_all |> 
  mutate(
    tidspunkt = if_else(hour(datotid) %in% 10:14, "middag", NA),
    tidspunkt = if_else(hour(datotid) %in% 15:18, "eftermiddag", tidspunkt),
    tidspunkt = if_else(hour(datotid) %in% 18:23, "aften", tidspunkt)
  )

# nye variabler for sidste møde mellem holdene. ved at bruge group_by på kamp variablen,
# bliver det pr sidste hjemmemøde og sidste udemøde. altså bliver sidste møde tilskuere altså en
# variabel for hjemmekampene hvor det er tilskuere i sidste hjemmekamp, og omvendt for udekampe

seasons_all <- seasons_all |> 
  group_by(kamp) |> 
  mutate(
    sidste_møde_tilskuere = lag(tilskuere)
  ) |> 
  ungroup()

# den nye variabel for resultatet for det sidste møde skal ikke opdeles pr. sidste hjemmemøde eller udemøde
# denne skal bare være generelt for sidste møde. derfor findes først modstanderen, og grupperes efter denne,
# og derefter laves variablen for resultatet i det sidste møde

seasons_all <- seasons_all |> 
  mutate(kamp2 = kamp) |> 
  separate_wider_delim(
    kamp2,
    delim = "-",
    names = c("hjemme", "ude")
  ) |> 
  mutate(
    modstander = if_else(str_detect(hjemme, "VFF"), ude, hjemme)
  ) |> 
  group_by(modstander) |> 
  mutate(sidste_møde_resultat = lag(vff_resultat)) |> 
  ungroup()

# da der ikke findes alt relevant vejr data før 2003, fjernes den tidligere sæson

seasons_all <- seasons_all |> 
  filter(as.numeric(sæson_år) >= 2003)

# for at lave et loop der kan hente DMI data for alle kampdagene på en gang,
# skal en ny variabel laves ud fra datotid_utc variablen
# den splittes sådan at selve datoen står for sig selv og selve klokken for sig selv
# de samles igen i en ny variabel med et T efter datoen, og et Z efter klokken,
# for at få det i det format der skal bruges i DMI's API request.
# med dette kan der laves et loop.

seasons_all <- seasons_all |> 
  mutate(datotidutc = datotid_utc) |> 
  separate_wider_delim(
    datotidutc,
    delim = " ",
    names = c("dato_dag_only", "dato_tid_only")
  ) |> 
  mutate(dmi_dato = paste0(dato_dag_only, "T", dato_tid_only, "Z"))

# datasættet med de nye variabler uploades i databasen

con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

dbWriteTable(con, "db_seasons_all", seasons_all, overwrite = TRUE)

dbDisconnect(con)

# dmi data ---------------------------------------------------------------
# for at lave loopet vælges først kun vff hjemmekampe, og kampe der ikke har et resultat 
# og ikke er relevante fjernes. med dette og datotids variable i det nye format, kan der laves et loop

vff_hjemme <- seasons_all |> 
  filter(str_detect(kamp, "VFF-")) |> 
  filter(resultat != is.na(resultat)) |> 
  filter(resultat != "Optakt" )

# vælger den nye dmi_dato og trækker den ud som en vektor, som skal bruges til at få data fra DMI

dmi_dato <- vff_hjemme |> 
  select(dmi_dato) |>
  pull()

# ved at bruge disse tidspunkter til at trække data ud fra dmi, fåes der ikke komplet data
# ved halve timer fåes der færre data punkter og variabler ud end for de hele timer,
# og for de få kampe der er startet xx:35, eller xx:05 fåes der slet ikke noget data
# derfor laves alle tidspunkter om til hele timer

# ændrer alle tider til nærmeste hele time, ved at runde ned gennem floor_date,
# og laver igen datoen til DMI API formatet

seasons_all <- seasons_all |> 
  mutate(datotid_utc = floor_date(datotid_utc, unit = "hour")) |> 
  select(-dato_dag_only, -dato_tid_only) |> 
  separate_wider_delim(
    datotid_utc,
    delim = " ",
    names = c("dato_dag_only", "dato_tid_only")
  ) |> 
  mutate(dmi_dato = paste0(dato_dag_only, "T", dato_tid_only, "Z"))

# datasættet uploades igen i databasen, for at kunne bruge den nye dmi_dato til at joine på

con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

dbWriteTable(con, "db_seasons_all", seasons_all, overwrite = TRUE)

dbDisconnect(con)

# dmi datoen trækkes ud som en vektor, sådan at den kan bruges i et loop

vff_hjemme <- seasons_all |> 
  filter(str_detect(kamp, "VFF-")) |> 
  filter(resultat != is.na(resultat)) |> 
  filter(resultat != "Optakt" )

dmi_dato <- vff_hjemme |> 
  select(dmi_dato) |>
  pull()

# vi har nu alle kampdagene, i et format der kan bruges i et loop
# bygger url op. datoen i request url kommer i for loopet

base_url <- "https://dmigw.govcloud.dk/v2/"
info_url <- "metObs/collections/observation/items?"
req_url <- "stationId=06060&datetime="
limit <- "&limit=100000"
api_key <- Sys.getenv("DMI_API_KEY")

# laver en tom liste som data skal ligge i

dato_vejr <- list()

# loop hvor vejr data for alle kamptiderne hentes

for (dato in dmi_dato) {
 full_url <- paste0(base_url, info_url, req_url, dato, limit, "&api-key=", api_key)
  
 api_call <- httr::GET(full_url)

 api_JSON <- httr::content(api_call, as = "parsed", simplifyVector = TRUE)

 dato_vejr[[as.character(dato)]] <- api_JSON
}

# sletter api_call og api_JSON

api_call <- NULL
api_JSON <- NULL

# vejrdataen ligger i dataframes, som ligger i andre dataframes, som ligger i lister,
# der ligger i en liste. der er heldigvis mønstre og ens navngivninger, så
# der laves nemt en funktion der gennem lapply køres på alle listerne

properties <- lapply(dato_vejr, function(x) x$features$properties)

# samler alt vejr data i en enkelt dataframe i stedet for en liste

vejr_alle <- bind_rows(properties)

# lægger alle vejrobservationer i SQLite database, inden der udvælges første variabler,
# i tilfælde af at andre/flere variabler skal bruges

con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

dbWriteTable(con, "db_vejr_alle", vejr_alle)

dbDisconnect(con)

vejr_alle <- dbReadTable(con, "db_vejr_alle")
# udvælger vejr variabler og lægger i en ny dataframe

vejr_udvalgt <- vejr_alle |> 
  filter(parameterId %in% c("precip_past1h", "temp_dry", "wind_speed")) |> 
  select(parameterId, value, observed)

# pivot sådan at det står rigtigt som variabler i stedet for observationer

vejr_udvalgt <- vejr_udvalgt |> 
  pivot_wider(
    names_from = parameterId,
    values_from = value
  )

# lægger de udvalgte vejrobservationer i SQLite database

con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

dbWriteTable(con, "db_vejr_udvalgte", vejr_udvalgt)

dbDisconnect(con)

# date.nager data --------------------------------------------------------

# bygger url op. request url'en består kun af årstaller, og kommer i loopet

base_url_dn <- "https://date.nager.at/api/v3/PublicHolidays/"
country_code <- "/DK"

# lave en tom liste som data skal ligge i

holidays <- list()

# loop hvor alle helligdagene i årene 2002 - 2025 hentes

for (i in 2002:2025) {
 full_url <- paste0(base_url_dn, i, country_code)
  
 api_call <- httr::GET(full_url)

 api_JSON <- httr::content(api_call, as = "parsed", simplifyVector = TRUE)

 holidays[[as.character(i)]] <- api_JSON
}

# samler data i en enkelt dataframe i stedet for en liste, omdøber variabler
# og vælger kun datoen, og hvilken helligdag det er

holidays <- holidays |> 
  bind_rows() |>  
  mutate(
    dato = date,
    helligdag = localName
  ) |> 
  select(dato, helligdag)

# uploader helligdage data til database

con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

dbWriteTable(con, "db_holidays", holidays)

dbDisconnect(con)

# joins med andet vff data -----------------------------------------------

con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

# undersøger først hvad der ligger i db_vff for at se hvordan og hvad der skal joines

db_vff <- dbReadTable(con, "db_vff")
dbListTables(con)

# det er kun d10-, d7- og d3- tilskuere der mangler i det superstats datasættet
# derfor vælges kun disse og joines med en compound key bestående af år og runde
# samtidig joines det med dmi og date.nager data

vff_all <- dbGetQuery(con,
  "SELECT s.*, r.temp_dry, r.wind_speed, r.precip_past1h, g.helligdag, 
   t.d10_tilskuere, t.d7_tilskuere, t.d3_tilskuere
   FROM db_seasons_all AS s
   LEFT JOIN db_vejr_udvalgte AS r
   ON s.dmi_dato = r.observed
   LEFT JOIN db_holidays AS g
   ON s.dato_dag_only = g.dato
   LEFT JOIN db_vff AS t
   ON s.real_år = t.år
   AND s.runde_nr = t.runde
   WHERE s.kamp LIKE '%VFF-%'"
)

dbDisconnect(con)

# nu når alle datasættene er joined og hentet fra databasen rydder vi lidt mere op i det
# de kampe der ikke har et resultat og ikke er relevante fjernes, og irrellevante variabler fravælges

vff_all <- vff_all |> 
  filter(resultat != is.na(resultat)) |> 
  filter(resultat != "Optakt" ) |> 
  mutate(datotid = as.POSIXct(datotid)) |> 
  select(-dato_tid, -kamp, -vff_resultat, -point_kamp, -hjemme_mål, -(resultat:resultat_remainder), -dommer, -tv,
         -mål_sidste_tre, -point_sæson, -dato_dag_only, -dato_tid_only, -dmi_dato, -real_år,
         -point_sidste3, -vff_mål, -ude_mål, -hjemme, -ude)

# der er stadig NA'er i de to sidste_møde variabler, så disse skal fikses
# for at fikse sidste_møde_tilskuere, laves først en ny variable der kategoriserer efter om 
# sidste_møde_tilskuere er NA eller ej

vff_all <- vff_all |> 
  mutate(nyt_hold = if_else(is.na(sidste_møde_tilskuere), 1, 0))

#filtrerer efter disse kampe, for at undersøge dem nærmere

nye_hold <- vff_all |> 
  filter(nyt_hold == 1)

# der er tydelig forskelle på størrelsen af holdene, så de deles op sådan at dem havde mindre end 4000
# tilskuere får egen kategori, og dem havde mere end 4000 tilskuere får egen kategori

vff_all <- vff_all |>
  mutate(lille_stor_ny = case_when(nyt_hold == 1 & tilskuere < 4000 ~ 1,
  nyt_hold == 1 & tilskuere > 4000 ~ 2))

# NA'erne i sidste_møde_tilskuere tildeles en ny værdi på baggrund af gennemsnitlige tilskuere for de to grupper

vff_all <- vff_all |> 
  group_by(lille_stor_ny) |> 
  mutate(avg_tilskuere = mean(tilskuere)) |> 
  ungroup() |> 
  mutate(
    sidste_møde_tilskuere = if_else(is.na(sidste_møde_tilskuere) & lille_stor_ny == 1, avg_tilskuere, sidste_møde_tilskuere),
    sidste_møde_tilskuere = if_else(is.na(sidste_møde_tilskuere) & lille_stor_ny == 2, avg_tilskuere, sidste_møde_tilskuere)
  )

# de irrelevante variabler der blev brugt til at udfylde NA'er slettes igen

vff_all <- vff_all |> 
  select(-(nyt_hold:avg_tilskuere))

# for at fikse sidste_møde_resultat ændres NA observationerne til "første møde"

vff_all <- vff_all |> 
  mutate(sidste_møde_resultat = if_else(is.na(sidste_møde_resultat), "første møde", sidste_møde_resultat))

# NA'er i helligdage ændres til at hedde ingen, og observationer med manglende vejr data slettes

vff_all <- vff_all |> 
  mutate(
    helligdag = if_else(is.na(helligdag), "ingen", helligdag)
  ) |> 
  filter(!is.na(precip_past1h))

# ny variabel for om det er sommerferie eller ej

vff_all <- vff_all |> 
  mutate(
  sommerferie = as.factor(if_else(month(datotid) == 07 | (day(datotid) %in% 26:30 & month(datotid) == 06) | (day(datotid) %in% 1:12 & month(datotid) == 08), 1, 0))
)

# sæson_år variablen står stadig som en karakter, så denne ændres til numerisk

vff_all <- vff_all |> 
  mutate(sæson_år = as.numeric(sæson_år))

# til sidst laves alle de kategoriske variabler til faktorer, og datasættet gemmes derefter
# som en rds fil

vff_all <- vff_all |> 
  mutate(across(where(is.character), as.factor))

write_rds(vff_all, "data/vff_all.rds")
