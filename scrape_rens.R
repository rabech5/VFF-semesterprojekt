# load packages

pacman::p_load(tidyverse, rvest, janitor)

# link til superstats, uden sæson år (kommer i loop funktion)

url <- "https://superstats.dk/program?season="

# laver en tom liste som tabellerne skal ligge i
seasons <- list()

# loop hvor hver sæson kommer som en liste, med runder som tabeller
# alle listerne sættes i den tomme liste

for (i in 2003:2026) {
  html <- read_html(paste0(url, i))
  
  tables <- html |>
    html_element("#content") |>
    html_elements("table") |>
    html_table()

  seasons[[as.character(i)]] <- tables
}

# sletter tables

tables <- NULL

# navngivning af hver sæson
# names(seasons) <- 2003:2026

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
  flatten() |> 
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

# finder alle VFF's hjemmekampe og gemmer dem i et nyt objekt

vff_hjemme <- seasons_all |> 
  filter(str_detect(kamp, "VFF-"))

# fjerner de kamper der ikke har et resultat og ikke er relevante

vff_hjemme <- vff_hjemme |> 
  filter(resultat != is.na(resultat)) |> 
  filter(resultat != "Optakt" ) |> 
  mutate(datotid = ydm_hm(paste(sæson_år, dato_tid), tz = "UTC"))

# finder kampdage, som skal bruges til at få data fra DMI

library(lubridate)

kampdage <- vff_hjemme |> 
  select(datotid)




