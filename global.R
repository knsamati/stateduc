# PACKAGES ----------------------------------------------------------------

library(shiny)
library(tidyr)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(reactable) 
library(sf)
library(leaflet)
library(duckplyr)

# DATA --------------------------------------------------------------------

df_ecole <- arrow::read_parquet("data/df_ecole.parquet") |> 
  mutate(annee_scolaire = case_when(
    CODE_TYPE_ANNEE==14 ~ "2013-2014",
    CODE_TYPE_ANNEE==15 ~ "2014-2015",
    CODE_TYPE_ANNEE==16 ~ "2015-2016",
    CODE_TYPE_ANNEE==17 ~ "2016-2017",
    CODE_TYPE_ANNEE==18 ~ "2017-2018"
  ))
df_iepp <- arrow::read_parquet("data/df_iepp.parquet") |> 
  mutate(annee_scolaire = case_when(
    CODE_TYPE_ANNEE==14 ~ "2013-2014",
    CODE_TYPE_ANNEE==15 ~ "2014-2015",
    CODE_TYPE_ANNEE==16 ~ "2015-2016",
    CODE_TYPE_ANNEE==17 ~ "2016-2017",
    CODE_TYPE_ANNEE==18 ~ "2017-2018"
  ))
df_dre <- arrow::read_parquet("data/df_dre.parquet") |> 
  mutate(annee_scolaire = case_when(
    CODE_TYPE_ANNEE==14 ~ "2013-2014",
    CODE_TYPE_ANNEE==15 ~ "2014-2015",
    CODE_TYPE_ANNEE==16 ~ "2015-2016",
    CODE_TYPE_ANNEE==17 ~ "2016-2017",
    CODE_TYPE_ANNEE==18 ~ "2017-2018"
  ))

carte <- st_read("data/fond_etab.gpkg")
etab <- st_read("data/geo_etab1.gpkg")


geo_etab <- etab |>  
  left_join(df_ecole,by = join_by(CodeAdmin == CODE_ADMINISTRATIF)) |>
  st_set_crs(NA)


geo_etab$lon <- st_coordinates(geo_etab)[,1]
geo_etab$lat <- st_coordinates(geo_etab)[,2]

geo_etab <- st_drop_geometry(geo_etab)


#df <- df %>% replace(., is.na(.), 0)

## dropdown selector options

possible_annee <- sort(unique(df_ecole$annee_scolaire))

possible_ecole <- sort(unique(df_ecole$NOM_ETABLISSEMENT))

source("modules/modalEcole.R")
source("modules/modalAnnu.R")

# BOOKMARKING -------------------------------------------------------------

## set bookmarking to be server-based rather than URL-based
enableBookmarking(store="server")

## Create custom Modal to pop up when a user bookmarks or "Shares"
showBookmarkUrlModalCustom <- function(url){
  store <- getShinyOption("bookmarkStore", default = "")
  if (store == "url") {
    subtitle <- "This link stores the current state of this application."
  }
  else if (store == "server") {
    subtitle <- "The current state of this application has been stored on the server."
  }
  else {
    subtitle <- NULL
  }
  showModal(urlModal(url, title = "Share a link to this table.", subtitle = "Copy this link to share this table."))
  
}

