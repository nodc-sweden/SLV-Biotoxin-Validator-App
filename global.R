# This file is sourced once, when the app is started
# (before ui.R and server.R are loaded)

# ---- Libraries ----
require(shiny)
require(tidyverse)
require(readxl)
require(sf)
require(leaflet)
require(worrms)
require(DT)
require(memoise)

# ---- Helper functions ----
source("R/helper.R")

# ---- App metadata ----
pkg_version <- read.dcf("DESCRIPTION", fields = "Version")[1]
github_url <- "https://github.com/nodc-sweden/SLV-Biotoxin-Validator-App"

# ---- Static configuration ----
column_mapping <- c(
  "ORDERER" = "Kund",
  "SDATE" = "Provtagningsdatum:",
  "ANADATE" = "Analys påbörjad den",
  "LATNM" = "scientificname",
  "SMPNO" = "Prov ID"
)

coordinate_column <- "GPS-koord."
site_column <- "Provtagningsplats:"
taxa_column <- "Provmärkning"
unused_columns <- c("Ankomstdatum", "Prov validerat den", "Eurofins provnummer", "Provets status")

# Read in static configuration files
config_areas <- read_excel("config/production_areas.xlsx", progress = FALSE)
toxin_list   <- read_excel("config/lista_toxiner.xlsx", progress = FALSE)
coastline    <- st_read("data/shapefiles/EEA_Coastline_Sweden_WestCoast.shp", quiet = TRUE)
