# ============================================
# SLV Biotoxin Validator App
# ============================================

# ---- Libraries ----
require(shiny)
require(tidyverse)
require(readxl)
require(sf)
require(leaflet)
require(worrms)
require(DT)

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
