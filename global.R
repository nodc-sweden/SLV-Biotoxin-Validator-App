# This file is sourced once, when the app is started
# (before ui.R and server.R are loaded)

# ---- Libraries ----
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(lubridate)
library(readxl)
library(sf)
library(leaflet)
library(worrms)
library(DT)
library(memoise)

# ---- Max upload size (20 MB) ----
options(shiny.maxRequestSize = 20 * 1024^2)

# ---- Helper functions ----
source("R/helper.R")
source("R/validate_columns.R")
source("R/plotting.R")

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
config_areas <- tryCatch(
  read_excel("config/production_areas.xlsx", progress = FALSE),
  error = function(e) stop("Failed to read config/production_areas.xlsx: ", e$message)
)
toxin_list <- tryCatch(
  read_excel("config/lista_toxiner.xlsx", progress = FALSE),
  error = function(e) stop("Failed to read config/lista_toxiner.xlsx: ", e$message)
)
coastline <- tryCatch(
  st_read("data/shapefiles/EEA_Coastline_Sweden_WestCoast.shp", quiet = TRUE),
  error = function(e) stop("Failed to read coastline shapefile: ", e$message)
)

# ---- Precompute coastline buffer for is_near_land ----
# This avoids expensive spatial operations (union, buffer, transform) on every upload
coastline_buffer <- precompute_coastline_buffer(
  coastline,
  distance = -10,
  crs = 4326,
  utm_zone = 33,
  remove_small_islands = TRUE,
  small_island_threshold = 2000000
)
