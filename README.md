# SLV Marine Biotoxin Data Validation App

## Overview
This [Shiny application](https://nodc-sweden.shinyapps.io/biotoxin-validator/) validates marine biotoxin data from the Swedish Food Agency (SLV). 
It allows users to upload Excel files containing detailed sampling data, verify geographic coordinates, 
validate taxonomic names, and visualize data through interactive maps and plots. After validation,
data are downloaded in a format suitable for the [SHARK](https://shark.smhi.se/) database.

## Features
- **File Upload:** Supports Excel (.xlsx) files.
- **Raw Data Display:** View uploaded data in a searchable tabular format.
- **Map Validation:** Visualize sampling locations using Leaflet maps.
- **Coordinate Validation:** Identify missing or erroneous coordinates.
- **Taxa Validation:** Cross-check species names using the WoRMS database.
- **Site Validation:** Extract and verify site names and numbers.
- **Time Series Plot:** Explore trends over time for selected parameters.
- **Geographical Plot:** Generate spatial plots for biotoxin levels.
- **Download Processed Data:** Export validated data as a text file for [SHARK](https://shark.smhi.se/).

## Installation
To run the app locally, clone this repo and ensure that you have R and the necessary libraries installed.

### Install Required R Packages
```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_deps()
```

### Running the App
```r
shiny::runApp("app.R")
```

## Usage
1. **Upload Data:** Click "Upload Excel File" and select a valid `.xlsx` file.
2. **Explore Data:** Navigate through tabs to inspect and validate different aspects of the dataset.
3. **Check Coordinates:** Ensure sampling sites are correctly placed on the map.
4. **Validate Taxa:** Confirm taxonomic names using the WoRMS database.
5. **Download Processed File:** Once validated, download the cleaned data in text format.

## File Format Requirements
- The Excel file should be in the format of a data export from Eurofins, and contain a column for GPS coordinates (`GPS-koord.`).
- Sampling sites must have a valid name and number.
- Taxa names should follow standard nomenclature for accurate validation.

## Data Processing
- **Coordinate Conversion:** Converts DDMM format to decimal degrees.
- **Land Detection:** Identifies sites that are potentially placed on land.
- **Taxonomic Validation:** Matches names with WoRMS database.
- **Site Extraction:** Parses and extracts relevant site details.

## Config files
This Shiny app uses three configuration files to ensure that toxins, production areas, and other relevant data are correctly mapped to the SHARK format. Additional toxins, production areas, and other entries can be added as needed.
- **`config/Format_Marine_Biotoxin.xlsx`** – Defines the SHARK data delivery format.
- **`config/lista_toxiner.xlsx`** – Contains toxin metadata and parameter mappings.
- **`config/production_areas.xlsx`** – Provides metadata for each production area.

## Deployment to Shinyapps.io
This repository is automatically deployed as a Shinyapps.io [web application](https://nodc-sweden.shinyapps.io/biotoxin-validator/) on every push to the `main` branch through GitHub Actions. When opening a pull request, the merging branch is deployed to a separate [test application](https://nodc-sweden.shinyapps.io/biotoxin-validator-test/).

## License
This project is licensed under the MIT License.
