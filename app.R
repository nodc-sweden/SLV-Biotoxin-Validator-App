#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load required libraries
require(shiny)
require(tidyverse)
require(readxl)
require(sf)
require(leaflet)
require(worrms)
require(DT)

# Load helper functions
source("R/helper.R")

# Read version from DESCRIPTION
pkg_version <- read.dcf("DESCRIPTION", fields = "Version")[1]

# GitHub repo link
github_url <- "https://github.com/nodc-sweden/SLV-Biotoxin-Validator-App"

# Load shapefile for Swedish Westcoast
coastline <- st_read("data/shapefiles/EEA_Coastline_Sweden_WestCoast.shp", quiet = TRUE)

# Read list of toxins
toxin_list <- read_excel("config/lista_toxiner.xlsx", progress = FALSE) %>%
  select(`Rapporterat-parameternamn`, Kortnamn_MH, Enhet_MH_kg, Enhet_MH_l, Parameternamn_MH, `Gränsvärde_kommersiell_försäljning`) %>%
  drop_na(Kortnamn_MH)

# Map Metadata headers to the correct column names
column_mapping <- c(
  "ORDERER" = "Kund",
  "SDATE" = "Provtagningsdatum:",
  "ANADATE" = "Analys påbörjad den",
  "LATNM" = "scientificname",
  "SMPNO" = "Prov ID"
)

# Define column names
coordinate_column <- "GPS-koord."
site_column <- "Provtagningsplats:"
taxa_column <- "Provmärkning"
unused_columns <- c("Ankomstdatum", "Prov validerat den", "Eurofins provnummer", "Provets status")

# Define UI for application
ui <- fluidPage(
  titlePanel("SLV Marine Biotoxin Data Validation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Eurofins excel file", accept = ".xlsx"),
      fileInput("file_summary", "Upload summary excel file (with info about wild/farmed)", accept = ".xlsx"),
      selectInput("coordinate_output", "Use position:", 
                  choices = c("Reported GPS position" = "actual", "Midpoint production area" = "midpoint"), 
                  selected = "midpoint"),
      selectInput("sample_type", "Sample type:", 
                  choices = c("Animal flesh" = "live_bivalve_molluscs_v2", "Water" = "watersample"), 
                  selected = "live_bivalve_molluscs_v2"),
      downloadButton("download", "Download Processed .txt File"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 h4("Issue Summary"),
                 DTOutput("table_summary"),
                 h4("Unknown Columns"),
                 DTOutput("unmapped_data")
        ),
        tabPanel("Map", leafletOutput("map", height = "800px")),
        tabPanel("Coordinate Validation", h4("Issues Found"), DTOutput("table_missing")),
        tabPanel("Taxa Validation", 
                 h4("Issues Found"),
                 DTOutput("table_taxa"),
                 h4("Valid Entries"),
                 DTOutput("table_taxa_valid")
        ),
        tabPanel("Site Validation",
                 h4("Issues Found"),
                 DTOutput("table_sites"),
                 h4("Valid Entries"),
                 DTOutput("table_sites_valid")
        ),
        tabPanel("Origin Validation", 
                 h4("Duplicate Inputs"), 
                 DTOutput("table_origin"),
                 h4("Missing Origin Input"),
                 DTOutput("table_missing_origin")),
        tabPanel("Time Series Plot",
                 fluidRow(
                   column(6, selectInput("selected_param", "Select Toxin:", choices = NULL)),
                   column(6, selectInput("log_scale", "Log Scale:", choices = c("No" = "none", "Yes" = "log10")))
                 ),
                 plotOutput("time_series_plot", height = "1000px")
        ),
        tabPanel("Geographical Plot",
                 fluidRow(
                   column(4, selectInput("selected_taxa", "Select Taxa:", choices = NULL)),
                   column(4, selectInput("selected_param_map", "Select Toxin:", choices = NULL)),
                   column(4, selectInput("log_scale_map", "Log Scale:", choices = c("No" = "none", "Yes" = "log10")))
                 ),
                 plotOutput("spatial_plot", height = "800px")
        ),
        tabPanel("Original Data", DTOutput("table_raw")),
        tabPanel("About",
                 fluidRow(
                   column(12, 
                          h3("About This Application"),
                          p("This Shiny application provides tools for validating and processing marine biotoxin data collected by the SLV."),
                          p("Instructions:"),
                          tags$ul(
                            tags$li("Upload and validate biotoxin data from Excel files, following the export format from Eurofins."),
                            tags$li("Upload a summary Excel files, containing information on Origin (Wild/Culutured). Required columns are År, Mån, Dat, Nr, Art and V/O."),
                            tags$li("Visualize sampling locations on an interactive map."),
                            tags$li("Check for missing or incorrect coordinates. Data in red color will require action before data submission to SHARK, orange may need attention."),
                            tags$li("Validate taxonomic names using the World Register of Marine Species (WoRMS). Data in red color require action."),
                            tags$li("Analyze site names and their corresponding regions. Data in red color require action."),
                            tags$li("Explore time series and spatial trends."),
                            tags$li(HTML('After validation, download a processed data file that can be delivered to <a href="https://shark.smhi.se/" target="_blank">SHARK</a>.'))))
                 ),
                 tags$footer(
                   style = "text-align:center; padding:10px; font-size:0.9em; color:#666;",
                   HTML(
                     paste0(
                       "Version ", pkg_version, " – ",
                       "<a href='", github_url, "' target='_blank'>GitHub repository</a>"
                     )
                   )
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file, site_df_data())
    df <- read_excel(input$file$datapath, skip = 1, .name_repair = "none", progress = FALSE)
    header <- names(read_excel(input$file$datapath, .name_repair = "none", progress = FALSE))
    colnames(df)[colnames(df) == ""] <- header[colnames(df) == ""]
    
    df <- df %>%
      mutate(
        LATIT = convert_ddmm_to_dd(substr(.[[coordinate_column]], 1, 6)),
        LONGI = convert_ddmm_to_dd(substr(.[[coordinate_column]], 8, 13))
      )
    
    if (input$coordinate_output == "midpoint") {
      site_df <- site_df_data()
      
      # Add site info
      df$PROD_AREA <- site_df$Produktionsområde
      df$PROD_AREA_ID <- site_df$number
      df$Mittpunkt_E_SWEREF99 <- site_df$Mittpunkt_E_SWEREF99
      df$Mittpunkt_N_SWEREF99 <- site_df$Mittpunkt_N_SWEREF99
      
      # Drop old decimal-degree columns if they exist (won't error if they don't)
      df <- df %>%
        select(-any_of(c("LATIT", "LONGI")))
      
      # Which rows have valid SWEREF99 TM coords?
      idx <- which(!is.na(df$Mittpunkt_E_SWEREF99) &
                     !is.na(df$Mittpunkt_N_SWEREF99))
      
      # Pre-allocate output vectors (NA where coords are missing)
      LONGI <- rep(NA_real_, nrow(df))
      LATIT <- rep(NA_real_, nrow(df))
      
      # Transform only valid rows
      if (length(idx) > 0) {
        pts <- st_as_sf(
          df[idx, ],
          coords = c("Mittpunkt_E_SWEREF99", "Mittpunkt_N_SWEREF99"),
          crs = 3006             # SWEREF99 TM
        ) %>%
          st_transform(4326)      # WGS84 decimal degrees
        
        xy <- st_coordinates(pts) # X = lon, Y = lat
        LONGI[idx] <- xy[, 1]
        LATIT[idx] <- xy[, 2]
      }
      
      # Attach back to your data
      df$LONGI <- LONGI
      df$LATIT <- LATIT
    }
    
    # Round coordinates
    df$LONGI <- round(df$LONGI, 4)
    df$LATIT <- round(df$LATIT, 4)
    
    df <- df %>%
      mutate(on_land = ifcb_is_near_land(
        LATIT, 
        LONGI, 
        shape = "data/shapefiles/EEA_Coastline_Sweden_WestCoast.shp",
        distance = -10)
      )
    
    return(df)
  })
  
  data_summary <- reactive({
    req(input$file_summary)
    # Read file
    df <- read_excel(input$file_summary$datapath, .name_repair = "none", progress = FALSE)
    
    # Define required column names
    required_cols <- c("År", "Mån", "Dat", "Nr", "Art", "V/O")
    
    # Check if all required columns exist
    missing_cols <- setdiff(required_cols, colnames(df))
    
    if (length(missing_cols) > 0) {
      showNotification(paste("The uploaded file is missing the following required columns:", 
                             paste(missing_cols, collapse = ", ")), type = "error", duration = 5)
      return(NULL)  # Stop further execution
    }
    
    # Process dataframe
    df <- df %>%
      mutate(SDATE = as.character(make_date(year = År, month = Mån, day = Dat))) %>%
      select(SDATE, Havsområde, Nr, Art, `V/O`) %>%
      filter(complete.cases(.))
    
    taxa <- df %>% select(Art) %>% distinct()
    taxa_names <- taxa$Art
    records <- NULL
    
    for (i in seq_along(taxa_names)) {
      record <- tryCatch(
        cbind(Art = taxa_names[i], wm_records_name(taxa_names[i], fuzzy = FALSE)),
        error = function(e) return(NULL)
      )
      records <- bind_rows(records, record)
    }
    
    summary <- df %>%
      left_join(records, by = "Art") %>%
      select(Nr, SDATE, `V/O`, scientificname) %>%
      rename(LATNM = scientificname,
             SAMPLE_ORIGIN = `V/O`,
             PROD_AREA_ID = Nr) %>%
      mutate(SAMPLE_ORIGIN = str_to_sentence(SAMPLE_ORIGIN),
             PROD_AREA_ID = as.character(PROD_AREA_ID)) %>%
      distinct() %>%
      group_by(PROD_AREA_ID, SDATE, LATNM) %>%
      filter(n_distinct(SAMPLE_ORIGIN) == 1) %>%  # Keep only groups with one unique SAMPLE_ORIGIN
      ungroup()
    
    problems <- df %>%
      group_by(Nr, Havsområde, SDATE, Art) %>%
      summarize(n_origins = n_distinct(`V/O`), .groups = "drop") %>%
      filter(n_origins > 1)
    
    problem_rows <- df %>%
      semi_join(problems, by = c("Nr", "Havsområde", "SDATE", "Art")) %>%
      rename(Date = SDATE)
    
    return(list(summary = summary, problems = problem_rows))
  })
  
  processed_data <- reactive({
    
    site_df <- site_df_data()  # Get the site_df from the reactive object
    
    data <- data()
    
    # Check if file_summary has been uploaded
    if (!is.null(input$file_summary)) {
      data_summary <- data_summary()$summary
    } else {
      data_summary <- NULL
    }
    
    # Use the stored `taxa` for joining
    taxa <- taxa_data$taxa
    
    # Define which unit column to use (e.g., based on input or condition)
    selected_unit_column <- if (input$sample_type == "live_bivalve_molluscs_v2") "Enhet_MH_kg" else "Enhet_MH_l"
    
    # Create a named vector for renaming
    rename_map <- setNames(toxin_list$Kortnamn_MH, toxin_list$`Rapporterat-parameternamn`)
    
    # Only remap existing colnames
    rename_map <- rename_map[names(rename_map) %in% names(data)]
    
    # Extract logical columns
    logical_cols <- toxin_list %>%
      filter(Enhet_MH_kg == "true or false")
    
    data_renamed <- data %>%
      rename_with(~ rename_map[.x], .cols = all_of(names(rename_map))) %>%
      select(where(~ !all(is.na(.))))
    
    # Rename parameters
    data <- data %>%
      mutate(across(all_of(logical_cols$`Rapporterat-parameternamn`), ~ case_when(
        . == "Ej påvisad" ~ FALSE, 
        . == "~PV0016C" ~ FALSE,
        . == "Påvisad" ~ TRUE,
        TRUE ~ NA_real_
      ))) %>%
      rename_with(~ rename_map[.x], .cols = all_of(names(rename_map)))
    
    # Loop through each parameter in toxin_list
    for (param in toxin_list$Kortnamn_MH) {
      # Create the new Q column name
      q_col <- paste0("Q_", param)
      
      # Ensure the column exists in data
      if (param %in% names(data) & !param %in% logical_cols$Kortnamn_MH) {
        # Extract the "<" signs into the new column
        data[[q_col]] <- ifelse(grepl("^<\\s*", data[[param]]), "<", NA)
        
        # Remove the "<" sign (with optional spaces) and convert to numeric
        data[[param]] <- gsub("^<\\s*", "", data[[param]])
        data[[param]][!grepl("^[0-9.]+$", data[[param]])] <- NA
        data[[param]] <- as.numeric(data[[param]])
      }
    }
    
    # Transform logical colunmns
    data <- data %>%
      mutate(across(all_of(logical_cols$Kortnamn_MH), ~ as.logical(.)))
    
    # Add taxa info
    data <- data %>% left_join(taxa, by = "Provmärkning")
    
    # Add site info
    data$PROD_AREA <- site_df$Produktionsområde
    data$PROD_AREA_ID <- site_df$number
    
    areas <- read_excel("config/production_areas.xlsx", progress = FALSE)
    
    data <- data %>%
      left_join(areas, by = c("PROD_AREA_ID" = "Nummer"))
    
    taxa <- data %>% select(Provmärkning) %>% distinct()
    
    data_mapped <- data %>%
      left_join(taxa, by = "Provmärkning") %>%
      rename(!!!column_mapping) %>%
      mutate(
        ORDERER = recode(ORDERER, "Livsmedelsverket" = "SLV"),
        PROJ = "SLV",
        MYEAR = lubridate::year(SDATE)
      ) %>%
      mutate(across(everything(), as.character))
    
    # Only join data_summary if file_summary exists
    if (!is.null(data_summary)) {
      data_mapped <- data_mapped %>%
        left_join(data_summary, by = c("SDATE", "LATNM", "PROD_AREA_ID"), relationship = "many-to-many")
    }
    
    # Select file based on sample type
    file_to_use <- paste0("config/Format_Marine_Biotoxin_", input$sample_type, ".xlsx")
    
    template <- read_excel(file_to_use, skip = 2, progress = FALSE)[-1]
    
    template_headers <- template[0,] %>%
      mutate(across(everything(), as.character))
    
    data_out <- template_headers %>%
      bind_rows(data_mapped %>% select(any_of(names(template_headers))))
    
    data_out <- data_out %>%
      mutate(
        POSYS = case_when(
          !is.na(LATIT) & !is.na(LONGI) & input$coordinate_output == "midpoint" ~ "Centroid",
          !is.na(LATIT) & !is.na(LONGI) & input$coordinate_output != "midpoint" ~ "GPS",
          TRUE ~ NA_character_
        ),
        COMNT_VISIT = case_when(
          !is.na(LATIT) & !is.na(LONGI) & input$coordinate_output == "midpoint" ~ "The given position is the centroid point of the production area. Data is collected from the entire area, and the coordinate uncertainty reflects the extent of this area.",
          !is.na(LATIT) & !is.na(LONGI) & input$coordinate_output != "midpoint" ~ NA,
          TRUE ~ NA_character_
        ),
        SMTYP = "HAN",
        SAMPLE_ORIGIN = recode(SAMPLE_ORIGIN,
                               "Vilda" = "Wild",
                               "Odlade" = "Farmed",
                               .default = SAMPLE_ORIGIN,
                               .missing = SAMPLE_ORIGIN),
        MNDEP = 0,
        MXDEP = 0
      )
    
    data_validation <- data_out %>%
      select(where(~ !all(is.na(.))))
    
    problem_columns <- names(data_renamed)[!names(data_renamed) %in% names(data_validation)]
    
    # Identify columns that could not be renamed
    problem_columns <- problem_columns[!problem_columns %in% c("LATIT", "LONGI", "on_land", "Mittpunkt_E_SWEREF99", "Mittpunkt_N_SWEREF99")]
    problem_columns <- problem_columns[!problem_columns %in% unused_columns]
    problem_columns <- problem_columns[!problem_columns %in% column_mapping]
    problem_columns <- problem_columns[!problem_columns %in% coordinate_column]
    problem_columns <- problem_columns[!problem_columns %in% site_column]
    problem_columns <- problem_columns[!problem_columns %in% taxa_column]
    
    # Reverse the map: keys become values and vice versa
    reverse_map <- setNames(names(rename_map), rename_map)
    
    # Translate only the columns that exist in the reverse map
    renamed_columns <- sapply(problem_columns, function(x) {
      if (x %in% names(reverse_map)) {
        reverse_map[x]
      } else {
        x  # Keep the original name if not found in the map
      }
    }, USE.NAMES = FALSE)
    
    # Now match the problem_columns with the reverse map
    renamed_columns <- as.character(renamed_columns)
    
    missing_columns <- tibble("Uninitialized column" = renamed_columns, "Column key" = problem_columns)
    
    # Use dynamically selected unit column
    units <- toxin_list %>%
      select(Kortnamn_MH, !!sym(selected_unit_column)) %>%
      rename(Unit = !!sym(selected_unit_column))
    
    missing_columns <- missing_columns %>%
      left_join(units, by = c("Column key" = "Kortnamn_MH"))
    
    return(list(data = data_out, renamed_columns = missing_columns))
  })
  
  # Create a reactiveValues object to store taxa
  taxa_data <- reactiveValues(taxa = NULL)
  
  # Update `taxa` within reactive functions
  observe({
    df <- data()
    taxa <- df %>% select(Provmärkning) %>% distinct()
    taxa_names <- taxa$Provmärkning
    records <- NULL
    
    for (i in seq_along(taxa_names)) {
      record <- tryCatch(
        cbind(Provmärkning = taxa_names[i], wm_records_name(taxa_names[i], fuzzy = FALSE)),
        error = function(e) return(NULL)
      )
      records <- bind_rows(records, record)
    }
    
    taxa <- taxa %>%
      left_join(records, by = "Provmärkning") %>%
      select(Provmärkning, AphiaID, scientificname)
    
    # Store the `taxa` data in the reactiveValues
    taxa_data$taxa <- taxa
  })
  
  issues <- reactive({
    df <- data()
    taxa <- taxa_data$taxa
    site_df <- site_df_data()
    processed_df <- processed_data()$data
    
    # Date issues
    date_issues <- df %>%
      filter(is.na(`Provtagningsdatum:`)) %>%
      nrow()
    
    # Coordinate issues
    coord_issues <- df %>%
      filter(on_land == TRUE | is.na(LATIT) | is.na(LONGI)) %>%
      nrow()
    
    # Taxa issues
    taxa_issues <- df %>%
      left_join(taxa, by = "Provmärkning") %>%
      filter(is.na(scientificname)) %>%
      nrow()
    
    # Origin issues
    origin_missing <- processed_df %>%
      filter(is.na(SAMPLE_ORIGIN))
    
    # Origin issues
    origin_issues <- origin_missing %>%
      nrow()
    
    # Site issues
    df$PROD_AREA <- site_df$Produktionsområde
    df$PROD_AREA_ID <- site_df$number
    df$`SLV namn` <- site_df$site
    
    site_issues <- df %>%
      filter(is.na(PROD_AREA_ID)) %>%
      nrow()
    
    # Return as a list so each can be accessed
    list(
      coord_issues = coord_issues,
      taxa_issues = taxa_issues,
      site_issues = site_issues,
      date_issues = date_issues,
      origin_issues = origin_issues,
      origin_missing = origin_missing
    )
  })
  
  output$table_summary <- renderDT({
    summary_df <- data.frame(
      Validation = c(
        "Coordinate Issues",
        "Taxa Issues",
        "Site Issues",
        "Missing Sampling Dates",
        "Origin Issues (Wild/Cultured)"
      ),
      "Number of issue rows" = c(
        issues()$coord_issues,
        issues()$taxa_issues,
        issues()$site_issues,
        issues()$date_issues,
        issues()$origin_issues
      ),
      check.names = FALSE
    )
    
    withProgress(message = "Loading data...", value = 0.5, {
      datatable(summary_df, options = list(dom = 't', rowCallback = JS(
        "function(row, data) { 
        $(row).css('color', 'red'); 
      }"
      )))
    })
  })
  
  output$unmapped_data <- renderDT({
    validate(need(input$file, "Waiting for file upload..."))
    datatable(processed_data()$renamed_columns, options = list(
      pageLength = 25,
      language = list(emptyTable = "All columns are mapped")))
  })
  
  output$table_raw <- renderDT({
    validate(need(input$file, "Waiting for file upload..."))
    
    df <- data() %>%
      select(-LATIT, -LONGI, -on_land)
    
    datatable(df)
  })
  
  output$map <- renderLeaflet({
    validate(need(input$file, "Waiting for file upload..."))
    
    df_map <- data() %>%
      filter(!is.na(LATIT) & !is.na(LONGI))
    leaflet(df_map) %>%
      addTiles() %>%
      addCircleMarkers(
        ~LONGI, ~LATIT,
        radius = 5,
        color = ~ifelse(on_land == TRUE, "red", "blue"),
        popup = ~paste(
          "Lon:", LONGI, 
          "<br>Lat:", LATIT,
          "<br>GPS-koord:", `GPS-koord.`,
          "<br>Provtagningsdatum:", `Provtagningsdatum:`,
          "<br>Provtagningsplats:", `Provtagningsplats:`,
          "<br>Provmärkning:", `Provmärkning`
        )      )
  })
  
  output$table_missing <- renderDT({
    validate(need(input$file, "Waiting for file upload..."))
    df_missing <- data() %>%
      filter(on_land == TRUE | is.na(LATIT) | is.na(LONGI))
    
    if (input$coordinate_output == "midpoint") {
      df_missing <- df_missing %>%
        mutate(midoint = paste(LATIT, LONGI, sep = ", ")) %>%
        select(`Provtagningsplats:`, `Provtagningsdatum:`, `GPS-koord.`, midoint, on_land) %>%
        rename(`Reported Site` = `Provtagningsplats:`,
               Date = `Provtagningsdatum:`,
               `GPS-coord` = `GPS-koord.`,
               `Midpoint coordinate` = midoint,
               `On land` = on_land)
    } else {
      df_missing <- df_missing %>%
        select(`Provtagningsplats:`, `Provtagningsdatum:`, `GPS-koord.`, on_land) %>%
        rename(`Reported Site` = `Provtagningsplats:`,
               Date = `Provtagningsdatum:`,
               `GPS-coord` = `GPS-koord.`,
               `On land` = on_land)
    }
    
    datatable(df_missing, options = list(
      pageLength = 25, 
      language = list(emptyTable = "No issues found"),
      rowCallback = JS(
        "function(row, data) { 
        var onLand = data[4];
        if (onLand == true) {
          $(row).css('color', 'orange');
        } else {
          $(row).css('color', 'red');
        }
      }"
      )
    ))
  })
  
  output$table_taxa <- renderDT({
    validate(need(input$file, ""))
    taxa <- taxa_data$taxa %>%
      filter(is.na(scientificname)) %>%
      rename(`Scientific Name` = scientificname,
             `Reported Scientific Name` = Provmärkning)
    
    datatable(taxa, options = list(pageLength = 25, language = list(emptyTable = "No issues found"), rowCallback = JS(
      "function(row, data) { 
        $(row).css('color', 'red'); 
      }"
    )))
  })
  
  output$table_taxa_valid <- renderDT({
    validate(need(input$file, "Waiting for file upload..."))
    taxa_valid <- taxa_data$taxa %>%
      filter(!is.na(scientificname)) %>%
      arrange(scientificname) %>%
      rename(`Scientific Name` = scientificname,
             `Reported Scientific Name` = Provmärkning)
    
    datatable(taxa_valid, options = list(pageLength = 25, rowCallback = JS(
      "function(row, data) { 
        $(row).css('color', 'green'); 
      }"
    )))
  })
  
  # Store site_df in a reactive object
  site_df_data <- reactive({
    validate(need(input$file, "Waiting for file upload..."))
    areas <- read_excel("config/production_areas.xlsx", progress = FALSE)
    
    # Just read enough of the uploaded file to extract sites
    df <- read_excel(input$file$datapath, skip = 1, .name_repair = "none", progress = FALSE)
    header <- names(read_excel(input$file$datapath, .name_repair = "none", progress = FALSE))
    colnames(df)[colnames(df) == ""] <- header[colnames(df) == ""]
    
    # Apply the function to each entry in the data frame
    result <- sapply(df$`Provtagningsplats:`, extract_site_and_number, simplify = FALSE)
    
    # Convert the result to a data frame for easier handling
    site_df <- do.call(rbind, lapply(result, function(x) data.frame(site = x$site, number = x$number)))
    
    # Remove potential noise
    site_df$site <- gsub("/", "", site_df$site)
    site_df$site <- trimws(site_df$site)
    site_df$number <- as.character(site_df$number)
    
    site_df <- site_df %>% left_join(areas, by = c("number" = "Nummer"))
    
    return(site_df)
  })
  
  output$table_sites <- renderDT({
    validate(need(input$file, ""))
    site_df <- site_df_data()  # Get the site_df from the reactive object
    
    df <- data()
    
    # Add the final output
    df$`SLV Produktionsområde` <- site_df$Produktionsområde
    df$PROD_AREA_ID <- site_df$number
    df$`SLV namn` <- site_df$site
    
    locations <- df %>%   
      group_by(`Provtagningsplats:`, PROD_AREA_ID, `SLV Produktionsområde`) %>%
      summarise(`N visits` = n(), .groups = "drop") %>%
      rename(`Reported Site` = `Provtagningsplats:`,
             `SLV Area` = `SLV Produktionsområde`,
             `SLV Area Number` = PROD_AREA_ID) %>%
      filter(is.na(`SLV Area`)) %>%
      arrange(`SLV Area Number`)
    
    datatable(locations, options = list(pageLength = 25, 
                                        language = list(emptyTable = "No issues found"),
                                        rowCallback = JS(
                                          "function(row, data) { 
                                            if (data[3] === null) { 
                                              $(row).css('color', 'red'); 
                                            } else { 
                                              $(row).css('color', 'green'); 
                                            } 
                                          }"
                                        )))
  })
  
  output$table_sites_valid <- renderDT({
    validate(need(input$file, "Waiting for file upload..."))
    site_df <- site_df_data()  # Get the site_df from the reactive object
    
    df <- data()
    
    # Add the final output
    df$`SLV Produktionsområde` <- site_df$Produktionsområde
    df$PROD_AREA_ID <- site_df$number
    df$`SLV namn` <- site_df$site
    
    locations <- df %>%   
      group_by(`Provtagningsplats:`, PROD_AREA_ID, `SLV Produktionsområde`) %>%
      summarise(`N visits` = n(), .groups = "drop") %>%
      rename(`Reported Site` = `Provtagningsplats:`,
             `SLV Area` = `SLV Produktionsområde`,
             `SLV Area Number` = PROD_AREA_ID) %>%
      filter(!is.na(`SLV Area`)) %>%
      arrange(`SLV Area Number`)
    
    datatable(locations, options = list(pageLength = 50,
                                        rowCallback = JS(
                                          "function(row, data) { 
                                            if (data[3] === null) { 
                                              $(row).css('color', 'red'); 
                                            } else { 
                                              $(row).css('color', 'green'); 
                                            } 
                                          }"
                                        )))
  })
  
  # Update dropdown choices dynamically based on toxin_list
  observe({
    req(processed_data()$data)
    processed <- processed_data()$data %>%
      select(where(~ !all(is.na(.))))
    
    toxin_choices <- toxin_list %>%
      filter(Kortnamn_MH %in% names(processed))
    
    taxa_choices <- sort(unique(processed$LATNM))
    
    updateSelectInput(session, "selected_taxa", choices = c("All taxa", taxa_choices), selected = "All taxa")
    updateSelectInput(session, "selected_param", choices = toxin_choices$Kortnamn_MH)
    updateSelectInput(session, "selected_param_map", choices = toxin_choices$Kortnamn_MH)
  })
  
  # Generate time series plot
  output$time_series_plot <- renderPlot({
    req(processed_data()$data, input$selected_param, input$log_scale)
    
    df <- processed_data()$data
    param <- input$selected_param
    log_scale <- input$log_scale
    selected_unit_column <- if (input$sample_type == "live_bivalve_molluscs_v2") "Enhet_MH_kg" else "Enhet_MH_l"
    
    # Identify valid parameters that contain data
    valid_params <- df %>%
      select(-SDATE, -LATNM) %>%  # Exclude non-parameter columns
      summarise(across(everything(), ~ sum(!is.na(.)) > 0)) %>%
      pivot_longer(everything(), names_to = "param", values_to = "has_data") %>%
      filter(has_data) %>%
      pull(param)
    
    # Convert col types
    df <- type_convert(df, col_types = cols())
    
    # Ensure the selected parameter exists in valid_params
    if (!(param %in% valid_params)) {
      showNotification(paste("Selected parameter", param, "has no data or doesn't exist."), type = "error")
      return(NULL)
    }
    
    if (is.logical(df[[param]])) {
      showNotification("Selected parameter is logical (TRUE/FALSE) and cannot be plotted.", type = "error")
      return(NULL)
    }
    
    q_col <- paste0("Q_", param)
    
    # Filter data for selected parameter
    df_param <- df %>%
      select(SDATE, LATNM, all_of(param), all_of(q_col)) %>%
      drop_na(all_of(param)) %>%
      filter(!if_all(all_of(param), is.na)) %>%
      mutate(
        SDATE = as.Date(SDATE),
        LATNM = as.character(LATNM),
        across(all_of(param), as.numeric)
      ) %>%
      filter(!is.infinite(!!sym(param)))
    
    # Get threshold values
    thresholds <- toxin_list %>%
      filter(Kortnamn_MH == param) %>%
      select(Gränsvärde_kommersiell_försäljning)
    
    # Get unit
    unit <- toxin_list %>%
      filter(Kortnamn_MH == param) %>%
      select(!!sym(selected_unit_column)) %>%
      rename(Unit = !!sym(selected_unit_column))
    
    # Get toxin name
    toxin <- toxin_list %>%
      filter(Kortnamn_MH == param) %>%
      select(Parameternamn_MH)
    
    # Create plot only if df_param has rows
    if (nrow(df_param) > 0) {
      p <- ggplot(df_param, aes(x = SDATE, y = !!sym(param))) +
        geom_point(aes(color = if_else(is.na(!!sym(q_col)), "FALSE", 
                                       as.character(str_detect(!!sym(q_col), "<")))), 
                   na.rm = TRUE, size = 3) +
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), 
                           labels = c("FALSE" = "Above LOQ", "TRUE" = "Below LOQ"), 
                           name = "Detection Limit") +
        scale_y_continuous(limits = c(0, NA)) +
        facet_wrap(~ LATNM, ncol = 1) +
        labs(x = "", y = paste0(toxin$Parameternamn_MH, " (", unit$Unit, ")")) +
        theme_minimal() +
        
        # Only add threshold line if it is not NA
        geom_hline(data = thresholds %>% filter(!is.na(Gränsvärde_kommersiell_försäljning)), 
                   aes(yintercept = Gränsvärde_kommersiell_försäljning, 
                       linetype = "Legal limit"), 
                   color = "blue", na.rm = TRUE) +
        
        scale_linetype_manual(name = "Threshold", values = c("Legal limit" = "dashed")) +
        
        # Show threshold legend only if the threshold exists
        guides(color = guide_legend(order = 1), 
               linetype = if (any(!is.na(thresholds$Gränsvärde_kommersiell_försäljning))) 
                 guide_legend(order = 2) else "none") +
        
        theme(
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)
        )
      
      # Apply log scale if selected
      if (log_scale == "log10") {
        p <- p + scale_y_log10()
      }
      
      p
    } else {
      showNotification(paste("No valid data for the selected parameter:", param), type = "warning")
    }
  })
  
  # Generate spatial plot
  output$spatial_plot <- renderPlot({
    req(processed_data()$data, input$selected_param_map, input$log_scale_map)
    
    df <- processed_data()$data
    param <- input$selected_param_map
    log_scale <- input$log_scale_map
    taxa <- input$selected_taxa
    selected_unit_column <- if (input$sample_type == "live_bivalve_molluscs_v2") "Enhet_MH_kg" else "Enhet_MH_l"
    
    # Get unit
    unit <- toxin_list %>%
      filter(Kortnamn_MH == param) %>%
      select(!!sym(selected_unit_column)) %>%
      rename(Unit = !!sym(selected_unit_column))
    
    # Get toxin name
    toxin <- toxin_list %>%
      filter(Kortnamn_MH == param) %>%
      select(Parameternamn_MH)
    
    # Ensure the selected parameter exists and has data
    valid_params <- df %>%
      select(-SDATE, -LATNM, -LONGI, -LATIT) %>%
      summarise(across(everything(), ~ sum(!is.na(.)) > 0)) %>%
      pivot_longer(everything(), names_to = "param", values_to = "has_data") %>%
      filter(has_data) %>%
      pull(param)
    
    # Convert col types
    df <- type_convert(df, col_types = cols())
    
    if (!(param %in% valid_params)) {
      showNotification(paste("Selected parameter", param, "has no data or doesn't exist."), type = "error")
      return(NULL)
    }
    
    if (is.logical(df[[param]])) {
      showNotification("Selected parameter is logical (TRUE/FALSE) and cannot be plotted.", type = "error")
      return(NULL)
    }
    
    # Prepare data for mapping
    if (taxa == "All taxa") {
      df_map <- df %>%
        select(LONGI, LATIT, all_of(param)) %>%
        drop_na(LONGI, LATIT, all_of(param)) %>%
        mutate(across(all_of(param), as.numeric)) %>%
        filter(!is.infinite(!!sym(param)))
    } else {
      df_map <- df %>%
        filter(LATNM == taxa) %>%
        select(LONGI, LATIT, all_of(param)) %>%
        drop_na(LONGI, LATIT, all_of(param)) %>%
        mutate(across(all_of(param), as.numeric)) %>%
        filter(!is.infinite(!!sym(param)))
    }
    
    # Convert to sf object for spatial plotting
    df_map_sf <- st_as_sf(df_map, coords = c("LONGI", "LATIT"), crs = 4326)
    
    # Create map
    if (nrow(df_map) > 0) {
      p_map <- ggplot() +
        geom_sf(data = coastline, fill = "gray80", color = "black") +
        geom_sf(data = df_map_sf, aes(size = !!sym(param), color = !!sym(param)), alpha = 0.7) +
        theme_minimal() +
        labs(
          title = taxa,
          x = "Longitude",
          y = "Latitude",
          color = paste0(toxin$Parameternamn_MH, " (", unit$Unit, ")"),
          size = paste0(toxin$Parameternamn_MH, " (", unit$Unit, ")")
        ) +
        theme(
          plot.title = element_text(size = 18, face = "bold"),  # Increased title size
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
        ) +
        coord_sf(expand = FALSE)
      
      # Apply log scale if selected
      if (log_scale == "log10") {
        p_map <- p_map + 
          scale_size_continuous(transform = "log10", range = c(8, 20)) + 
          scale_color_viridis_c(option = "plasma", transform = "log10")
      } else {
        p_map <- p_map + 
          scale_size_continuous(range = c(8, 20)) + 
          scale_color_viridis_c(option = "plasma")
      }
      
      p_map
    } else {
      showNotification(paste("No valid data for the selected parameter:", param, "and taxa:", taxa), type = "warning")
    }
  })
  
  output$table_origin <- renderDT({
    
    table <- data_summary()$problems %>%
      rename(`Production Area` = Havsområde,
             `Production Area Code` = Nr,
             Species = Art,
             `Origin (Wild/Farmed)` = `V/O`)
    
    datatable(table, options = list(pageLength = 25, language = list(emptyTable = "No issues found"), rowCallback = JS(
      "function(row, data) { 
        $(row).css('color', 'red'); 
      }"
    )))
  })
  
  output$table_missing_origin <- renderDT({
    validate(need(input$file, "Waiting for file upload..."))
    
    table <- issues()$origin_missing %>%
      select(SDATE, PROD_AREA, PROD_AREA_ID, LATNM, SAMPLE_ORIGIN) %>%
      rename(Date = SDATE,
             `Production Area` = PROD_AREA,
             `Production Area Code` = PROD_AREA_ID,
             Species = LATNM,
             `Origin (Wild/Farmed)` = SAMPLE_ORIGIN)
    
    datatable(table, options = list(pageLength = 25, language = list(emptyTable = "No issues found"), rowCallback = JS(
      "function(row, data) { 
        $(row).css('color', 'red'); 
      }"
    )))
  })
  
  output$download <- downloadHandler(
    filename = function() { "data.txt" },
    content = function(file) {
      processed_data <- processed_data()$data
      
      
      
      write.table(processed_data()$data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "", fileEncoding = "Windows-1252")
    }
  )
}

# Run the application
shinyApp(ui, server)
