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

# Load function from iRfcb
source("R/ifcb_is_near_land.R")

# Load shapefile for Swedish Westcoast
coastline <- st_read("data/shapefiles/EEA_Coastline_Sweden_WestCoast.shp", quiet = TRUE)

# Read list of toxins
toxin_list <- read_excel("config/lista_toxiner.xlsx", progress = FALSE) %>%
  select(`Rapporterat-parameternamn`, Parameter, Enhet, `Gränsvärde_kommersiell_försäljning`) %>%
  drop_na(Parameter)

# Function to convert DDMM coordinates to decimal degrees
convert_ddmm_to_dd <- function(coord) {
  coord <- as.character(coord)  # Ensure input is character
  coord <- gsub("[^0-9]", "", coord)  # Remove non-numeric characters
  
  # Handle cases where input is too short
  coord[nchar(coord) < 6] <- NA
  
  # Extract components safely
  deg <- suppressWarnings(as.numeric(substr(coord, 1, 2)))
  min <- suppressWarnings(as.numeric(substr(coord, 3, 4)))
  min_decimals <- suppressWarnings(as.numeric(substr(coord, 5, 6)))
  
  # Handle cases where conversion fails
  valid <- !(is.na(deg) | is.na(min) | is.na(min_decimals))
  
  min_with_decimals <- ifelse(valid, min + (min_decimals / 100), NA)
  decimal_degrees <- ifelse(valid, deg + (min_with_decimals / 60), NA)
  
  return(decimal_degrees)
}

# Function to extract site and number from a string
extract_site_and_number <- function(input_string) {
  # Regex to extract the first 3-digit number in the string
  match <- regmatches(input_string, regexpr("\\b(\\d{3})\\b", input_string))
  
  if (length(match) == 0) {
    return(list(site = input_string, number = NA))  # No 3-digit number found
  }
  
  # Extract everything before the first 3-digit number as site name
  site_name <- sub("\\s*\\d{3}.*$", "", input_string) 
  
  number <- as.numeric(match)  # Convert extracted number to numeric
  
  return(list(site = trimws(site_name), number = number))
}

# Define UI for application
ui <- fluidPage(
  titlePanel("SLV Marine Biotoxin Data Validation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = ".xlsx"),
      downloadButton("download", "Download Processed .txt File")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Raw Data", DTOutput("table_raw")),
        tabPanel("Map Validation", leafletOutput("map")),
        tabPanel("Coordinate Validation", DTOutput("table_missing")),
        tabPanel("Taxa Validation", DTOutput("table_taxa")),
        tabPanel("Site Validation", DTOutput("table_sites")),
        tabPanel("Time Series Plot",
                 fluidRow(
                   column(6, selectInput("selected_param", "Select Parameter:", choices = NULL)),
                   column(6, selectInput("log_scale", "Log Scale:", choices = c("No" = "none", "Yes" = "log10")))
                 ),
                 plotOutput("time_series_plot", height = "800px")
        ),
        tabPanel("Geographical Plot",
                 fluidRow(
                   column(6, selectInput("selected_param_map", "Select Parameter:", choices = NULL)),
                   column(6, selectInput("log_scale_map", "Log Scale:", choices = c("No" = "none", "Yes" = "log10")))
                 ),
                 plotOutput("spatial_plot", height = "800px")
                 )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath, skip = 1, .name_repair = "none", progress = FALSE)
    header <- names(read_excel(input$file$datapath, .name_repair = "none", progress = FALSE))
    colnames(df)[colnames(df) == ""] <- header[colnames(df) == ""]
    
    df <- df %>%
      mutate(
        LATIT = convert_ddmm_to_dd(substr(`GPS-koord.`, 1, 6)),
        LONGI = convert_ddmm_to_dd(substr(`GPS-koord.`, 8, 13))
      ) %>%
      mutate(on_land = ifcb_is_near_land(
        LATIT, 
        LONGI, 
        shape = "data/shapefiles/EEA_Coastline_Sweden_WestCoast.shp",
        distance = -10)
      )
    return(df)
  })
  
  output$table_raw <- renderDT({
    validate(need(input$file, "Waiting for file upload..."))
    
    withProgress(message = "Loading data...", value = 0.5, {
      Sys.sleep(1)  # Simulate processing time
      datatable(data())
    })
  })
  
  output$map <- renderLeaflet({
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
          "<br>Provtagningsplats:", `Provtagningsplats:`
        )      )
  })
  
  output$table_missing <- renderDT({
    df_missing <- data() %>%
      filter(on_land == TRUE | is.na(LATIT) | is.na(LONGI)) %>%
      select(`Provtagningsplats:`, `Provtagningsdatum:`, `GPS-koord.`, on_land) %>%
      rename(`Reported Site` = `Provtagningsplats:`,
             Date = `Provtagningsdatum:`,
             `GPS-coord` = `GPS-koord.`,
             `On land` = on_land)
    
    datatable(df_missing, options = list(
      pageLength = 25,
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
  
  output$table_taxa <- renderDT({
    taxa <- taxa_data$taxa %>%
      arrange(scientificname) %>%
      rename(`Scientific Name` = scientificname,
             `Reported Scientific Name` = Provmärkning)
    
    datatable(taxa, options = list(pageLength = 25,
                                   rowCallback = JS(
                                     "function(row, data) { 
                                     if (data[2] === null) { 
                                       $(row).css('color', 'red'); 
                                     } else { 
                                       $(row).css('color', 'green'); 
                                     } 
                                   }"
                                   )))
  })
  
  # Store site_df in a reactive object
  site_df_data <- reactive({
    areas <- read_excel("config/production_areas.xlsx", progress = FALSE)
    
    df <- data()
    # Apply the function to each entry in the data frame
    result <- sapply(df$`Provtagningsplats:`, extract_site_and_number, simplify = FALSE)
    
    # Convert the result to a data frame for easier handling
    site_df <- do.call(rbind, lapply(result, function(x) data.frame(site = x$site, number = x$number)))
    
    # Remove potential noise
    site_df$site <- gsub("/", "", site_df$site)
    site_df$site <- trimws(site_df$site)
    
    site_df <- site_df %>% left_join(areas, by = c("number" = "Nummer"))
    
    return(site_df)
  })
  
  output$table_sites <- renderDT({
    site_df <- site_df_data()  # Get the site_df from the reactive object
    
    df <- data()
    
    # Add the final output
    df$`SLV produktionssområde` <- site_df$Produktionssområde
    df$`SLV produktionsområdesrådesnummer` <- site_df$number
    df$`SLV namn` <- site_df$site
    
    locations <- df %>%   
      group_by(`Provtagningsplats:`, `SLV produktionsområdesrådesnummer`, `SLV produktionssområde`) %>%
      summarise(`N visits` = n(), .groups = "drop") %>%
      rename(`Reported Site` = `Provtagningsplats:`,
             `SLV Area` = `SLV produktionssområde`,
             `SLV Area Number` = `SLV produktionsområdesrådesnummer`) %>%
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
  
  processed_data <- reactive({
    site_df <- site_df_data()  # Get the site_df from the reactive object
    
    data <- data()
    
    # Use the stored `taxa` for joining
    taxa <- taxa_data$taxa
    
    # Create a named vector for renaming
    rename_map <- setNames(toxin_list$Parameter, toxin_list$`Rapporterat-parameternamn`)
    
    # Extract logical columns
    logical_cols <- toxin_list %>%
      filter(Enhet == "SANT_eller_FALSKT")
    
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
    for (param in toxin_list$Parameter) {
      # Create the new Q column name
      q_col <- paste0("Q_", param)
      
      # Ensure the column exists in data
      if (param %in% names(data)) {
        # Extract the "<" signs into the new column
        data[[q_col]] <- ifelse(grepl("^<\\s*", data[[param]]), "<", NA)
        
        # Remove the "<" sign (with optional spaces) and convert to numeric
        data[[param]] <- gsub("^<\\s*", "", data[[param]])
        data[[param]][!grepl("^[0-9.]+$", data[[param]])] <- NA
        data[[param]] <- as.numeric(data[[param]])
      }
    }
    
    # Add taxa info
    data <- data %>% left_join(taxa, by = "Provmärkning")
    
    # Add site info
    data$`SLV produktionssområde` <- site_df$Produktionssområde
    data$`SLV produktionsområdesrådesnummer` <- site_df$number
    
    areas <- read_excel("config/production_areas.xlsx", progress = FALSE)
    
    data <- data %>%
      left_join(areas, by = c("SLV produktionsområdesrådesnummer" = "Nummer"))
    
    column_mapping <- c(
      "ORDERER" = "Kund",
      "SDATE" = "Provtagningsdatum:",
      "ANADATE" = "Analys påbörjad den",
      "LATNM" = "scientificname",
      "SMPNO" = "Prov ID"
    )
    
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
    
    template <- read_excel("config/Format_Marine_Biotoxin.xlsx",
                           skip = 2, progress = FALSE)[-1]
    
    template_headers <- template[0,] %>%
      mutate(across(everything(), as.character))
    
    data_out <- template_headers %>%
      bind_rows(data_mapped %>% select(any_of(names(template_headers))))
    
    return(data_out)
  })
  
  # Update dropdown choices dynamically based on toxin_list
  observe({
    req(processed_data())
    updateSelectInput(session, "selected_param", choices = toxin_list$Parameter)
    updateSelectInput(session, "selected_param_map", choices = toxin_list$Parameter)
  })
  
  # Generate time series plot
  output$time_series_plot <- renderPlot({
    req(processed_data(), input$selected_param, input$log_scale)
    
    df <- processed_data()
    param <- input$selected_param
    log_scale <- input$log_scale
    
    # Identify valid parameters that contain data
    valid_params <- df %>%
      select(-SDATE, -LATNM) %>%  # Exclude non-parameter columns
      summarise(across(everything(), ~ sum(!is.na(.)) > 0)) %>%
      pivot_longer(everything(), names_to = "param", values_to = "has_data") %>%
      filter(has_data) %>%
      pull(param)
    
    # Ensure the selected parameter exists in valid_params
    if (!(param %in% valid_params)) {
      showNotification(paste("Selected parameter", param, "has no data or doesn't exist."), type = "error")
      return(NULL)
    }
    
    # Filter data for selected parameter
    df_param <- df %>%
      select(SDATE, LATNM, all_of(param)) %>%
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
      filter(Parameter == param) %>%
      select(Gränsvärde_kommersiell_försäljning)
    
    # Get unit
    unit <- toxin_list %>%
      filter(Parameter == param) %>%
      select(Enhet)
    
    # Create plot only if df_param has rows
    if (nrow(df_param) > 0) {
      p <- ggplot(df_param, aes(x = SDATE, y = !!sym(param))) +
        geom_point(na.rm = TRUE, size = 3) +
        facet_wrap(~ LATNM, ncol = 1) +
        labs(x = "", y = paste0(param, " (", unit$Enhet, ")")) +
        theme_minimal() +
        geom_hline(data = thresholds, aes(yintercept = `Gränsvärde_kommersiell_försäljning`), linetype = "dashed", color = "red", na.rm = TRUE) +
        theme(
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
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
    req(processed_data(), input$selected_param_map, input$log_scale_map)
    
    df <- processed_data()
    param <- input$selected_param_map
    log_scale <- input$log_scale_map
    
    # Ensure the selected parameter exists and has data
    valid_params <- df %>%
      select(-SDATE, -LATNM, -LONGI, -LATIT) %>%
      summarise(across(everything(), ~ sum(!is.na(.)) > 0)) %>%
      pivot_longer(everything(), names_to = "param", values_to = "has_data") %>%
      filter(has_data) %>%
      pull(param)
    
    if (!(param %in% valid_params)) {
      showNotification(paste("Selected parameter", param, "has no data or doesn't exist."), type = "error")
      return(NULL)
    }
    
    # Prepare data for mapping
    df_map <- df %>%
      select(LONGI, LATIT, all_of(param)) %>%
      drop_na(LONGI, LATIT, all_of(param)) %>%
      mutate(across(all_of(param), as.numeric)) %>%
      filter(!is.infinite(!!sym(param)))
    
    # Convert to sf object for spatial plotting
    df_map_sf <- st_as_sf(df_map, coords = c("LONGI", "LATIT"), crs = 4326)
    
    # Create map
    p_map <- ggplot() +
      geom_sf(data = coastline, fill = "gray80", color = "black") +
      geom_sf(data = df_map_sf, aes(size = !!sym(param), color = !!sym(param)), alpha = 0.7) +
      theme_minimal() +
      labs(x = "Longitude", y = "Latitude", color = param, size = param) +
      theme(
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
  })
  
  output$download <- downloadHandler(
    filename = function() { "data.txt" },
    content = function(file) {
      write_tsv(processed_data(), file, na = "", progress = FALSE)
    }
  )
}

# Run the application
shinyApp(ui, server)
