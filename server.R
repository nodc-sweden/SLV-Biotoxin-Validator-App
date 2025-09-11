# ============================================
# SERVER
# ============================================
server <- function(input, output, session) {
  
  # ---- File input + basic parsing ----
  uploaded <- reactive({
    req(input$file_eurofins)
    read_with_headers(input$file_eurofins$datapath, skip = 1)
  })
  
  data_summary <- reactive({
    req(input$file_slv_summary)
    # Read file
    df <- read_excel(input$file_slv_summary$datapath, .name_repair = "none", progress = FALSE)
    
    # Extract filename
    filename <- basename(input$file_slv_summary$name)
    
    # Try to extract a 4-digit year (between 1900–2099 for safety)
    year_match <- str_extract(filename, "(19|20)\\d{2}")
    
    # Add year column if a match was found
    if (!"År" %in% colnames(df) && !is.na(year_match)) {
      df <- df %>% mutate("År" = as.integer(year_match))
      showNotification(paste("The column 'År' is missing from data, assuming all rows are from year:", 
                             paste(year_match)), type = "warning", duration = 10)
    }
    
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
    records <- tibble(Art = character(),
                      AphiaID = integer(),
                      scientificname = character())
    
    # Call helper to collect info from WoRMS
    records <- withProgress(message = "Looking up taxa in WoRMS...", value = 0, {
      fetch_worms_for_taxa(taxa_names, name_col = "Art")
    })
    
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
      ungroup() %>%
      mutate(SAMPLE_ORIGIN = recode(SAMPLE_ORIGIN,
                                    "Vilda" = "Wild",
                                    "Odlade" = "Farmed",
                                    .default = SAMPLE_ORIGIN,
                                    .missing = SAMPLE_ORIGIN))
    
    problems <- df %>%
      group_by(Nr, Havsområde, SDATE, Art) %>%
      summarize(n_origins = n_distinct(`V/O`), .groups = "drop") %>%
      filter(n_origins > 1)
    
    problem_rows <- df %>%
      semi_join(problems, by = c("Nr", "Havsområde", "SDATE", "Art")) %>%
      rename(Date = SDATE)
    
    return(list(summary = summary, problems = problem_rows))
  })
  
  # ---- Site information ----
  
  # Store site_df in a reactive object
  site_df_data <- reactive({
    validate(need(input$file_eurofins, "Waiting for file upload..."))
    
    # Just read enough of the uploaded file to extract sites
    df <- uploaded()
    
    # Extract site and number to each entry in the data frame
    site_df <- purrr::map_dfr(df$`Provtagningsplats:`, function(x) {
      val <- extract_site_and_number(x)
      tibble(site = gsub("/", "", trimws(val$site)), number = as.character(val$number))
    })
    
    site_df <- site_df %>% left_join(config_areas, by = c("number" = "Nummer"))
    
    return(site_df)
  })
  
  # ---- Main reactive datasets ----
  
  data <- reactive({
    req(input$file_eurofins, site_df_data())
    df <- uploaded()
    
    # If "midpoint" chosen, enrich df with site info before computing
    if (input$coordinate_output == "midpoint") {
      site_df <- site_df_data()
      
      # Add site info
      df$PROD_AREA <- site_df$Produktionsområde
      df$PROD_AREA_ID <- site_df$number
      df$Mittpunkt_E_SWEREF99 <- site_df$Mittpunkt_E_SWEREF99
      df$Mittpunkt_N_SWEREF99 <- site_df$Mittpunkt_N_SWEREF99
    }
    
    # Use the unified helper for either GPS or midpoint logic
    df <- compute_coordinates(
      df,
      coordinate_column = coordinate_column,
      coordinate_output = input$coordinate_output,
      midpoint_cols = c("Mittpunkt_E_SWEREF99", "Mittpunkt_N_SWEREF99")
    )
    
    # Flag if coordinates are on land
    df <- df %>%
      mutate(
        on_land = is_near_land(
          LATIT,
          LONGI,
          shape = coastline,
          distance = -10 # negative for inside land polygon
        )
      )
    
    return(df)
  })
  
  # Create a reactiveValues object to store taxa
  taxa_data <- reactiveValues(taxa = NULL)
  
  # Update `taxa` within reactive functions
  observe({
    df <- data()
    taxa <- df %>% select(Provmärkning) %>% distinct()
    taxa_names <- taxa$Provmärkning
    
    # Use your memoised bulk fetcher instead of looping
    records <- withProgress(message = "Looking up taxa in WoRMS...", value = 0, {
      fetch_worms_for_taxa(taxa_names, name_col = "Provmärkning")
    })
    
    # If *everything* failed, then notify
    if (all(is.na(records$AphiaID))) {
      showNotification(
        "Unable to collect species information from WoRMS, please try again later", 
        type = "warning"
      )
    }
    
    taxa <- taxa %>%
      left_join(records, by = "Provmärkning") %>%
      select(Provmärkning, AphiaID, scientificname)
    
    # Store the `taxa` data in the reactiveValues
    taxa_data$taxa <- taxa
  })
  
  processed_data <- reactive({
    site_df <- site_df_data()
    data <- data()
    taxa <- taxa_data$taxa
    
    data_summary <- if (!is.null(input$file_slv_summary)) data_summary()$summary else NULL
    selected_unit_column <- get_selected_unit_column(input$sample_type)
    rename_map <- build_rename_map(toxin_list, data)
    logical_cols <- get_logical_cols(toxin_list)
    
    # For missing column tracking later
    data_renamed <- data %>%
      rename_with(~ rename_map[.x], .cols = all_of(names(rename_map))) %>%
      select(where(~ !all(is.na(.))))
    
    # Clean values
    data <- clean_logical_values(data, logical_cols, rename_map)
    data <- clean_numeric_values(data, toxin_list, logical_cols)
    
    # Add metadata
    data <- add_site_taxa_info(data, taxa, site_df, config_areas)
    data_mapped <- apply_column_mapping(data, column_mapping)
    
    # Join data_summary if available
    if (!is.null(data_summary)) {
      data_mapped <- data_mapped %>%
        left_join(data_summary, by = c("SDATE", "LATNM", "PROD_AREA_ID"), relationship = "many-to-many")
    }
    
    # Apply template headers
    template_headers <- get_template(input$sample_type)$headers
    data_out <- template_headers %>%
      bind_rows(data_mapped %>% select(any_of(names(template_headers)))) %>%
      add_position_metadata(input$coordinate_output)
    
    # Missing column diagnostics
    missing_columns <- identify_missing_columns(
      data_renamed, data_out, rename_map,
      unused_columns, column_mapping,
      coordinate_column, site_column, taxa_column,
      toxin_list, selected_unit_column
    )
    
    list(data = data_out, renamed_columns = missing_columns)
  })
  
  # ---- Diagnostics and validation ----
  
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
      filter(is.na(AphiaID)) %>%
      nrow()
    
    # Origin issues
    if ("SAMPLE_ORIGIN" %in% names(processed_df)) {
      origin_missing <- processed_df %>%
        filter(is.na(SAMPLE_ORIGIN))
    } else {
      # Column missing -> treat all rows as issues
      origin_missing <- processed_df
    }
    
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
  
  # ---- Observers for UI updates ----
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
  
  # ---- Outputs ----
  # Tables
  output$table_issue_summary <- renderDT({
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
  
  output$table_unmapped_columns <- renderDT({
    validate(need(input$file_eurofins, "Waiting for file upload..."))
    datatable(processed_data()$renamed_columns, options = list(
      pageLength = 25,
      language = list(emptyTable = "All columns are mapped")))
  })
  
  output$table_raw <- renderDT({
    validate(need(input$file_eurofins, "Waiting for file upload..."))
    
    datatable(uploaded())
  })
  
  output$table_missing <- renderDT({
    validate(need(input$file_eurofins, "Waiting for file upload..."))
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
  
  output$table_taxa_issues <- renderDT({
    validate(need(input$file_eurofins, ""))
    taxa <- taxa_data$taxa %>%
      filter(is.na(AphiaID)) %>%
      rename(`Scientific Name` = scientificname,
             `Reported Scientific Name` = Provmärkning)
    
    datatable(taxa, options = list(pageLength = 25, language = list(emptyTable = "No issues found"), rowCallback = JS(
      "function(row, data) { 
        $(row).css('color', 'red'); 
      }"
    )))
  })
  
  output$table_taxa_valid <- renderDT({
    validate(need(input$file_eurofins, "Waiting for file upload..."))
    taxa_valid <- taxa_data$taxa %>%
      filter(!is.na(AphiaID)) %>%
      arrange(scientificname) %>%
      rename(`Scientific Name` = scientificname,
             `Reported Scientific Name` = Provmärkning)
    
    datatable(taxa_valid, options = list(pageLength = 25, rowCallback = JS(
      "function(row, data) { 
        $(row).css('color', 'green'); 
      }"
    )))
  })
  
  output$table_site_issues <- renderDT({
    validate(need(input$file_eurofins, ""))
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
    validate(need(input$file_eurofins, "Waiting for file upload..."))
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
  
  output$table_origin_duplicates <- renderDT({
    
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
  
  output$table_origin_missing <- renderDT({
    validate(need(input$file_eurofins, "Waiting for file upload..."))
    
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
  
  # Plots
  
  output$time_series_plot <- renderPlot({
    req(processed_data()$data, input$selected_param, input$log_scale)
    
    df <- processed_data()$data
    param <- input$selected_param
    log_scale <- input$log_scale
    selected_unit_column <- get_selected_unit_column(input$sample_type)
    
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
      select(CLOSE_LEV)
    
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
        geom_hline(data = thresholds %>% filter(!is.na(CLOSE_LEV)), 
                   aes(yintercept = CLOSE_LEV, 
                       linetype = "Legal limit"), 
                   color = "blue", na.rm = TRUE) +
        
        scale_linetype_manual(name = "Threshold", values = c("Legal limit" = "dashed")) +
        
        # Show threshold legend only if the threshold exists
        guides(color = guide_legend(order = 1), 
               linetype = if (any(!is.na(thresholds$CLOSE_LEV))) 
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
  
  output$georaphical_plot <- renderPlot({
    req(processed_data()$data, input$selected_param_map, input$log_scale_map)
    
    df <- processed_data()$data
    param <- input$selected_param_map
    log_scale <- input$log_scale_map
    taxa <- input$selected_taxa
    selected_unit_column <- get_selected_unit_column(input$sample_type)
    
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
  
  # Map
  
  output$map <- renderLeaflet({
    validate(need(input$file_eurofins, "Waiting for file upload..."))
    
    # Remove points with missing position
    df_map <- data() %>%
      filter(!is.na(LATIT) & !is.na(LONGI))
    
    # Find all unique reported production areas
    site_df <- site_df_data() %>%
      distinct()
    
    # Use helper for converting midpoint coordinates to DD
    site_df <- compute_coordinates(
      site_df,
      coordinate_column = "",
      coordinate_output = "midpoint",
      midpoint_cols = c("Mittpunkt_E_SWEREF99", "Mittpunkt_N_SWEREF99")
    ) %>%
      filter(!is.na(LATIT) & !is.na(LONGI))
    
    # Create leaflet map
    leaflet(df_map) %>%
      addTiles() %>%
      
      # Add production areas as mipoint and uncertainty
      addCircles(
        data = site_df,
        lng = ~LONGI,
        lat = ~LATIT,
        radius = ~COORDINATE_UNCERTAINTY_M,   # meters
        color = "orange",
        fillColor = "orange",
        fillOpacity = 0.2,
        weight = 1,
        popup = ~paste(
          "Produktionsområde:", `Produktionsområde`, number, 
          "<br>Positionsosäkerhet:", COORDINATE_UNCERTAINTY_M, "m"),
        group = "Production areas"
      ) %>%
      
      # Add sampling markers from df_map
      addCircleMarkers(
        data = df_map,
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
        ),
        group = "Samples"
      ) %>%
      
      # add a layer control to toggle
      addLayersControl(
        overlayGroups = c("Production areas", "Samples"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Download
  
  output$download_data <- downloadHandler(
    filename = function() { "data.txt" },
    content = function(file) {
      write.table(processed_data()$data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "", fileEncoding = "Windows-1252")
    }
  )
  
  output$download_analysis_info <- downloadHandler(
    filename = function() { "analysis_info.txt" },
    content = function(file) {
      processed <- processed_data()$data
      
      # Drop NA columns
      processed <- processed[, colSums(!is.na(processed)) > 0]
      
      # Extract date range
      first_date <- as.Date(min(processed$SDATE, na.rm = TRUE))
      last_date <- as.Date(max(processed$SDATE, na.rm = TRUE))
      first_day <- as.Date(paste0(format(first_date, "%Y"), "-01-01"))
      last_day <- as.Date(paste0(format(last_date, "%Y"), "-12-31"))
      
      # Get headers from template
      template_headers <- get_template(input$sample_type, "Analysinfo")$headers
      
      analysis_info <- toxin_list %>%
        filter(Kortnamn_MH %in% names(processed) )%>%
        mutate(across(everything(), as.character)) %>%
        rename(PARAM = Kortnamn_MH)
      
      data_out <- template_headers %>%
        bind_rows(analysis_info) %>%
        select(any_of(names(template_headers))) %>%
        mutate(VALIDFR = as.character(first_day),
               VALIDTO = as.character(last_day))
      
      write.table(data_out, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "", fileEncoding = "Windows-1252")
    }
  )
}
