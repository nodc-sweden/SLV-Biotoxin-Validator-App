# ============================================
# SERVER
# ============================================
server <- function(input, output, session) {

  # ---- Reactive flag for conditionalPanel download buttons ----
  output$file_uploaded <- reactive({ !is.null(input$file_eurofins) })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)

  # ---- File input + basic parsing ----
  uploaded <- reactive({
    req(input$file_eurofins)
    df <- read_with_headers(input$file_eurofins$datapath, skip = 1)

    # Validate critical columns exist
    critical_cols <- c("Provtagningsdatum:", "Provtagningsplats:", "Provmärkning")
    missing_critical <- setdiff(critical_cols, names(df))
    if (length(missing_critical) > 0) {
      showNotification(
        paste("Upload Error: Missing critical columns:", paste(missing_critical, collapse = ", ")),
        type = "error", duration = 10
      )
      return(NULL)
    }

    # Safely modify date format if column exists and contains data
    if ("Provtagningsdatum:" %in% names(df) && any(!is.na(df[["Provtagningsdatum:"]]))) {
      df[["Provtagningsdatum:"]] <- ifelse(
        grepl("^[0-9]+$", df[["Provtagningsdatum:"]]),
        as.character(as.Date(as.numeric(df[["Provtagningsdatum:"]]), origin = "1899-12-30")),
        df[["Provtagningsdatum:"]]
      )

      df$`Provtagningsdatum:` <- as.Date(df$`Provtagningsdatum:`)
      df$`Provtagningsdatum:` <- as.character(df$`Provtagningsdatum:`)

      # Warn about implausible dates
      date_check <- validate_date_plausibility(df$`Provtagningsdatum:`)
      if (date_check$n_implausible > 0) {
        showNotification(
          paste(date_check$n_implausible, "date(s) fall outside plausible range (2000 to next year)"),
          type = "warning", duration = 10
        )
      }
    }

    df
  })

  data_summary <- reactive({
    req(input$file_slv_summary)
    # Read file
    df <- read_excel(input$file_slv_summary$datapath, .name_repair = "none", progress = FALSE, col_types = "text")

    # Extract filename
    filename <- basename(input$file_slv_summary$name)

    # Try to extract a 4-digit year (between 1900-2099 for safety)
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
      return(NULL)
    }

    # Process dataframe
    df <- df %>%
      mutate(SDATE = as.character(safe_make_date(År, Mån, Dat))) %>%
      select(SDATE, Havsområde, Nr, Art, `V/O`) %>%
      filter(complete.cases(.))

    taxa <- df %>% select(Art) %>% distinct()
    taxa_names <- taxa$Art

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
      filter(n_distinct(SAMPLE_ORIGIN) == 1) %>%
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

  # ---- Site information (key-based lookup) ----

  site_lookup <- reactive({
    validate(need(input$file_eurofins, "Waiting for file upload..."))
    df <- uploaded()
    req(df)

    site_values <- safe_column_access(df, "Provtagningsplats:", default_value = "")
    build_site_lookup(site_values, config_areas)
  })

  # ---- Main reactive datasets ----

  data <- reactive({
    req(input$file_eurofins, uploaded())
    df <- uploaded()
    req(df)
    lookup <- site_lookup()

    withProgress(message = "Computing coordinates...", value = 0.3, {
      # If "midpoint" chosen, join only the coordinate columns needed for computation
      if (input$coordinate_output == "midpoint") {
        midpoint_lookup <- lookup %>%
          select(`Provtagningsplats:`, Mittpunkt_E_SWEREF99, Mittpunkt_N_SWEREF99) %>%
          distinct()
        df <- df %>%
          left_join(midpoint_lookup, by = "Provtagningsplats:")
      }

      # Use the unified helper for either GPS or midpoint logic
      df <- compute_coordinates(
        df,
        coordinate_column = coordinate_column,
        coordinate_output = input$coordinate_output,
        midpoint_cols = c("Mittpunkt_E_SWEREF99", "Mittpunkt_N_SWEREF99")
      )

      incProgress(0.3, message = "Checking positions against coastline...")

      # Flag if coordinates are on land (using precomputed buffer)
      df <- df %>%
        mutate(
          on_land = is_near_land(LATIT, LONGI, precomputed_buffer = coastline_buffer)
        )
    })

    return(df)
  })

  # Create a reactiveValues object to store taxa
  taxa_data <- reactiveValues(taxa = NULL)

  # Update `taxa` within reactive functions
  observe({
    df <- data()

    # Safely extract taxa, handling missing column
    if ("Provmärkning" %in% names(df)) {
      taxa <- df %>% select(Provmärkning) %>% distinct()
      taxa_names <- taxa$Provmärkning
    } else {
      showNotification("Taxa column 'Provmärkning' not found in data", type = "warning")
      taxa_names <- character(0)
      taxa <- tibble(Provmärkning = character())
    }

    # Use your memoised bulk fetcher instead of looping
    records <- withProgress(message = "Looking up taxa in WoRMS...", value = 0, {
      fetch_worms_for_taxa(taxa_names, name_col = "Provmärkning")
    })

    # If *everything* failed, then notify
    if (nrow(records) > 0 && all(is.na(records$AphiaID))) {
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

  # Type-converted processed data (done once, not in every render)
  processed_data_typed <- reactive({
    req(processed_data()$data)
    readr::type_convert(processed_data()$data, col_types = readr::cols())
  })

  processed_data <- reactive({
    lookup <- site_lookup()
    data <- data()
    req(taxa_data$taxa)
    taxa <- taxa_data$taxa

    data_summary <- if (!is.null(input$file_slv_summary)) data_summary()$summary else NULL
    selected_unit_column <- get_selected_unit_column(input$sample_type)
    rename_map <- build_rename_map(toxin_list, data)
    logical_cols <- get_logical_cols(toxin_list)

    # For missing column tracking later
    data_renamed <- data %>%
      rename_with(~ rename_map[.x], .cols = any_of(names(rename_map))) %>%
      select(where(~ !all(is.na(.))))

    # Clean values
    data <- clean_logical_values(data, logical_cols, rename_map)
    data <- clean_numeric_values(data, toxin_list, logical_cols)

    # Add metadata (key-based join via site_lookup)
    data <- add_site_taxa_info(data, taxa, lookup, config_areas)
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
    req(taxa_data$taxa)
    taxa <- taxa_data$taxa
    lookup <- site_lookup()
    processed_df <- processed_data()$data

    # Date issues - safely check for date column
    date_issues <- if ("Provtagningsdatum:" %in% names(df)) {
      df %>%
        filter(is.na(`Provtagningsdatum:`)) %>%
        nrow()
    } else {
      nrow(df)
    }

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
      origin_missing <- processed_df
    }
    origin_issues <- origin_missing %>% nrow()

    # Site issues (key-based join)
    df_with_sites <- df %>%
      left_join(
        lookup %>% select(`Provtagningsplats:`, Produktionsområde, number),
        by = "Provtagningsplats:"
      )

    site_issues <- df_with_sites %>%
      filter(is.na(number)) %>%
      nrow()

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

  # ---- Outputs: Tables ----

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
          var count = data[2];
          if (count > 0) {
            $(row).css('color', 'red');
          } else {
            $(row).css('color', 'green');
          }
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
        mutate(midpoint = paste(LATIT, LONGI, sep = ", ")) %>%
        select(`Provtagningsplats:`, `Provtagningsdatum:`, `GPS-koord.`, midpoint, on_land) %>%
        rename(`Reported Site` = `Provtagningsplats:`,
               Date = `Provtagningsdatum:`,
               `GPS-coord` = `GPS-koord.`,
               `Midpoint coordinate` = midpoint,
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
    lookup <- site_lookup()
    df <- data()

    # Key-based join
    df_with_sites <- df %>%
      left_join(
        lookup %>% select(`Provtagningsplats:`, Produktionsområde, number, site),
        by = "Provtagningsplats:"
      )

    locations <- df_with_sites %>%
      group_by(`Provtagningsplats:`, number, Produktionsområde) %>%
      summarise(`N visits` = n(), .groups = "drop") %>%
      rename(`Reported Site` = `Provtagningsplats:`,
             `SLV Area` = Produktionsområde,
             `SLV Area Number` = number) %>%
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
    lookup <- site_lookup()
    df <- data()

    # Key-based join
    df_with_sites <- df %>%
      left_join(
        lookup %>% select(`Provtagningsplats:`, Produktionsområde, number, site),
        by = "Provtagningsplats:"
      )

    locations <- df_with_sites %>%
      group_by(`Provtagningsplats:`, number, Produktionsområde) %>%
      summarise(`N visits` = n(), .groups = "drop") %>%
      rename(`Reported Site` = `Provtagningsplats:`,
             `SLV Area` = Produktionsområde,
             `SLV Area Number` = number) %>%
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
    validate(need(input$file_slv_summary, "Waiting for SLV summary upload..."))

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
      select(any_of(c("SDATE", "PROD_AREA", "PROD_AREA_ID", "LATNM", "SAMPLE_ORIGIN"))) %>%
      rename(any_of(c(
        Date = "SDATE",
        `Production Area` = "PROD_AREA",
        `Production Area Code` = "PROD_AREA_ID",
        Species = "LATNM",
        `Origin (Wild/Farmed)` = "SAMPLE_ORIGIN"
      )))

    datatable(table, options = list(pageLength = 25, language = list(emptyTable = "No issues found"), rowCallback = JS(
      "function(row, data) {
        $(row).css('color', 'red');
      }"
    )))
  })

  # ---- Outputs: Plots ----

  output$time_series_plot <- renderPlot({
    req(processed_data()$data, input$selected_param, input$log_scale)

    df <- processed_data_typed()
    param <- input$selected_param
    log_scale <- input$log_scale
    selected_unit_column <- get_selected_unit_column(input$sample_type)

    # Validate parameter
    err <- validate_plot_param(df, param)
    if (!is.null(err)) {
      showNotification(err, type = "error")
      return(NULL)
    }

    q_col <- paste0("Q_", param)

    # Filter data for selected parameter
    df_param <- df %>%
      select(SDATE, LATNM, any_of(param), any_of(q_col)) %>%
      drop_na(any_of(param)) %>%
      filter(!if_all(any_of(param), is.na)) %>%
      mutate(
        SDATE = as.Date(SDATE),
        LATNM = as.character(LATNM),
        across(any_of(param), as.numeric)
      ) %>%
      filter(!is.infinite(!!sym(param)))

    if (nrow(df_param) == 0) {
      showNotification(paste("No valid data for the selected parameter:", param), type = "warning")
      return(NULL)
    }

    toxin_meta <- get_toxin_metadata(toxin_list, param, selected_unit_column)
    build_time_series_plot(df_param, param, q_col, toxin_meta, log_scale)
  })

  output$georaphical_plot <- renderPlot({
    req(processed_data()$data, input$selected_param_map, input$log_scale_map)

    df <- processed_data_typed()
    param <- input$selected_param_map
    log_scale <- input$log_scale_map
    taxa <- input$selected_taxa
    selected_unit_column <- get_selected_unit_column(input$sample_type)

    # Validate parameter
    err <- validate_plot_param(df, param, exclude_cols = c("SDATE", "LATNM", "LONGI", "LATIT"))
    if (!is.null(err)) {
      showNotification(err, type = "error")
      return(NULL)
    }

    # Prepare data for mapping
    if (taxa == "All taxa") {
      df_map <- df %>%
        select(LONGI, LATIT, any_of(param)) %>%
        drop_na(LONGI, LATIT, any_of(param)) %>%
        mutate(across(any_of(param), as.numeric)) %>%
        filter(!is.infinite(!!sym(param)))
    } else {
      df_map <- df %>%
        filter(LATNM == taxa) %>%
        select(LONGI, LATIT, any_of(param)) %>%
        drop_na(LONGI, LATIT, any_of(param)) %>%
        mutate(across(any_of(param), as.numeric)) %>%
        filter(!is.infinite(!!sym(param)))
    }

    if (nrow(df_map) == 0) {
      showNotification(paste("No valid data for the selected parameter:", param, "and taxa:", taxa), type = "warning")
      return(NULL)
    }

    df_map_sf <- st_as_sf(df_map, coords = c("LONGI", "LATIT"), crs = 4326)
    toxin_meta <- get_toxin_metadata(toxin_list, param, selected_unit_column)
    build_geographical_plot(df_map_sf, coastline, param, toxin_meta, taxa, log_scale)
  })

  # ---- Outputs: Map ----

  output$map <- renderLeaflet({
    validate(need(input$file_eurofins, "Waiting for file upload..."))

    # Remove points with missing position
    df_map <- data() %>%
      filter(!is.na(LATIT) & !is.na(LONGI))

    # Find all unique reported production areas
    lookup <- site_lookup() %>%
      distinct()

    # Use helper for converting midpoint coordinates to DD
    lookup_with_coords <- compute_coordinates(
      lookup,
      coordinate_column = "",
      coordinate_output = "midpoint",
      midpoint_cols = c("Mittpunkt_E_SWEREF99", "Mittpunkt_N_SWEREF99")
    ) %>%
      filter(!is.na(LATIT) & !is.na(LONGI))

    # Create leaflet map
    leaflet(df_map) %>%
      addTiles() %>%

      # Add production areas as midpoint and uncertainty
      addCircles(
        data = lookup_with_coords,
        lng = ~LONGI,
        lat = ~LATIT,
        radius = ~COORDINATE_UNCERTAINTY_M,
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

  # ---- Outputs: Downloads ----

  output$download_data <- downloadHandler(
    filename = function() { "data.txt" },
    content = function(file) {
      write.table(processed_data()$data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "", fileEncoding = "Windows-1252")
    }
  )

  output$download_analysis_info <- downloadHandler(
    filename = function() { "analyse_info.txt" },
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
        filter(Kortnamn_MH %in% names(processed)) %>%
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
