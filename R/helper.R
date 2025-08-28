# Determine if Positions are Near Land
is_near_land <- function(latitudes,
                         longitudes,
                         distance = 500,
                         shape = NULL,
                         crs = 4326,
                         utm_zone = 33,
                         remove_small_islands = TRUE,
                         small_island_threshold = 2000000) {
  
  # Check for NAs in latitudes and longitudes
  na_positions <- is.na(latitudes) | is.na(longitudes)
  
  # Create a result vector initialized to NA
  result <- rep(NA, length(latitudes))
  
  # If all positions are NA, return the result early
  if (all(na_positions)) {
    return(result)
  }
  
  # Filter out NA positions for further processing
  latitudes_filtered <- latitudes[!na_positions]
  longitudes_filtered <- longitudes[!na_positions]
  
  utm_epsg <- paste0("epsg:", 32600 + utm_zone)
  
  # Create a bounding box around the coordinates with a buffer
  bbox <- st_bbox(c(xmin = min(longitudes_filtered) - 1, xmax = max(longitudes_filtered) + 1,
                    ymin = min(latitudes_filtered) - 1, ymax = max(latitudes_filtered) + 1),
                  crs = st_crs(crs))
  
  # Get coastline
  if (is.null(shape)) {
    # Directory to extract files
    exdir <- tempdir()  # Temporary directory
    
    # Extract the files
    unzip(system.file("exdata/ne_50m_land.zip", package = "iRfcb"), exdir = exdir)
    
    # Get coastline and land data within the bounding box
    land <- st_read(file.path(exdir, "ne_50m_land.shp"), quiet = TRUE)
  } else {
    land <- shape
    land <- st_transform(land, crs = crs)
  }
  
  # Check geometry type
  geom_type <- unique(st_geometry_type(land))
  
  # Optionally remove small islands based on area threshold
  if (!is.null(shape) && remove_small_islands && any(st_geometry_type(land) %in% c("POLYGON", "MULTIPOLYGON"))) {
    land$area <- st_area(land)
    
    small_islands <- which(as.numeric(land$area) < small_island_threshold)
    land <- land[-small_islands, ]
    
    # Remove the 'area' attribute
    land$area <- NULL
  }
  
  # Filter land data to include only the region within the bounding box
  land <- suppressWarnings(st_intersection(land, st_as_sfc(bbox)))
  
  # Cleanup and transform land data
  land <- land %>% st_union() %>% st_make_valid() %>% st_wrap_dateline()
  land_utm <- st_transform(land, crs = utm_epsg)
  
  # Create a buffered shape around the coastline in meters (specified distance)
  l_buffer <- terra::vect(land_utm)
  terra::crs(l_buffer) <- utm_epsg
  l_buffer <- terra::buffer(l_buffer, width = distance) %>% st_as_sf()
  
  # Apply st_wrap_dateline only if the CRS is geographic
  if (st_crs(l_buffer)$epsg == crs) {
    l_buffer <- l_buffer %>% st_wrap_dateline()
  }
  
  # Transform the buffered coastline and land data back to the original CRS
  l_buffer <- st_transform(l_buffer, crs = crs)
  
  # Create sf object for positions
  positions_sf <- st_as_sf(data.frame(lon = longitudes_filtered, lat = latitudes_filtered),
                           coords = c("lon", "lat"), crs = st_crs(crs))
  
  # Check which positions intersect with the buffer and land
  near_land <- st_intersects(positions_sf, l_buffer)
  
  # Extract logical vectors indicating whether each position is near land or on land
  near_land_logical <- lengths(near_land) > 0
  
  # Assign results back to the appropriate positions in the result vector
  result[!na_positions] <- near_land_logical
  
  # Return the logical vector indicating near land with NAs for original NA positions
  return(result)
}

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

# Function to read Excel files with headers in the first row
read_with_headers <- function(path, skip = 1) {
  df <- read_excel(path, skip = skip, .name_repair = "none", progress = FALSE)
  header <- names(read_excel(path, .name_repair = "none", progress = FALSE))
  colnames(df)[colnames(df) == ""] <- header[colnames(df) == ""]
  df
}

# # 2) Build site_df from uploaded df in a robust, vectorized way (use map_dfr)
# build_site_df <- function(df, areas, site_col = "Provtagningsplats:") {
#   # extract_site_and_number must return list(site=..., number=...)
#   res <- purrr::map_dfr(df[[site_col]], function(x) {
#     val <- extract_site_and_number(x)
#     data.frame(site = val$site, number = as.character(val$number), stringsAsFactors = FALSE)
#   })
#   res$site <- trimws(gsub("/", "", res$site))
#   res %>% left_join(areas, by = c("number" = "Nummer"))
# }

# Memoised WoRMS calls (reduces repeated API calls)
# Wrap wm_records_name in a safe memoised function
safe_wm_records_name <- memoise::memoise(function(name, fuzzy = FALSE) {
  tryCatch(wm_records_name(name, fuzzy = fuzzy), error = function(e) NULL)
})

# Fetch taxa info for vector of names
fetch_worms_for_taxa <- function(taxa_names, name_col = "Art", target_col_name = "scientificname") {
  if (length(taxa_names) == 0) return(tibble(!!name_col := character(), AphiaID = integer(), scientificname = character()))
  res <- purrr::map_dfr(taxa_names, function(nm) {
    rec <- safe_wm_records_name(nm, fuzzy = FALSE)
    if (is.null(rec) || nrow(rec) == 0) {
      tibble(!!name_col := nm, AphiaID = NA_integer_, scientificname = nm)
    } else {
      # wm_records_name often returns a data.frame; take first row (or adapt as needed)
      tibble(!!name_col := nm, AphiaID = rec$AphiaID[1], scientificname = rec$scientificname[1])
    }
  })
  res
}

# Compute lat/lon robustly (handles midpoints or raw coords)
compute_coordinates <- function(df, coordinate_column, coordinate_output = c("actual", "midpoint"), midpoint_cols = c("Mittpunkt_E_SWEREF99","Mittpunkt_N_SWEREF99")) {
  coordinate_output <- match.arg(coordinate_output)
  
  # Always preallocate columns so they exist
  df$LONGI <- NA_real_
  df$LATIT <- NA_real_
  
  # First try to parse raw DDMM strings if present
  if (coordinate_output == "actual" && coordinate_column %in% names(df)) {
    # Avoid hardcoding substr indices: parse with regex or safe string ops if format is consistent
    df <- df %>% mutate(
      LATIT = convert_ddmm_to_dd(substr(.[[coordinate_column]], 1, 6)),
      LONGI = convert_ddmm_to_dd(substr(.[[coordinate_column]], 8, 13))
    )
  } else if (coordinate_output == "midpoint" && all(midpoint_cols %in% names(df))) {
    # transform only rows with valid midpoint coordinates
    idx <- !is.na(df[[midpoint_cols[1]]]) & !is.na(df[[midpoint_cols[2]]])
    if (any(idx)) {
      pts <- st_as_sf(df[idx, ], coords = midpoint_cols, crs = 3006) %>% st_transform(4326)
      xy <- st_coordinates(pts)
      df$LONGI[idx] <- xy[,1]
      df$LATIT[idx]  <- xy[,2]
    }
  }
  df$LONGI <- round(df$LONGI, 4)
  df$LATIT <- round(df$LATIT, 4)
  df
}
