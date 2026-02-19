# Additional helper functions for column validation
# This file should be sourced after helper.R

# Validate that critical columns exist before processing
validate_critical_columns <- function(df, critical_cols) {
  missing_cols <- setdiff(critical_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing critical columns:", paste(missing_cols, collapse = ", ")))
  }
  invisible(TRUE)
}

# Safe column access with default values
safe_column_access <- function(df, col_name, default_value = NA) {
  if (col_name %in% names(df)) {
    return(df[[col_name]])
  } else {
    warning(paste("Column", col_name, "not found, using default value"))
    return(rep(default_value, nrow(df)))
  }
}

# Validate coordinate format before parsing
validate_coordinate_format <- function(coord_strings) {
  # Check if coordinates match expected DDMM format
  valid_format <- grepl("^[0-9]{6,}$", gsub("[^0-9]", "", coord_strings))
  invalid_coords <- sum(!valid_format, na.rm = TRUE)

  if (invalid_coords > 0) {
    warning(paste(invalid_coords, "coordinates don't match expected DDMM format"))
  }

  return(valid_format)
}

# Safe date creation with error handling
safe_make_date <- function(year, month, day) {
  tryCatch({
    make_date(year = as.numeric(year), month = as.numeric(month), day = as.numeric(day))
  }, error = function(e) {
    warning(paste("Date creation failed:", e$message))
    return(as.Date(NA))
  })
}