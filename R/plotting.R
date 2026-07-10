# Shared plotting helpers — extracted from server.R render functions

# Validate that a selected parameter is plottable in the given dataframe
# Returns NULL if valid, or an error message string if not
validate_plot_param <- function(df, param, exclude_cols = c("SDATE", "LATNM")) {
  valid_params <- df %>%
    select(-any_of(exclude_cols)) %>%
    summarise(across(everything(), ~ sum(!is.na(.)) > 0)) %>%
    pivot_longer(everything(), names_to = "param", values_to = "has_data") %>%
    filter(has_data) %>%
    pull(param)

  if (!(param %in% valid_params)) {
    return(paste("Selected parameter", param, "has no data or doesn't exist."))
  }
  if (!(param %in% names(df))) {
    return(paste("Selected parameter", param, "is not found in the data."))
  }
  if (is.logical(df[[param]])) {
    return("Selected parameter is logical (TRUE/FALSE) and cannot be plotted.")
  }
  NULL
}

# Look up toxin metadata (unit, name, threshold) from the toxin list
get_toxin_metadata <- function(toxin_list, param, unit_column) {
  row <- toxin_list %>% filter(Kortnamn_MH == param)
  list(
    thresholds = row %>% select(CLOSE_LEV),
    unit = row %>% select(!!sym(unit_column)) %>% rename(Unit = !!sym(unit_column)),
    name = row %>% select(Parameternamn_MH)
  )
}

# Build the time series ggplot object
build_time_series_plot <- function(df_param, param, q_col, toxin_meta, log_scale) {
  thresholds <- toxin_meta$thresholds
  unit <- toxin_meta$unit
  toxin <- toxin_meta$name

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
    geom_hline(data = thresholds %>% filter(!is.na(CLOSE_LEV)),
               aes(yintercept = CLOSE_LEV,
                   linetype = "Legal limit"),
               color = "blue", na.rm = TRUE) +
    scale_linetype_manual(name = "Threshold", values = c("Legal limit" = "dashed")) +
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

  if (log_scale == "log10") {
    p <- p + scale_y_log10()
  }

  p
}

# Build the geographical ggplot object
build_geographical_plot <- function(df_map_sf, coastline, param, toxin_meta, taxa_label, log_scale) {
  unit <- toxin_meta$unit
  toxin <- toxin_meta$name

  p_map <- ggplot() +
    geom_sf(data = coastline, fill = "gray80", color = "black") +
    geom_sf(data = df_map_sf, aes(size = !!sym(param), color = !!sym(param)), alpha = 0.7) +
    theme_minimal() +
    labs(
      title = taxa_label,
      x = "Longitude",
      y = "Latitude",
      color = paste0(toxin$Parameternamn_MH, " (", unit$Unit, ")"),
      size = paste0(toxin$Parameternamn_MH, " (", unit$Unit, ")")
    ) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    ) +
    coord_sf(expand = FALSE)

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
}
