# ============================================
# UI
# ============================================
ui <- fluidPage(
  titlePanel("SLV Marine Biotoxin Data Validation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Eurofins Excel file", accept = ".xlsx"),
      fileInput("file_summary", "Upload summary Excel file (with info about wild/farmed)", accept = ".xlsx"),
      selectInput("coordinate_output", "Use position:", 
                  choices = c("Reported GPS position" = "actual", "Midpoint production area" = "midpoint"), 
                  selected = "midpoint"),
      selectInput("sample_type", "Sample type:", 
                  choices = c("Animal flesh" = "live_bivalve_molluscs_v2", "Water" = "watersample"), 
                  selected = "live_bivalve_molluscs_v2"),
      downloadButton("download", "Download Processed .txt File"),
      br(), br(),
      downloadButton("download_analysis", "Download Analysis Info .txt File"),
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
