# ui component 

#--------------------------------------#
####    Build R Shiny Dashboard     ####
#--------------------------------------# 

#### functions ####

create_temp_records_panel <- function(plots_config) {
  # plots_config should be a list of lists, each containing configuration for one plot
  lapply(plots_config, function(config) {
    # Get the data source for the year range
    data_source <- config$data_source
    
    fluidRow(
      # Column for the checkbox group input
      column(
        width = 4,
        box(
          title = "Data Tools",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          # Add checkbox group for line selection
          checkboxGroupInput(
            inputId = config$checkbox_id,
            label = "Select Data to Display:",
            choices = config$checkbox_choices,
            selected = config$default_selected
          ),
          sliderInput(
            inputId = config$year_range_id,
            label = "Select Year Range:",
            min = min(data_source$year),
            max = max(data_source$year),
            value = c(min(data_source$year), max(data_source$year)),
            sep = ""
          )
        )
      ),
      # Plot output
      column(
        width = 8,
        box(
          title = config$plot_title,
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput(config$plot_id, height = "600px")
        )
      )
    )
  })
}

#### User interface (ui)

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Acadia Climate Dashboard",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    
    # Add sidebar menu with tabs
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("home")),
      menuItem("Temperature Trends", tabName = "temp", icon = icon("thermometer-half")),
      menuItem("Precipitation Trends", tabName = "precip", icon = icon("cloud-rain")),
      menuItem("Sea Level Trends", tabName = "sea", icon = icon("water"))
    )
  ),
  
  
  dashboardBody(
    tags$head(tags$link(type = "text/css", rel = "stylesheet", href = "css/style.css")),
    
    # Define tab items
    tabItems(
      # Tab for dashboard overview (add content here later)
      tabItem(tabName = "overview",
              fluidRow(
                # Left column with text
                column(width = 6,
                       box(
                         title = "Dashboard Overview",
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,
                         tags$img(
                           src = "img/COPY_SchoodicInstitute_Horizontal_CMYK.png",
                           width = "100%",
                           style = "margin-bottom: 15px;",
                           alt = "Schoodic Institute logo" 
                         ),
                         p("This R Shiny Dashboard summarizes climate data from the Acadia National Park region gathered from local weather stations and the National Oceanic and Atmospheric Administration (NOAA). Local data was gathered from the McFarland Hill Atmospheric Research Station and the Winter Harbor-SERC weather station. Climate data was compiled and cleaned to produce visualizations of temperature and precipitation long-term trends, anomalies, and extremes."),
                         tags$ul(
                           tags$li(HTML('Climate summaries were created from daily and monthly gridded climate data downloaded from NOAA\'s National Centers for Environmental Information (<a href="https://www.ncei.noaa.gov" target="_blank">NCEI </a>). Climate data was compiled and cleaned using R scripts by Kyle Lima, built from the climateNETN package by Kate Miller (<a href="https://github.com/KateMMiller/climateNETN" target="_blank">climateNETN </a>).')),
                           tags$li("Climate summaries were created from hourly data collected by the McFarland Hill Atmospheric Research Station."),
                           tags$li("Climate summaries were created from 15 minute interval data collected by the Winter Harbor-SERC weather station (ID: D2258).")),
                         p(HTML("<strong>Data Access</strong>: data from all sources used in this app and R scripts for data cleaning can be downloaded and found at this page: link."))
                         )
                      ),
                # Right column with map
                column(width = 6,
                       box(
                         title = "Weather Station Locations",
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,
                         height = "600px",
                         leafletOutput("LocationMap", height = "550px")
                       )
                )
              )
      ),
      
      
      # Tab for interactive temperature plot
      tabItem(tabName = "temp",
              
              tabsetPanel(
                tabPanel(
                  "Temperature Trends",
                  
                  # Add fluidRow for the checkbox group and plot
                  fluidRow(
                    # Column for the checkbox group input
                    column(
                      width = 4, 
                      box(
                        title = "Data Tools",
                        status = "primary", 
                        solidHeader = TRUE, 
                        width = 12,
                        # Add checkbox group for line selection
                        checkboxGroupInput(
                          inputId = "linesToShow",
                          label = "Select Temperature Data to Display:",
                          choices = c("NOAA Average Maximum Temp." = "NOAA Average Max Temp",
                                      "NOAA Average Temp." = "NOAA Average Temp",
                                      "NOAA Average Minimum Temp." = "NOAA Average Min Temp",
                                      "McFarland Average Temp." = "McFarland Average Temp",
                                      "SERC Average Temp." = "SERC Average Temp",
                                      "Linear Model for NOAA Average Max Temp" = "lm_noaa_max_temp",
                                      "Linear Model for NOAA Average Temp" = "lm_noaa_temp",
                                      "Linear Model for NOAA Average Min Temp" = "lm_noaa_min_temp",
                                      "Linear Model for McFarland Average Temp" = "lm_mcfarland_temp",
                                      "Linear Model for SERC Average Temp" = "lm_serc_temp"
                          ),
                          selected = c("NOAA Average Temp", "NOAA Average Max Temp", "NOAA Average Min Temp", "McFarland Average Temp", "SERC Average Temp")
                        ),
                      
                      # Add slider for year range
                          sliderInput(
                            inputId = "year_range_temp",
                            label = "Select Year Range:",
                            min = min(temperature.data.merged$year),
                            max = max(temperature.data.merged$year),
                            value = c(min(temperature.data.merged$year), max(temperature.data.merged$year)),
                            sep = ""
                        )
                      )
                    ),
                    
                    # Column for the plot
                    column(
                      width = 8,
                      box(
                        title = "Long-Term Temperature Trends", 
                        status = "primary", 
                        solidHeader = TRUE, 
                        width = 12,
                        plotlyOutput("myInteractivePlot", height = "600px")
                      )
                    )
                  ),
                  
                  # fluidRow for model statistics
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "Model Statistics",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        
                        # NOAA Average Temperature model stats
                        conditionalPanel(
                          condition = "input.linesToShow.includes('lm_noaa_temp')",
                          h4("NOAA Average Temperature Model"),
                          verbatimTextOutput("noaa_temp_model_summary")
                        ),
                        
                        # NOAA Maximum Temperature model stats
                        conditionalPanel(
                          condition = "input.linesToShow.includes('lm_noaa_max_temp')",
                          h4("NOAA Maximum Temperature Model"),
                          verbatimTextOutput("noaa_max_temp_model_summary")
                        ),
                        
                        # NOAA Minimum Temperature model stats
                        conditionalPanel(
                          condition = "input.linesToShow.includes('lm_noaa_min_temp')",
                          h4("NOAA Minimum Temperature Model"),
                          verbatimTextOutput("noaa_min_temp_model_summary")
                        ),
                        
                        # McFarland Temperature model stats
                        conditionalPanel(
                          condition = "input.linesToShow.includes('lm_mcfarland_temp')",
                          h4("McFarland Temperature Model"),
                          verbatimTextOutput("mcfarland_temp_model_summary")
                        ),
                        # McFarland Temperature model stats
                        conditionalPanel(
                          condition = "input.linesToShow.includes('lm_serc_temp')",
                          h4("SERC Temperature Model"),
                          verbatimTextOutput("serc_temp_model_summary")
                        )
                      )
                    )
                  )
                ),
                  
                
                tabPanel(
                  "Temperature Anomalies", 
                  
                  # Add fluidRow for the checkbox group and plot
                  fluidRow(
                    # Column for the checkbox group input
                    column(
                      width = 4,
                      box(
                        title = "Data Tools",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        # # Add checkbox group for line selection
                        # checkboxGroupInput(
                        #   inputId = "linesToShowAnom",
                        #   label = "Select Temperature Anomaly Data to Display:",
                        #   choices = c("NOAA Temp Anomaly" = "NOAA Temp Anom",
                        #               "McFarland Temp Anomaly" = "McFarland Temp Anom"
                        #   ),
                        #   selected = c("NOAA Temp Anom", "McFarland Temp Anom")
                        # ),

                        # Add slider for year range
                        sliderInput(
                          inputId = "year_range_temp_anom",
                          label = "Select Year Range:",
                          min = min(shiny.merged.anom$year),
                          max = max(shiny.merged.anom$year),
                          value = c(min(shiny.merged.anom$year), max(shiny.merged.anom$year)),
                          sep = ""
                        )
                      )
                    )
                  ),
                  
                  #NOAA anom plot
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "NOAA Temperature Anomalies",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("NOAAAnomPlot", height = "600px")
                      )
                    )
                  ),
                  
                  #McFarland anom plot 
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "McFarland Temperature Anomalies",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("McFarlandAnomPlot", height = "600px")
                      )
                    )
                  )
                  
                ),
                
                # tab for temp records plots
                tabPanel(
                  "Temperature Records and Extremes",
                  
                  create_temp_records_panel(
                    list(
                      # First plot (monthly maximum temperatures)
                      list(
                        plot_title = "Monthly Maximum NOAA Temperature Records",
                        year_range_id = "year_range_records",
                        checkbox_id = "temp_records_display",
                        plot_id = "MaxTempRecordsPlot",
                        data_source = shiny.monthly.records,  # Original data source
                        checkbox_choices = c(
                          "Monthly Maximum Mean Temperature Records" = "mean_max_temp",
                          "Monthly Maximum Temperature Records" = "max_temp"
                        ),
                        default_selected = c("mean_max_temp")
                      ),
                      
                      # Second plot (monthly minimum temperatures)
                      list(
                        plot_title = "Monthly Minimum NOAA Temperature Records",
                        year_range_id = "year_range_records2",
                        checkbox_id = "min_temp_records_display",
                        plot_id = "MinTempRecordsPlot",
                        data_source = shiny.monthly.records,  # Original data source
                        checkbox_choices = c(
                          "Monthly Minimum Mean Temperature Records" = "mean_min_temp",
                          "Monthly Minimum Temperature Records" = "min_temp"
                        ),
                        default_selected = c("mean_min_temp")
                      ),
                      
                      # Third plot (daily maximum temperatures)
                      list(
                        plot_title = "Daily Maximum NOAA Temperature Records",
                        year_range_id = "year_range_records3",
                        checkbox_id = "daily_max_temp_display",
                        plot_id = "DailyMaxRecordsPlot",
                        data_source = shiny.daily.temp.records,  
                        checkbox_choices = c(
                          "Daily Maximum Mean Temperature Records" = "daily_mean_max_temp",
                          "Daily Maximum Temperature Records" = "daily_max_temp"
                        ),
                        default_selected = c("daily_mean_max_temp")
                      ),
                      
                      # Fourth plot (daily maximum temperatures)
                      list(
                        plot_title = "Daily Minimum NOAA Temperature Records",
                        year_range_id = "year_range_records4",
                        checkbox_id = "daily_min_temp_display",
                        plot_id = "DailyMinRecordsPlot",
                        data_source = shiny.daily.temp.records,  
                        checkbox_choices = c(
                          "Daily Minimum Mean Temperature Records" = "daily_mean_min_temp",
                          "Daily Minimum Temperature Records" = "daily_min_temp"
                        ),
                        default_selected = c("daily_mean_min_temp")
                      )
                    )
                  )
                )
              )
            ),
                 
      
      # Tab for interactive precipitation plots
      tabItem(tabName = "precip",
              
              tabsetPanel(
                tabPanel(
                  "Precipitation Trends",
                  
                  fluidRow(
                    column(
                      width = 4,
                      box(
                        title = "Data Tools",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        checkboxGroupInput(
                          inputId = "linesToShowPrecip",
                          label = "Select Precipitation Data to Display:",
                          choices = c("NOAA Total Precip." = "NOAA Precip",
                                      "McFarland Total Precip." = "McFarland Precip",
                                      "SERC Total Precip." = "SERC Precip",
                                      "Linear Model for NOAA Precip" = "lm_noaa_precip",
                                      "Linear Model for McFarland Precip" = "lm_mcfarland_precip",
                                      "Linear Model for SERC Precip" = "lm_serc_precip"),
                          selected = c("NOAA Precip", "McFarland Precip", "SERC Precip")
                        ),
                      
                      # Add slider for year range
                      sliderInput(
                        inputId = "year_range_precip",
                        label = "Select Year Range:",
                        min = min(precipitation.data.merged$year),
                        max = max(precipitation.data.merged$year),
                        value = c(min(precipitation.data.merged$year), max(precipitation.data.merged$year)),
                        sep = ""
                        )
                      )
                    ),
                    
                    column(
                      width = 8,
                      box(
                        title = "Long-Term Precipitation Trends",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("PrecipPlot", height = "600px")
                      )
                    )
                ),
                  
                  # fluidRow for model statistics
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "Model Statistics",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        
                        # NOAA precipitation model stats
                        conditionalPanel(
                          condition = "input.linesToShowPrecip.includes('lm_noaa_precip')",
                          h4("NOAA Precipitation Model"),
                          verbatimTextOutput("noaa_precip_model_summary")
                        ),
                        
                        # McFarland precipitation model stats
                        conditionalPanel(
                          condition = "input.linesToShowPrecip.includes('lm_mcfarland_precip')",
                          h4("McFarland Precipitation Model"),
                          verbatimTextOutput("mcfarland_precip_model_summary")
                        ),
                        
                        # SERC precipitation model stats
                        conditionalPanel(
                          condition = "input.linesToShowPrecip.includes('lm_serc_precip')",
                          h4("SERC Precipitation Model"),
                          verbatimTextOutput("serc_precip_model_summary")
                        )
                      )
                    )
                  )
                ),

                
                tabPanel(
                  "Precipitation Anomalies", 
                  
                  # Add fluidRow for the checkbox group and plot
                  fluidRow(
                    # Column for the checkbox group input
                    column(
                      width = 4,
                      box(
                        title = "Data Tools",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        # # Add checkbox group for line selection
                        # checkboxGroupInput(
                        #   inputId = "linesToShowAnom",
                        #   label = "Select Temperature Anomaly Data to Display:",
                        #   choices = c("NOAA Temp Anomaly" = "NOAA Temp Anom",
                        #               "McFarland Temp Anomaly" = "McFarland Temp Anom"
                        #   ),
                        #   selected = c("NOAA Temp Anom", "McFarland Temp Anom")
                        # ),
                        
                        # Add slider for year range
                        sliderInput(
                          inputId = "year_range_precip_anom",
                          label = "Select Year Range:",
                          min = min(shiny.merged.precip.anom$year),
                          max = max(shiny.merged.precip.anom$year),
                          value = c(min(shiny.merged.precip.anom$year), max(shiny.merged.precip.anom$year)),
                          sep = ""
                        )
                      )
                    )
                  ),
                  
                  #NOAA anom plot
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "NOAA Precipitation Anomalies",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("NOAAPrecipAnomPlot", height = "600px")
                      )
                    )
                  ),
                  
                  #McFarland anom plot 
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "McFarland Precipitation Anomalies",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("McFarlandPrecipAnomPlot", height = "600px")
                      )
                    )
                  )
                  
                ),
                
                tabPanel(
                  "Precipitation Records and Extremes",
                  
                  # Add fluidRow for the checkbox group and plot
                  fluidRow(
                    # Column for the checkbox group input
                    column(
                      width = 4,
                      box(
                        title = "Data Tools",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        # # Add checkbox group for line selection
                        # checkboxGroupInput(
                        #   "temp_records_display",
                        #   "Select NOAA Temperature Records Data to Display:",
                        #   choices = c("Maximum Mean Temperature Records" = "mean_max_temp",
                        #               "Maximum Temperature Records" = "max_temp",
                        #               "Minimum Mean Temperature Records" = "mean_min_temp",
                        #               "Minimum Temperature Records" = "min_temp"
                        #   ),
                        #   selected = c("mean_max_temp", "max_temp", "mean_min_temp", "min_temp")
                        # ),

                      sliderInput(
                        inputId = "year_range_precip",
                        label = "Select Year Range:",
                        min = min(shiny.monthly.precip.records$year),
                        max = max(shiny.monthly.precip.records$year),
                        value = c(min(shiny.monthly.precip.records$year), max(shiny.monthly.precip.records$year)),
                        sep = "" # Prevent commas in year values
                      )
                    )
                  )
                ),

                  # max precip record plot output
                  fluidRow(
                    column(
                    width = 12,
                    box(
                      title = "Maximim NOAA Precipitation Records", 
                      status = "primary", 
                      solidHeader = TRUE, 
                      width = 12,
                      plotlyOutput("MaxPrecipRecordsPlot", height = "600px")
                    )
                  )
                ),
                  # min precip record plot output
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "Minimum NOAA Precipitation Records", 
                        status = "primary", 
                        solidHeader = TRUE, 
                        width = 12,
                      plotlyOutput("MinPrecipRecordsPlot", height = "600px")
                    )
                  )
              )
            )
      )
    ),
      
      # Tab for interactive sea level plots
      tabItem(tabName = "sea",
              h2("Sea Level Trends"),
              p("Plots for sea level trends.")
      )
    )
  )
 )




