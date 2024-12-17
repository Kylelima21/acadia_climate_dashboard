# ui component 

#--------------------------------------#
####    Build R Shiny Dashboard     ####
#--------------------------------------# 

#### User interface (ui)

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Acadia Climate Dashboard",
    titleWidth = 325
  ),
  
  dashboardSidebar(
    width = 325,
    
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
              h2("Dashboard Overview"),
              p("Content for the dashboard overview can go here.")
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
                        title = "Temperature Data Adjustments",
                        status = "primary", 
                        solidHeader = TRUE, 
                        width = 12,
                        # Add checkbox group for line selection
                        checkboxGroupInput(
                          inputId = "linesToShow",
                          label = "Select Temperature Data to Display:",
                          choices = c("NOAA Average Temp." = "NOAA Average Temp",
                                      "NOAA Average Maximum Temp." = "NOAA Average Max Temp",
                                      "NOAA Average Minimum Temp." = "NOAA Average Min Temp",
                                      "McFarland Average Temp." = "McFarland Average Temp",
                                      "Linear Model for NOAA Average Temp" = "lm_noaa_temp",
                                      "Linear Model for NOAA Average Max Temp" = "lm_noaa_max_temp",
                                      "Linear Model for NOAA Average Min Temp" = "lm_noaa_min_temp",
                                      "Linear Model for McFarland Average Temp" = "lm_mcfarland_temp"
                          ),
                          selected = c("NOAA Average Temp", "NOAA Average Max Temp", "NOAA Average Min Temp", "McFarland Average Temp")
                        ),
                      
                      # Add slider for year range
                          sliderInput(
                            inputId = "year_range_temp",
                            label = "Select Year Range:",
                            min = min(shiny.merged.temp$year),
                            max = max(shiny.merged.temp$year),
                            value = c(min(shiny.merged.temp$year), max(shiny.merged.temp$year)),
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
                        )
                      )
                    )
                  )
                ),
                  
                
                tabPanel(
                  "Temperature Anomalies", 
                  
                  #NOAA anom plot
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "NOAA Temperature Anomalies",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 15,
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
                        width = 15,
                        plotlyOutput("McFarlandAnomPlot", height = "600px")
                      )
                    )
                  )
                  
                ),
                
                tabPanel(
                  "Temperature Records and Extremes",
                  
                  # Add slider for year range
                  fluidRow(
                    column(
                      width = 4,
                      sliderInput(
                        inputId = "year_range",
                        label = "Select Year Range:",
                        min = min(shiny.monthly.records$year),
                        max = max(shiny.monthly.records$year),
                        value = c(min(shiny.monthly.records$year), max(shiny.monthly.records$year)),
                        sep = "" # Prevent commas in year values
                      )
                    )
                  ),
                  
                  # max temp record plot output
                  fluidRow(
                    column(
                      width = 12,
                      plotlyOutput("MaxTempRecordsPlot", height = "600px")
                    )
                  ),
                  
                  # min temp records plot output
                  fluidRow(
                    column(
                      width = 12,
                      plotlyOutput("MinTempRecordsPlot", height = "600px")

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
                        title = "Precipitation Data Adjustments",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        checkboxGroupInput(
                          inputId = "linesToShowPrecip",
                          label = "Select Precipitation Data to Display:",
                          choices = c("NOAA Total Precip." = "NOAA Precip",
                                      "McFarland Total Precip." = "McFarland Precip",
                                      "Linear Model for NOAA Precip" = "lm_noaa_precip",
                                      "Linear Model for McFarland Precip" = "lm_mcfarland_precip"),
                          selected = c("NOAA Precip", "McFarland Precip")
                        ),
                      
                      # Add slider for year range
                      sliderInput(
                        inputId = "year_range_precip",
                        label = "Select Year Range:",
                        min = min(shiny.merged.precip$year),
                        max = max(shiny.merged.precip$year),
                        value = c(min(shiny.merged.precip$year), max(shiny.merged.precip$year)),
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
                        )
                      )
                    )
                  )
                ),

                
                tabPanel(
                  "Precipitation Anomalies", 
                  
                  #NOAA anom plot
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "NOAA Precipitation Anomalies",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 15,
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
                        width = 15,
                        plotlyOutput("McFarlandPrecipAnomPlot", height = "600px")
                      )
                    )
                  )
                  
                ),
                
                tabPanel(
                  "Precipitation Records and Extremes",

                  # Add slider for year range
                  fluidRow(
                    column(
                      width = 4,
                      sliderInput(
                        inputId = "year_range",
                        label = "Select Year Range:",
                        min = min(shiny.monthly.precip.records$year),
                        max = max(shiny.monthly.precip.records$year),
                        value = c(min(shiny.monthly.precip.records$year), max(shiny.monthly.precip.records$year)),
                        sep = "" # Prevent commas in year values
                      )
                    )
                  ),

                  # max precip record plot output
                  fluidRow(
                    column(
                      width = 12,
                      plotlyOutput("MaxPrecipRecordsPlot", height = "600px")
                    )
                  ),
                  # min precip record plot output
                  fluidRow(
                    column(
                      width = 12,
                      plotlyOutput("MinPrecipRecordsPlot", height = "600px")
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




