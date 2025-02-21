# ui component 

#--------------------------------------#
####    Build R Shiny Dashboard     ####
#--------------------------------------# 

#### functions ####

## function for generating data manipulation panel next to plots (e.i. check boxes, sliders, etc.)
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
                # row with text and images
                column(width = 12,
                       box(
                         title = "Dashboard Overview",
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,
                         fluidRow(
                           # Left column for images
                           column(
                             width = 4,
                             div(
                               style = "text-align: center;",
                               tags$img(src = "img/COPY2_SchoodicInstitute_Horizontal_CMYK.png",
                                        width = "100%",
                                        style = "margin-bottom: 20px; margin-top: 20px",
                                        alt = "Schoodic Institute logo" 
                               ),
                               tags$img(
                                 src = "img/dashboard_overview_acadia_full.jpg",
                                 width = "100%",
                                 style = "margin-bottom: 10px; border: 2px solid #ccc; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border-radius: 5px;",
                                 alt = "View of Acadia National Park from Schoodic Head" 
                               ))),
                           column(
                             width = 8,
                             div(
                               style = "font-size: 16px; line-height: 1.6;",
                               p("This R Shiny Dashboard summarizes climate data from Acadia National Park gathered from local weather stations, the National Oceanic and Atmospheric Administration (NOAA), and the National Oceanography Centre (NOC). Local data was gathered from the McFarland Hill Atmospheric Research Station, the Winter Harbor-SERC weather station, and the Bar Harbor, Frenchman Bay, ME Station. Climate data was compiled and cleaned to produce visualizations of temperature and precipitation long-term trends, anomalies, and extremes as well as long-term sea level trends."),
                               tags$ul(
                                 style = "font-size: 16px;",
                                 tags$li(HTML('Climate summaries were created from daily and monthly gridded climate data downloaded from NOAA\'s National Centers for Environmental Information (<a href="https://www.ncei.noaa.gov" target="_blank">NCEI </a>). Climate data was compiled and cleaned using R scripts by Kyle Lima, built from the climateNETN package by Kate Miller (<a href="https://github.com/KateMMiller/climateNETN" target="_blank">climateNETN </a>).')),
                                 tags$li("Climate summaries were created from hourly data collected by the McFarland Hill Atmospheric Research Station."),
                                 tags$li("Climate summaries were created from 15 minute interval data collected by the Winter Harbor-SERC weather station (ID: D2258)."),
                                 tags$li(HTML('Sea level trend visualizations were created from monthly and annual mean sea level data collected by the Bar Harbor, Frenchman Bay, ME Station (ID: 525) and documented by the Permanent Service for Mean Sea Level (<a href="https://psmsl.org/" target="_blank">PSMSL </a>), based at the NOC, which specializes in providing tide gauge data around the world.'))),
                               p(HTML("<strong>Data Access</strong>: data from all sources used in this app and R scripts for data compiling and cleaning can be downloaded at this page: link. Instructions for data downloading and R script use are also provided."))
                             )
                           )
                         )
                       )
                )
              ),
              
              #second row with map
              fluidRow(
                column(width = 12,
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
                  
                  # Add figure/descriptive text 
                  br(),
                  fluidRow(
                    column(width = 12,
                           box(
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             p("Plotted below are annual average maximum, minimum, and mean temperature trends from 1895 to 2024 for data derived from NOAA NClimGrid datasets. Also plotted are McFarland Hill annual average temperature data which spans from 1999 to 2024 and SERC annual average temperature data which spans from 2009 to 2024. The data tools on the left can be used to add or remove elements from the plot; if linear models are added, the corresponding model statistics are calculated and provided in the model statistics box below.")
                           )
                    )
                  ),
                  
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
                                      "NOAA Average Mean Temp." = "NOAA Average Mean Temp",
                                      "NOAA Average Minimum Temp." = "NOAA Average Min Temp",
                                      "McFarland Average Temp." = "McFarland Average Temp",
                                      "SERC Average Temp." = "SERC Average Temp",
                                      "Linear Model for NOAA Average Max Temp" = "lm_noaa_max_temp",
                                      "Linear Model for NOAA Average Mean Temp" = "lm_noaa_temp",
                                      "Linear Model for NOAA Average Min Temp" = "lm_noaa_min_temp",
                                      "Linear Model for McFarland Average Temp" = "lm_mcfarland_temp",
                                      "Linear Model for SERC Average Temp" = "lm_serc_temp"
                          ),
                          selected = c("NOAA Average Mean Temp", "NOAA Average Max Temp", "NOAA Average Min Temp", "McFarland Average Temp", "SERC Average Temp")
                        ),
                      
                      # Add slider for year range
                          sliderInput(
                            inputId = "year_range_temp",
                            label = "Select Year Range:",
                            min = min(temp.data.merged$year),
                            max = max(temp.data.merged$year),
                            value = c(min(temp.data.merged$year), max(temp.data.merged$year)),
                            sep = ""
                        )
                      )
                    ),
                    
                    # Column for the plot
                    column(
                      width = 8,
                      box(
                        title = "Long-Term Annual Temperature Trends", 
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
                        
                        # NOAA average temp model stats
                        conditionalPanel(
                          condition = "input.linesToShow.includes('lm_noaa_temp')",
                          h4("NOAA Average Mean Temperature Model"),
                          verbatimTextOutput("noaa_temp_model_summary")
                        ),
                        
                        # NOAA max temp model stats
                        conditionalPanel(
                          condition = "input.linesToShow.includes('lm_noaa_max_temp')",
                          h4("NOAA Maximum Temperature Model"),
                          verbatimTextOutput("noaa_max_temp_model_summary")
                        ),
                        
                        # NOAA min temp model stats
                        conditionalPanel(
                          condition = "input.linesToShow.includes('lm_noaa_min_temp')",
                          h4("NOAA Minimum Temperature Model"),
                          verbatimTextOutput("noaa_min_temp_model_summary")
                        ),
                        
                        # McFarland temp model stats
                        conditionalPanel(
                          condition = "input.linesToShow.includes('lm_mcfarland_temp')",
                          h4("McFarland Temperature Model"),
                          verbatimTextOutput("mcfarland_temp_model_summary")
                        ),
                        # SERC temp model stats
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
                  
                  # First fluidRow with slider and text box side by side
                  fluidRow(
                    # Column for the slider (left side)
                    br(),
                    column(
                      width = 4,
                      box(
                        title = "Data Tools",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        sliderInput(
                          inputId = "year_range_temp_anom",
                          label = "Select Year Range:",
                          min = min(anom.temp.merged$year),
                          max = max(anom.temp.merged$year),
                          value = c(min(anom.temp.merged$year), max(anom.temp.merged$year)),
                          sep = ""
                        )
                      )
                    ),
                    
                    # Column for the explanatory text (right side)
                    column(
                      width = 8,
                      box(
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        p("Plotted below are monthly temperature anomalies which represent the difference between observed temperatures and historic baseline temperatures. Positive anomalies (above the baseline in red) indicate temperatures that are above (warmer than) the historic baseline. Negative anomalies (below the baseline in blue) indicate temperatures that are below (cooler than) the historic baseline."),
                        tags$ul(
                          tags$li("NOAA monthly temperature anomalies were calculated for 1895 to 2024. The historic baseline was calculated by averaging the mean temperature for each month of the year from 1901-2000 to generate a 20th century baseline. Data was derived from the Monthly NOAA nClimGrid dataset for these calculations and anomaly plot visualization."),
                          tags$li("McFarland Hill monthly temperature anomalies were calculated for 1999 to 2024. The historic baseline was derived from NOAA average temperature normals for 1981 to 2010 legacy period."),
                          tags$li("SERC monthly temperature anomalies were calculated for 2009 to 2024. The historic baseline was derived from NOAA average temperature normals for the 1981 to 2010 legacy period."))
                        )
                      )
                    ),
                  
                  #NOAA temp anom plot
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
                  
                  #McFarland temp anom plot 
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
                  ),
                  
                  #SERC temp anom plot 
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "SERC Temperature Anomalies",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("SERCAnomPlot", height = "600px")
                      )
                    )
                  )
                  
                ),
                
                # tab for temp records plots
                tabPanel(
                  "Temperature Records and Extremes",
                  
                  # Add figure/descriptive text 
                  br(),
                  fluidRow(
                    column(width = 12,
                           box(
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             p("Plotted below are monthly and daily temperature records and extremes:"),
                             tags$ul(
                               tags$li("Highest monthly mean and maximum temperature records of each year."),
                               tags$li("Lowest monthly mean and minimum temperature records of each year."),
                               tags$li("Highest daily mean and maximum temperature records of each year."),
                               tags$li("Lowest daily mean and minimum temperature records of each year.")),
                             p("Data are derived from NOAA NClimGrid datasets.")
                           )
                    )
                  ),
                  
                  create_temp_records_panel(
                    list(
                      # First plot (monthly maximum temperatures)
                      list(
                        plot_title = "Highest Monthly NOAA Temperature Records",
                        year_range_id = "year_range_records",
                        checkbox_id = "temp_records_display",
                        plot_id = "MaxTempRecordsPlot",
                        data_source = records.noaa.monthly,
                        checkbox_choices = c(
                          "Highest Monthly Mean Temperature Records" = "mean_max_temp",
                          "Highest Monthly Maximum Temperature Records" = "max_temp"
                        ),
                        default_selected = c("mean_max_temp")
                      ),
                      
                      # Second plot (monthly minimum temperatures)
                      list(
                        plot_title = "Lowest Monthly NOAA Temperature Records",
                        year_range_id = "year_range_records2",
                        checkbox_id = "min_temp_records_display",
                        plot_id = "MinTempRecordsPlot",
                        data_source = records.noaa.monthly,
                        checkbox_choices = c(
                          "Lowest Monthly Mean Temperature Records" = "mean_min_temp",
                          "Lowest Monthly Minimum Temperature Records" = "min_temp"
                        ),
                        default_selected = c("mean_min_temp")
                      ),
                      
                      # Third plot (daily maximum temperatures)
                      list(
                        plot_title = "Highest Daily NOAA Temperature Records",
                        year_range_id = "year_range_records3",
                        checkbox_id = "daily_max_temp_display",
                        plot_id = "DailyMaxRecordsPlot",
                        data_source = records.noaa.daily,  
                        checkbox_choices = c(
                          "Highest Daily Mean Temperature Records" = "daily_mean_max_temp",
                          "Highest Daily Maximum Temperature Records" = "daily_max_temp"
                        ),
                        default_selected = c("daily_mean_max_temp")
                      ),
                      
                      # Fourth plot (daily maximum temperatures)
                      list(
                        plot_title = "Lowest Daily NOAA Temperature Records",
                        year_range_id = "year_range_records4",
                        checkbox_id = "daily_min_temp_display",
                        plot_id = "DailyMinRecordsPlot",
                        data_source = records.noaa.daily,  
                        checkbox_choices = c(
                          "Lowest Daily Mean Temperature Records" = "daily_mean_min_temp",
                          "Lowest Daily Minimum Temperature Records" = "daily_min_temp"
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
                  
                  # Add figure/descriptive text 
                  br(),
                  fluidRow(
                    column(width = 12,
                           box(
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             p("Plotted below are annual average total precipitation trends from 1895 to 2024 for data derived from NOAA NClimGrid datasets. Also plotted are McFarland Hill annual average total precipitation data which spans from 1999 to 2024 and SERC annual average total precipitation data which spans from 2009 to 2024. The data tools on the left can be used to add or remove elements from the plot; if linear models are added, the corresponding model statistics are calculated and provided in the model statistics box below.")
                           )
                    )
                  ),
                  
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
                        min = min(precip.data.merged$year),
                        max = max(precip.data.merged$year),
                        value = c(min(precip.data.merged$year), max(precip.data.merged$year)),
                        sep = ""
                        )
                      )
                    ),
                    
                    column(
                      width = 8,
                      box(
                        title = "Long-Term Annual Precipitation Trends",
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
                        
                        # NOAA precip model stats
                        conditionalPanel(
                          condition = "input.linesToShowPrecip.includes('lm_noaa_precip')",
                          h4("NOAA Precipitation Model"),
                          verbatimTextOutput("noaa_precip_model_summary")
                        ),
                        
                        # McFarland precip model stats
                        conditionalPanel(
                          condition = "input.linesToShowPrecip.includes('lm_mcfarland_precip')",
                          h4("McFarland Precipitation Model"),
                          verbatimTextOutput("mcfarland_precip_model_summary")
                        ),
                        
                        # SERC precip model stats
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
                  
                  # First fluidRow with slider and text box side by side
                  fluidRow(
                    # Column for the slider (left side)
                    br(),
                    column(
                      width = 4,
                      box(
                        title = "Data Tools",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        sliderInput(
                          inputId = "year_range_precip_anom",
                          label = "Select Year Range:",
                          min = min(anom.precip.merged$year),
                          max = max(anom.precip.merged$year),
                          value = c(min(anom.precip.merged$year), max(anom.precip.merged$year)),
                          sep = ""
                        )
                      )
                    ),
                    
                    # Column for the explanatory text (right side)
                    column(
                      width = 8,
                      box(
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        p("Plotted below are monthly percent precipitation anomalies which represent the percent difference between observed precipitation and historic baseline precipitation totals. Positive anomalies (red) indicate precipitation totals that are higher- or wetter-than-average conditions, while negative anomalies (blue) indicate precipitation totals that are lower- or drier-than-average conditions."),
                        tags$ul(
                          tags$li("NOAA monthly percent precipitation anomalies were calculated for 1895 to 2024. Historic baselines were calculated by averaging the total precipitation for each month of the year from 1901-2000 to generate a 20th century baseline. Data was derived from the Monthly NOAA NClimGrid dataset for these calculations and anomaly plot visualization."),
                          tags$li("McFarland Hill monthly percent precipitation anomalies were calculated for 1999 to 2024. The historic baseline was derived from NOAA average precipitation normals for 1981 to 2010 legacy period."),
                          tags$li("SERC monthly percent precipitation anomalies were calculated for 2009 to 2024. The historic baseline was derived from NOAA average precipitation normals for the 1981 to 2010 legacy period."))
                      )
                    )
                  ),
                  
                  #NOAA precip anom plot
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
                  
                  #McFarland precip anom plot 
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
                  ),
                  
                  #SERC precip anom plot 
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "SERC Precipitation Anomalies",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("SERCPrecipAnomPlot", height = "600px")
                      )
                    )
                  )
                  
                ),
                
                tabPanel(
                  "Precipitation Records and Extremes",
                  
                  # First fluidRow with slider and text box side by side
                  fluidRow(
                    # Column for the slider (left side)
                    br(),
                    column(
                      width = 4,
                      box(
                        title = "Data Tools",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        sliderInput(
                          inputId = "year_range_temp_anom",
                          label = "Select Year Range:",
                          min = min(records.noaa.monthly$year),
                          max = max(records.noaa.monthly$year),
                          value = c(min(records.noaa.monthly$year), max(records.noaa.monthly$year)),
                          sep = ""
                        )
                      )
                    ),
                    
                    # Column for the explanatory text (right side)
                    br(),
                    column(
                      width = 8,
                      box(
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        p("Plotted below are the highest monthly precipitation records of each year and the lowest monthly precipitation records of each year. Data are derived from NOAA NClimGrid datasets.")
                      )
                    )
                  ),

                  # max precip record plot output
                  fluidRow(
                    column(
                    width = 12,
                    box(
                      title = "Highest NOAA Precipitation Records", 
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
                        title = "Lowest NOAA Precipitation Records", 
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
              
              # Add figure/descriptive text 
              br(),
              fluidRow(
                column(width = 12,
                       box(
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         p("Plotted below are monthly sea level trends from 1947 to 2024 and annual sea level trends from 1948 to 2024 from the Bar Harbor, Frenchman Bay, ME Station (Station ID: 525). Data are derived from PSMSL. The data tools on the left can be used to add or remove elements from the plots; if linear models are added, the corresponding model statistics are calculated and provided in the model statistics boxes below the plots.")
                       )
                )
              ),
              
              # First fluidRow with slider and text box side by side
              fluidRow(
                # Column for the slider (left side)
                column(
                  width = 4,
                  box(
                    title = "Data Tools",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    # Add checkbox group for line selection
                    checkboxGroupInput(
                      inputId = "linesToShowMonthlySea",
                      label = "Select Temperature Data to Display:",
                      choices = c("Monthly Mean Sea Level (mm)" = "Monthly Mean Sea Level (mm)",
                                  "Linear Model for Monthly Sea Level" = "lm_monthly_sea"),
                      selected = c("Monthly Mean Sea Level (mm)")
                    ),
                    
                    sliderInput(
                      inputId = "year_range_monthly_sea_level",
                      label = "Select Year Range:",
                      min = min(frenchman.monthly.clean$year),
                      max = max(frenchman.monthly.clean$year),
                      value = c(min(frenchman.monthly.clean$year), max(frenchman.monthly.clean$year)),
                      sep = ""
                    )
                  )
                ),
              
              #Monthly Sea Level Plot
                column(
                  width = 8,
                  box(
                    title = "Bar Harbor Monthly Sea Level Trends",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotlyOutput("MonthlySeaLevel", height = "600px")
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
                    
                    # NOAA average temp model stats
                    conditionalPanel(
                      condition = "input.linesToShowMonthlySea.includes('lm_monthly_sea')",
                      h4("Monthly Mean Sea Level Model"),
                      verbatimTextOutput("monthly_sea_model_summary")
                    )))),
              
              # First fluidRow with slider and text box side by side
              fluidRow(
                # Column for the slider (left side)
                column(
                  width = 4,
                  box(
                    title = "Data Tools",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    # Add checkbox group for line selection
                    checkboxGroupInput(
                      inputId = "linesToShowAnnualSea",
                      label = "Select Temperature Data to Display:",
                      choices = c("Annual Mean Sea Level (mm)" = "Annual Mean Sea Level (mm)",
                                  "Linear Model for Annual Sea Level" = "lm_annual_sea"),
                      selected = c("Annual Mean Sea Level (mm)")
                    ),
                    sliderInput(
                      inputId = "year_range_annual_sea_level",
                      label = "Select Year Range:",
                      min = min(frenchman.annual.clean$year),
                      max = max(frenchman.annual.clean$year),
                      value = c(min(frenchman.annual.clean$year), max(frenchman.annual.clean$year)),
                      sep = ""
                    )
                  )
                ),
              
              # annual sea level plot
                column(
                  width = 8,
                  box(
                    title = "Bar Harbor Annual Sea Level Trends",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotlyOutput("AnnualSeaLevel", height = "600px")
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
                    
                    # NOAA average temp model stats
                    conditionalPanel(
                      condition = "input.linesToShowAnnualSea.includes('lm_annual_sea')",
                      h4("Annual Mean Sea Level Model"),
                      verbatimTextOutput("annual_sea_model_summary")
                    ))))
              
        )
      )
    )
  )




