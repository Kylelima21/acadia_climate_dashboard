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
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Temperature Trends", tabName = "temp", icon = icon("chart-line")),
      menuItem("Precipitation Trends", tabName = "precip", icon = icon("chart-line"))
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
                        title = "Select Temperature Data to Display:",
                        status = "primary", 
                        solidHeader = TRUE, 
                        width = 12,
                        # Add checkbox group for line selection
                        checkboxGroupInput(
                          inputId = "linesToShow",
                          label = NULL,
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
                  ),
                  
                )
              )
      ),
      
      #Tab for interactive precipitation plot
      tabItem(tabName = "precip",
              
              tabsetPanel(
                tabPanel(
                  "Precipitation Trends",
                  
                  fluidRow(
                    column(
                      width = 4,
                      box(
                        title = "Select Precipitation Data to Display:",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        checkboxGroupInput(
                          inputId = "linesToShowPrecip",
                          label = NULL,
                          choices = c("NOAA Total Precip." = "NOAA Precip",
                                      "McFarland Total Precip." = "McFarland Precip",
                                      "Linear Model for NOAA Precip" = "lm_noaa_precip",
                                      "Linear Model for McFarland Precip" = "lm_mcfarland_precip"),
                          selected = c("NOAA Precip", "McFarland Precip")
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
                  ),
                  
                )
              )
      )
    )
  )
)
