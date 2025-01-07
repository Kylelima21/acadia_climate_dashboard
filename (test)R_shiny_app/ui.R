# ui component

ui <- page_navbar(
  title = "Acadia Climate Dashboard",
  theme = bs_theme(version = 5, 
                   bootswatch = "cerulean",
                   primary = "#0066cc"),
  
  # Overview Panel
  nav_panel("Dashboard Overview",
            value = "overview",
            layout_columns(
              card(
                card_header("About"),
                p("Climate data dashboard showing temperature and precipitation trends in Acadia National Park."),
                p("Data sources: NOAA nClimGrid and McFarland Hill station")
              )
            )
  ),
  
  # Temperature Panel
  nav_panel("Temperature Trends",
            value = "temp",
            layout_sidebar(
              sidebar = sidebar(
                checkboxGroupInput(
                  "linesToShow",
                  "Select Temperature Data:",
                  choices = c(
                    "NOAA Average Temp." = "NOAA Average Temp",
                    "NOAA Average Maximum Temp." = "NOAA Average Max Temp",
                    "NOAA Average Minimum Temp." = "NOAA Average Min Temp",
                    "McFarland Average Temp." = "McFarland Average Temp",
                    "Linear Model for NOAA Average Temp" = "lm_noaa_temp",
                    "Linear Model for NOAA Average Max Temp" = "lm_noaa_max_temp",
                    "Linear Model for NOAA Average Min Temp" = "lm_noaa_min_temp",
                    "Linear Model for McFarland Average Temp" = "lm_mcfarland_temp"
                  ),
                  selected = c("NOAA Average Temp", "NOAA Average Max Temp")
                )
              ),
              
              layout_columns(
                card(
                  card_header("Temperature Trends"),
                  plotlyOutput("myInteractivePlot", height = PLOT_SETTINGS$plot_height)
                )
              ),
              
              layout_columns(
                card(
                  card_header("Temperature Anomalies"),
                  plotlyOutput("NOAAAnomPlot", height = PLOT_SETTINGS$plot_height)
                ),
                card(
                  card_header("McFarland Temperature Anomalies"),
                  plotlyOutput("McFarlandAnomPlot", height = PLOT_SETTINGS$plot_height)
                )
              )
            )
  ),
  
  # Precipitation Panel
  nav_panel("Precipitation Trends",
            value = "precip",
            layout_sidebar(
              sidebar = sidebar(
                checkboxGroupInput(
                  "linesToShowPrecip",
                  "Select Precipitation Data:",
                  choices = c(
                    "NOAA Total Precip." = "NOAA Precip",
                    "McFarland Total Precip." = "McFarland Precip",
                    "Linear Model for NOAA Precip" = "lm_noaa_precip",
                    "Linear Model for McFarland Precip" = "lm_mcfarland_precip"
                  ),
                  selected = c("NOAA Precip", "McFarland Precip")
                )
              ),
              
              layout_columns(
                card(
                  card_header("Precipitation Trends"),
                  plotlyOutput("PrecipPlot", height = PLOT_SETTINGS$plot_height)
                )
              ),
              
              layout_columns(
                card(
                  card_header("Precipitation Anomalies"),
                  plotlyOutput("NOAAPrecipAnomPlot", height = PLOT_SETTINGS$plot_height)
                ),
                card(
                  card_header("McFarland Precipitation Anomalies"),
                  plotlyOutput("McFarlandPrecipAnomPlot", height = PLOT_SETTINGS$plot_height)
                )
              )
            )
  )
)
