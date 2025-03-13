## NPF Funded project to make climate data more accessible and describe extreme
## weather events in relation to historical norms

#--------------------------------------#
####          STARTING UP           ####
#--------------------------------------# 

## Source the data and packages
source("global.R")




#--------------------------------------#
####         USER INTERFACE         ####
#--------------------------------------#

ui <- fluidPage(
  
  ## ├ UI SET UP ----
  tags$head(
    tags$link(type = "text/css", rel = "stylesheet", href = "css/style.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("Acadia National Park Climate Dashboard"),
    # tags$script(src = "css/main.js", type = "module", "defer")
  ),
  
  HTML("<a href='#'><img src='img/btp.png' alt='back to top' 
       class='scrolltotop sttan'></a>"),
  
  
  ## ├ BODY ----
  tags$body(
    
    
    ## ¬ Header ----
    div(class = "titlepage-box",
        div(class = "topheader",
            tags$img(src = "img/SchoodicInstitute_Horizontal_CMYK_WHITE.png",
                     alt = "Schoodic Institute at Acadia National Park logo",
                     class = "topheader-logo"),
            tags$a(class = "topheader-btn", href = "https://kylelima21.github.io/acadia_climate_dashboard/", target = "_blank",
                   "Download data"),
        ),
        div(class = "header-content",
            HTML("<h1>Acadia Climate <span class='grtxt'>Dashboard</span></h1>"),
            p(class = "headerp",
              "This dashboard summarizes climate data from Acadia National Park gathered from local weather stations and models. The goal of this dashboard is to make it easier to access and view local climate, weather, and sea level data and trends in them over time. We also try to emphasize extreme events and allow for the comparison to historical norms."),
            div(class = "header-update",
                p(class = "updatep", HTML("<i>Updated: March 2025</i>"))
            ),
        ),
    ),
    
    
    ### Tab Content
    navset_card_underline(selected = "Temperature Trends",
      
      ## ¬ Dashboard Overview ----
      nav_panel("Dashboard Overview",
                div(class = "summary-box",
                    div(class = "sumtxt",
                        h2("Summary"),
                        p("This dashboard summarizes climate data from Acadia National Park gathered from local weather stations and models, the National Oceanic and Atmospheric Administration (NOAA), and the National Oceanography Centre (NOC). Local data was gathered from the McFarland Hill Atmospheric Research Station, the Winter Harbor-SERC weather station, and the Bar Harbor, Frenchman Bay, ME Station. Climate data was compiled and cleaned to produce visualizations of temperature and precipitation long-term trends, anomalies, and extremes as well as long-term sea level trends. Dashboard design was inspired by the Organisation for Economic Co-operation and Development's Climate Action Dashboard.")),
                    tags$img(class = "sumimg",
                             src = "img/IMG_2172.png",
                             alt = "Raindrops and sun through a car windsheild")
                ),
                div(class = "map-box",
                    div(class = "maptxt",
                        h2("Weather Station Locations"),
                        p("Explore the map to see the locations of the weather stations that we have included data from in this dashboard. Click on a marker to see the station name and coordinates.")
                    ),
                    div(class = "maploc",
                        leafletOutput("LocationMap")
                    )
                ),
                div(
                  h2(class = "wstxt", "About the Data"),
                  tags$ul(
                    tags$li(HTML('Climate summaries were created from daily and monthly gridded climate data downloaded from NOAA\'s National Centers for Environmental Information (<a href="https://www.ncei.noaa.gov" target="_blank">NCEI</a>).')),
                    tags$li(HTML('Climate data was compiled and cleaned using R scripts by Kyle Lima, built from the <a href="https://github.com/KateMMiller/climateNETN" target="_blank">climateNETN package</a> by Kate Miller.')),
                    tags$li("Climate summaries were created from hourly data collected by the McFarland Hill Atmospheric Research Station."),
                    tags$li("Climate summaries were created from 15 minute interval data collected by the Winter Harbor-SERC weather station (ID: D2258)."),
                    tags$li(HTML('Sea level trend visualizations were created from monthly and annual mean sea level data collected by the Bar Harbor, Frenchman Bay, ME Station (ID: 525) and documented by the Permanent Service for Mean Sea Level (<a href="https://psmsl.org/" target="_blank">PSMSL</a>), based at the NOC, which specializes in providing tide gauge data around the world.'))),
                ),
                div(
                  h2(class = "wstxt", "Data Access"),
                  p(HTML('Data from all sources used in this app and R scripts for data compiling and cleaning can be downloaded following the instructions on the <a href="https://kylelima21.github.io/acadia_climate_dashboard/" target="_blank">data download page</a>. Instructions for data downloading and R script use are also provided.')),
                ),
                div(
                  h2(class = "wstxt", "Contact"),
                  p(HTML('Found an error, have questions, or just want to reach out? Please contact Kyle Lima at klima@schoodicinstitute.org.')),
                )
      ),
      
      
      ## ¬ Temperature Trends ----
      nav_panel("Temperature Trends",
                div(class = "trend-box",
                    div(class = "leg-t-box",
                        h3("Data Filtering Tools"),
                        checkboxGroupInput(
                          inputId = "linesToShow",
                          label = "Select data to display:",
                          choices = c("NOAA Average Maximum Temp." = "NOAA Average Max Temp",
                                      "NOAA Average Mean Temp." = "NOAA Average Mean Temp",
                                      "NOAA Average Minimum Temp." = "NOAA Average Min Temp",
                                      "McFarland Average Temp." = "McFarland Average Temp",
                                      "SERC Average Temp." = "SERC Average Temp",
                                      "Linear Model for NOAA Average Max Temp" = "lm_noaa_max_temp",
                                      "Linear Model for NOAA Average Mean Temp" = "lm_noaa_temp",
                                      "Linear Model for NOAA Average Min Temp" = "lm_noaa_min_temp",
                                      "Linear Model for McFarland Average Temp" = "lm_mcfarland_temp",
                                      "Linear Model for SERC Average Temp" = "lm_serc_temp"),
                          selected = c("NOAA Average Mean Temp", "NOAA Average Max Temp", "NOAA Average Min Temp", "McFarland Average Temp", "SERC Average Temp")),
    
                          div(class = "slider",
                            sliderInput(
                              inputId = "year_range_temp",
                              label = "Select year range:",
                              min = min(temp.data.merged$year),
                              max = max(temp.data.merged$year),
                              value = c(min(temp.data.merged$year), max(temp.data.merged$year)),
                              sep = "")
                          ),
                    ),
                    
                    div(class = "plot-t-box",
                        plotlyOutput("myInteractivePlot")
                    )
                ),
                
                div(class = "trend-box",
                    sliderInput(
                      inputId = "year_range_temp_anom",
                      label = "Select Year Range:",
                      min = min(anom.temp.merged$year),
                      max = max(anom.temp.merged$year),
                      value = c(min(anom.temp.merged$year), max(anom.temp.merged$year)),
                      sep = ""
                    ),
                    
                    plotlyOutput("NOAAAnomPlot", height = "600px"),
                    
                    plotlyOutput("McFarlandAnomPlot", height = "600px"),
                    
                    plotlyOutput("SERCAnomPlot", height = "600px"),
                    )
      ),
      
      
      ## ¬ Precip Trends ----
      nav_panel("Precipitation Trends",
                div(class = "trend-box",
                    checkboxGroupInput(
                      inputId = "linesToShowPrecip",
                      label = "Select Precipitation Data to Display:",
                      choices = c("NOAA Total Precip." = "NOAA Precip",
                                  "McFarland Total Precip." = "McFarland Precip",
                                  "SERC Total Precip." = "SERC Precip",
                                  "Linear Model for NOAA Precip" = "lm_noaa_precip",
                                  "Linear Model for McFarland Precip" = "lm_mcfarland_precip",
                                  "Linear Model for SERC Precip" = "lm_serc_precip"),
                      selected = c("NOAA Precip", "McFarland Precip", "SERC Precip")),
                    
                    sliderInput(
                      inputId = "year_range_precip",
                      label = "Select Year Range:",
                      min = min(precip.data.merged$year),
                      max = max(precip.data.merged$year),
                      value = c(min(precip.data.merged$year), max(precip.data.merged$year)),
                      sep = "")
                    ),
                
                plotlyOutput("PrecipPlot", height = "600px"),
                
                sliderInput(
                  inputId = "year_range_precip_anom",
                  label = "Select Year Range:",
                  min = min(anom.precip.merged$year),
                  max = max(anom.precip.merged$year),
                  value = c(min(anom.precip.merged$year), max(anom.precip.merged$year)),
                  sep = ""),
                
                plotlyOutput("NOAAPrecipAnomPlot", height = "600px"),
                
                plotlyOutput("McFarlandPrecipAnomPlot", height = "600px"),
                
                plotlyOutput("SERCPrecipAnomPlot", height = "600px"),
                
                
                sliderInput(
                  inputId = "year_range_temp_anom",
                  label = "Select Year Range:",
                  min = min(records.noaa.monthly$year),
                  max = max(records.noaa.monthly$year),
                  value = c(min(records.noaa.monthly$year), max(records.noaa.monthly$year)),
                  sep = ""),
                
                plotlyOutput("MaxPrecipRecordsPlot", height = "600px"),
                
                plotlyOutput("MinPrecipRecordsPlot", height = "600px")
                
      ),
      
      ## ¬ Sea Level Trends ----
      nav_panel("Sea Level Trends",
                
                checkboxGroupInput(
                  inputId = "linesToShowMonthlySea",
                  label = "Select Temperature Data to Display:",
                  choices = c("Monthly Mean Sea Level (mm)" = "Monthly Mean Sea Level (mm)",
                              "Linear Model for Monthly Sea Level" = "lm_monthly_sea"),
                  selected = c("Monthly Mean Sea Level (mm)")),
                
                sliderInput(
                  inputId = "year_range_monthly_sea_level",
                  label = "Select Year Range:",
                  min = min(frenchman.monthly.clean$year),
                  max = max(frenchman.monthly.clean$year),
                  value = c(min(frenchman.monthly.clean$year), max(frenchman.monthly.clean$year)),
                  sep = ""),
                
                plotlyOutput("MonthlySeaLevel", height = "600px"),
                
                checkboxGroupInput(
                  inputId = "linesToShowAnnualSea",
                  label = "Select Temperature Data to Display:",
                  choices = c("Annual Mean Sea Level (mm)" = "Annual Mean Sea Level (mm)",
                              "Linear Model for Annual Sea Level" = "lm_annual_sea"),
                  selected = c("Annual Mean Sea Level (mm)")),
                
                sliderInput(
                  inputId = "year_range_annual_sea_level",
                  label = "Select Year Range:",
                  min = min(frenchman.annual.clean$year),
                  max = max(frenchman.annual.clean$year),
                  value = c(min(frenchman.annual.clean$year), max(frenchman.annual.clean$year)),
                  sep = ""),
                
                plotlyOutput("AnnualSeaLevel", height = "600px")
                
      ),
    ),
    
    
    
    ## ¬ Footer ----
    div(class = "footer-box",
        div(class = "footer-content",
            div(class = "footer-si",
                tags$img(src = "img/SchoodicInstitute_Horizontal_CMYK.png",
                         alt = "Schoodic Institute at Acadia National Park logo",
                         class = "footer-logo"),
                p(HTML("<i>Our Mission is inspiring science, learning, and community for a changing world.</i>")),
            ),
            div(
              h1("Data Access"),
              p(HTML("<a href='https://kylelima21.github.io/acadia_climate_dashboard/' target='_blank'>Instruction page for downloading and cleaning data</a>")),
            ),
            div(
              h1("Data"),
              p(HTML("<a href='https://www.ncei.noaa.gov/' target='_blank'>NOAA National Centers for Environmental Information</a>")),
              p(HTML("<a href='https://ard-request.air-resource.com/data.aspx' target='_blank'>McFarland Hill Atmospheric Research Station</a>")),
              p(HTML("<a href='https://mesowest.utah.edu/cgi-bin/droman/meso_base_dyn.cgi?stn=D2258' target='_blank'>MesoWest SERC weather station</a>")),
              p(HTML("<a href='https://psmsl.org/data/obtaining/stations/525.php' target='_blank'>NOC Frenchman Bay tide gauge</a>")),
            ),
            div(
              h1("Source Code"),
              p(HTML("<a href='https://github.com/Kylelima21/acadia_climate_dashboard' target='_blank'>GitHub repository</a>")),
            ),
            div(
              h1("Website"),
              p(HTML("<a href='https://schoodicinstitute.org/' target='_blank'>Visit our website</a>")),
            ),
            div(
              h1("Contact"),
              p("Contact us at klima@schoodicinstitute.org"),
            ),
        ),
        div(class = "copyright",
            p(paste0("© ", year(Sys.Date()), " Schoodic Institute | Created by Natalia Portales and Kyle Lima"))
        ),
    )
  )
  
)



#--------------------------------------#
####             SERVER             ####
#--------------------------------------#

server <- function(input, output) {
  
  #----------------------#
  ####    Map Output  ####
  #----------------------# 
  
  ## Weather station map
  output$LocationMap <- renderLeaflet({
    
    # Define location data
    station_locations <- data.frame(
      name = c("McFarland Hill Atmospheric Research Station", 
               "Winter Harbor-SERC Weather Station", 
               "NOAA - Acadia National Park",
               "Bar Harbor, Frenchman Bay Station"),
      lat = c(44.3772, 44.33567, 44.372907, 44.391667),
      lng = c(-68.2608, -68.062, -68.258257, -68.205)
    )
    
    # Create the map
    leaflet() %>%
      # Add both base layers Esri.WorldTopoMap
      addProviderTiles("Stadia.OSMBright", group = "Topographic") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      setView(lng = -68.19, lat = 44.3386, zoom = 11) %>%  # Center on Acadia
      
      #Add markers for stations
      addMarkers(
        data = station_locations,
        lng = ~lng, 
        lat = ~lat,
        popup = ~paste0("<strong>", name, "</strong><br>", lat, ", ", lng),
        group = "Stations"
      ) %>%
      
      addLayersControl(
        baseGroups = c("Topographic", "Satellite"),
        overlayGroups = c("Stations"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  #---------------------------------------#
  ####  Reactive data transformations  ####
  #---------------------------------------#   
  
  # Reactive for temperature data
  temperature_data <- reactive({
    temp.data.merged %>% 
      rename(
        Year = year, 
        `NOAA Average Mean Temp` = noaa.temp,
        `NOAA Average Max Temp` = noaa.max.temp, 
        `NOAA Average Min Temp` = noaa.min.temp, 
        `McFarland Average Temp` = mcfarland.temp,
        `SERC Average Temp`= serc.temp
      )
  })
  
  # Reactive for precipitation data
  precipitation_data <- reactive({
    precip.data.merged %>% 
      rename(
        Year = year, 
        `NOAA Precip` = noaa.precip,
        `McFarland Precip` = mcfarland.precip,
        `SERC Precip` = serc.precip
      )
  })
  
  # Reactive for temperature anomaly data
  temp_anomaly_data <- reactive({
    anom.temp.merged %>%
      rename(
        Year = year, 
        `Year-Month` = noaa.year.month, 
        `NOAA Temp Anomaly (°C)` = noaa.temp.anom,
        `McFarland Temp Anomaly (°C)` = mcfarland.temp.anom,
        `SERC Temp Anomaly (°C)` = serc.temp.anom
      ) %>%
      mutate(
        `Year-Month` = as.Date(`Year-Month`),
        noaa_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>NOAA Temp Anomaly:", round(`NOAA Temp Anomaly (°C)`, 4)
        ),
        mcfarland_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>McFarland Temp Anomaly:", round(`McFarland Temp Anomaly (°C)`, 4)
        ),
        serc_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>SERC Temp Anomaly:", round(`SERC Temp Anomaly (°C)`, 4)
        )
      )  %>%
      # Add filter based on slider input
      filter(
        Year >= input$year_range_temp_anom[1],
        Year <= input$year_range_temp_anom[2]
      )
  })
  
  # Reactive for precipitation anomaly data
  precip_anomaly_data <- reactive({
    anom.precip.merged %>%
      rename(
        Year = year, 
        `Year-Month` = noaa.year.month, 
        `NOAA Precip Anomaly (%)` = noaa.percent.precip.anom,
        `McFarland Precip Anomaly (%)` = mcfarland.percent.precip.anom,
        `SERC Precip Anomaly (%)` = serc.percent.precip.anom
      ) %>%
      mutate(
        `Year-Month` = as.Date(`Year-Month`),
        noaa_precip_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>NOAA Precip Anomaly:", round(`NOAA Precip Anomaly (%)`, 4)
        ),
        mcfarland_precip_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>McFarland Precip Anomaly:", round(`McFarland Precip Anomaly (%)`, 4)
        ),
        serc_precip_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>SERC Precip Anomaly:", round(`SERC Precip Anomaly (%)`, 4)
        )
      ) %>%
      # Add filter based on slider input
      filter(
        Year >= input$year_range_precip_anom[1],
        Year <= input$year_range_precip_anom[2]
      )
  })
  
  # Reactive for linear models
  temp_models <- reactive({
    data <- temperature_data() %>%
      filter(Year >= input$year_range_temp[1], Year <= input$year_range_temp[2])
    
    list(
      noaa_avg = if ("lm_noaa_temp" %in% input$linesToShow) 
        lm(`NOAA Average Mean Temp` ~ Year, data = data),
      noaa_max = if ("lm_noaa_max_temp" %in% input$linesToShow) 
        lm(`NOAA Average Max Temp` ~ Year, data = data),
      noaa_min = if ("lm_noaa_min_temp" %in% input$linesToShow) 
        lm(`NOAA Average Min Temp` ~ Year, data = data),
      mcfarland = if ("lm_mcfarland_temp" %in% input$linesToShow) 
        lm(`McFarland Average Temp` ~ Year, data = data),
      serc = if ("lm_serc_temp" %in% input$linesToShow) 
        lm(`SERC Average Temp` ~ Year, data = data)
    )
  })
  
  # Reactive for precipitation models
  precip_models <- reactive({
    data <- precipitation_data() %>%
      filter(Year >= input$year_range_precip[1], Year <= input$year_range_precip[2])
    
    list(
      noaa_precip = if ("lm_noaa_precip" %in% input$linesToShowPrecip) 
        lm(`NOAA Precip` ~ Year, data = data),
      mcfarland_precip = if ("lm_mcfarland_precip" %in% input$linesToShowPrecip) 
        lm(`McFarland Precip` ~ Year, data = data),
      serc_precip = if ("lm_serc_precip" %in% input$linesToShowPrecip) 
        lm(`SERC Precip` ~ Year, data = data)
    )
  })
  
  # Reactive for monthly sea level data
  monthly_sea_level_data <- reactive({
    frenchman.monthly.clean %>%
      rename(
        Year = year, 
        `Year-Month` = year.month, 
        `Monthly Mean Sea Level (mm)` = mean.sea.level.mm,
      ) %>%
      mutate(
        `Year-Month` = as.Date(`Year-Month`),
        monthly_sea_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>Mean Sea Level (mm):", round(`Monthly Mean Sea Level (mm)`, 3)
        )
      ) %>%
      # Add filter based on slider input
      filter(
        Year >= input$year_range_monthly_sea_level[1],
        Year <= input$year_range_monthly_sea_level[2]
      )
  })
  
  # Reactive for annual sea level data
  annual_sea_level_data <- reactive({
    frenchman.annual.clean %>%
      rename(
        Year = year, 
        `Annual Mean Sea Level (mm)` = mean.sea.level.mm,
      ) %>%
      mutate(
        annual_sea_hover_text = paste(
          "Year:", Year,
          "<br>Mean Sea Level (mm):", round(`Annual Mean Sea Level (mm)`, 3)
        )
      ) %>%
      # Add filter based on slider input
      filter(
        Year >= input$year_range_annual_sea_level[1],
        Year <= input$year_range_annual_sea_level[2]
      )
  })
  
  # Reactive for monthly sea level model
  monthly_sea_model <- reactive({
    data <- monthly_sea_level_data() %>%
      filter(Year >= input$year_range_monthly_sea_level[1], Year <= input$year_range_monthly_sea_level[2])
    
    list(
      monthly_sea = if ("lm_monthly_sea" %in% input$linesToShowMonthlySea) 
        lm(`Monthly Mean Sea Level (mm)` ~ Year, data = data)
    )
  })
  
  # Reactive for annual sea level model
  annual_sea_model <- reactive({
    data <-  annual_sea_level_data() %>%
      filter(Year >= input$year_range_annual_sea_level[1], Year <= input$year_range_annual_sea_level[2])
    
    list(
      annual_sea = if ("lm_annual_sea" %in% input$linesToShowAnnualSea) 
        lm(`Annual Mean Sea Level (mm)` ~ Year, data = data)
    )
  })
  
  
  #----------------------#
  ####   Functions    ####
  #----------------------# 
  
  # Helper function for adding hover text 
  customize_hover_text <- function(plt, units = "°C") {
    for(i in seq_along(plt$x$data)) {
      if(!is.null(plt$x$data[[i]]$name)) {
        if(!is.null(plt$x$data[[i]]$mode) && 
           !is.null(plt$x$data[[i]]$line$color) && 
           plt$x$data[[i]]$mode == "lines" && 
           identical(plt$x$data[[i]]$line$color, "black")) {
          
          base_name <- gsub("\\.$", "", plt$x$data[[i]]$name)
          base_name <- gsub("fitted values", paste(base_name, "trend"), base_name)
          
          plt$x$data[[i]]$hovertemplate <- paste0(
            base_name, ": %{y:.1f} ", units, "<br>",
            "<extra></extra>"
          )
        } else if(!is.null(plt$x$data[[i]]$fill) && 
                  plt$x$data[[i]]$fill == "tonexty") {
          plt$x$data[[i]]$hovertemplate <- paste0(
            "95% Confidence Interval: %{y:.1f} ", units, "<br>",
            "<extra></extra>"
          )
        } else if(!is.null(plt$x$data[[i]]$mode) && 
                  plt$x$data[[i]]$mode == "lines") {
          plt$x$data[[i]]$hovertemplate <- paste0(
            "%{data.name}: %{y:.1f} ", units, "<br>",
            "<extra></extra>"
          )
          plt$x$data[[i]]$name <- gsub("\\.$", "", plt$x$data[[i]]$name)
        }
      }
    }
    plt
  }
  
  
  # Helper function for adding model lines
  add_model_line <- function(plot, model, var_name) {
    plot +
      geom_smooth(
        aes(y = .data[[var_name]]),
        method = "lm",
        se = TRUE,
        fill = "grey80",
        alpha = 0.5,
        color = NA
      ) +
      geom_line(
        aes(y = .data[[var_name]]),
        stat = "smooth",
        method = "lm",
        color = "black",
        linewidth = 0.8
      )
  }
  
  
  #----------------------#
  ####  Plot outputs  ####
  #----------------------#
  
  #-------------------------------#
  ####  Long-Term Trend Plots  ####
  #-------------------------------# 
  
  # Temperature plot output ----------------------------------------------------
  output$myInteractivePlot <- renderPlotly({
    data <- temperature_data()
    models <- temp_models()
    
    # Filter data based on year range from slider
    filtered_data <- data %>%
      filter(Year >= input$year_range_temp[1], Year <= input$year_range_temp[2])
    
    #create ggplot output
    p <- ggplot(filtered_data, aes(x = Year)) +
      scale_x_continuous(breaks = pretty(filtered_data$Year)) +
      labs(title = NULL,
           x = "Year",
           y = "Temperature (°C)") +
      theme_minimal() #+
      #theme(axis.text = element_text(size = "1rem"),
            #axis.title = element_text(size = "1rem", margin = unit(c(0, 0, 5, 0), "mm")),
            #legend.text = element_text(size = "1rem"))
    
    # Add temperature lines based on selection
    #add noaa max temp
    if ("NOAA Average Max Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `NOAA Average Max Temp`,
                             color = "NOAA Average Maximum Temp."))
      
      if (!is.null(models$noaa_max)) {
        p <- add_model_line(p, models$noaa_max, "NOAA Average Max Temp")
        
      }
    }
    
    #add noaa average temp
    if ("NOAA Average Mean Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `NOAA Average Mean Temp`,
                             color = "NOAA Average Mean Temp."))
      
      if (!is.null(models$noaa_avg)) {
        p <- add_model_line(p, models$noaa_avg, "NOAA Average Mean Temp")
        
      }
    }
    
    #add noaa min temp
    if ("NOAA Average Min Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `NOAA Average Min Temp`,
                             color = "NOAA Average Minimum Temp."))
      
      if (!is.null(models$noaa_min)) {
        p <- add_model_line(p, models$noaa_min, "NOAA Average Min Temp")
        
      }
    }
    
    #add McFarland temp
    if ("McFarland Average Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `McFarland Average Temp`,
                             color = "McFarland Average Temp."))
      
      if (!is.null(models$mcfarland)) {
        p <- add_model_line(p, models$mcfarland, "McFarland Average Temp")
        
      }
    }
    
    #add SERC temp
    if ("SERC Average Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `SERC Average Temp`,
                             color = "SERC Average Temp."))
      
      if (!is.null(models$serc)) {
        p <- add_model_line(p, models$serc, "SERC Average Temp")
        
      }
    }
    
    # Customize the legend and colors
    p <- p + scale_color_manual(
      values = c(
        "NOAA Average Mean Temp." = "#000000", 
        "NOAA Average Maximum Temp." = "#CC3300", 
        "NOAA Average Minimum Temp." = "#003399", 
        "McFarland Average Temp." = "#00CC00",
        "SERC Average Temp." = "#996633"
      ),
      name = NULL
    )
    
    # Convert to plotly and customize hover text
    temp_plt <- ggplotly(p) %>%
      layout(
        showlegend = TRUE, 
        legend = list(
          itemclick = FALSE, 
          itemdoubleclick = FALSE,
          orientation = "h", 
          x = 0.5, 
          y = -0.2,
          xanchor = "center"),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(hoverformat = "%Y")
      ) %>%
      customize_hover_text(units = "°C") 
  })
  
  
  # Temp model summaries -------------------------------------------------------
  
  output$noaa_temp_model_summary <- renderPrint({
    req("lm_noaa_temp" %in% input$linesToShow)
    summary(temp_models()$noaa_avg)
  })
  
  output$noaa_max_temp_model_summary <- renderPrint({
    req("lm_noaa_max_temp" %in% input$linesToShow)
    summary(temp_models()$noaa_max)
  })
  
  output$noaa_min_temp_model_summary <- renderPrint({
    req("lm_noaa_min_temp" %in% input$linesToShow)
    summary(temp_models()$noaa_min)
  })
  
  output$mcfarland_temp_model_summary <- renderPrint({
    req("lm_mcfarland_temp" %in% input$linesToShow)
    summary(temp_models()$mcfarland)
  })
  
  output$serc_temp_model_summary <- renderPrint({
    req("lm_serc_temp" %in% input$linesToShow)
    summary(temp_models()$serc)
  })
  
  # Precipitation plot output --------------------------------------------------
  output$PrecipPlot <- renderPlotly({
    data <- precipitation_data()
    models <- precip_models()
    
    # Filter data based on year range from slider
    filtered_data <- data %>%
      filter(Year >= input$year_range_precip[1], Year <= input$year_range_precip[2])
    
    p2 <- ggplot(filtered_data, aes(x = Year)) +
      scale_x_continuous(breaks = pretty(filtered_data$Year)) +
      labs(title = NULL,
           x = "Year",
           y = "Total Precipitation (in)") +
      theme_minimal()
    
    # Add precipitation lines based on selection
    #add noaa precip data
    if ("NOAA Precip" %in% input$linesToShowPrecip) {
      p2 <- p2 + geom_line(aes(x = Year,
                               y = `NOAA Precip`,
                               color = "NOAA Total Precip."))
      
      if (!is.null(models$noaa_precip)) {
        p2 <- add_model_line(p2, models$noaa_precip, "NOAA Precip")
        
      }
    }
    
    #add McFarland precip data
    if ("McFarland Precip" %in% input$linesToShowPrecip) {
      p2 <- p2 + geom_line(aes(x = Year,
                               y = `McFarland Precip`,
                               color = "McFarland Total Precip."))
      
      if (!is.null(models$mcfarland_precip)) {
        p2 <- add_model_line(p2, models$mcfarland_precip, "McFarland Precip")
        
      }
    }
    
    #add SERC precip data
    if ("SERC Precip" %in% input$linesToShowPrecip) {
      p2 <- p2 + geom_line(aes(x = Year,
                               y = `SERC Precip`,
                               color = "SERC Total Precip."))
      
      if (!is.null(models$serc_precip)) {
        p2 <- add_model_line(p2, models$serc_precip, "SERC Precip")
        
      }
    }
    
    # Customize the legend and colors
    p2 <- p2 + scale_color_manual(
      values = c(
        "NOAA Total Precip." = "#000000", 
        "McFarland Total Precip." = "#00CC00",
        "SERC Average Precip." = "#996633"
      ),
      name = NULL
    )
    
    # Convert to plotly and customize hover text
    precip_plt <- ggplotly(p2) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          itemclick = FALSE, 
          itemdoubleclick = FALSE,
          orientation = "h", 
          x = 0.5, 
          y = -0.2,
          xanchor = "center"),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(hoverformat = "%Y")
      ) %>%
      customize_hover_text(units = "in")
  })
  
  # Precip model summaries -----------------------------------------------------
  
  output$noaa_precip_model_summary <- renderPrint({
    req("lm_noaa_precip" %in% input$linesToShowPrecip)
    summary(precip_models()$noaa_precip)
  })
  
  output$mcfarland_precip_model_summary <- renderPrint({
    req("lm_mcfarland_precip" %in% input$linesToShowPrecip)
    summary(precip_models()$mcfarland_precip)
  })
  
  output$serc_precip_model_summary <- renderPrint({
    req("lm_serc_precip" %in% input$linesToShowPrecip)
    summary(precip_models()$serc_precip)
  })
  
  #-----------------------#
  ####  Anomaly Plots  ####
  #-----------------------#  
  
  #create anomaly plot function
  create_anomaly_plot <- function(data, 
                                  x_col = "Year-Month", 
                                  y_col = "NOAA Temp Anomaly (°C)", 
                                  hover_text_col = "noaa_hover_text",
                                  legend_title = "Anomaly Data",
                                  break_interval = "10 years") {
    
    # Get date range for x-axis
    min_date <- min(data[[x_col]], na.rm = TRUE)
    max_date <- max(data[[x_col]], na.rm = TRUE)
    
    p <- ggplot(data, aes(x = .data[[x_col]])) +
      geom_bar(aes(
        y = .data[[y_col]],
        fill = factor(.data[[y_col]] > 0, 
                      levels = c(TRUE, FALSE), 
                      labels = c("Above baseline", "Below baseline")),
        text = .data[[hover_text_col]]
      ), stat = "identity") +
      scale_fill_manual(
        values = c("Above baseline" = "red",
                   "Below baseline" = "blue",
                   "Baseline" = "black"),
        name = legend_title) +
      geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
      scale_x_date(
        breaks = scales::breaks_width(break_interval),  
        labels = scales::date_format("%Y")
      )  +
      xlab("Year") +
      theme_classic() +
      theme(#axis.text = element_text(size = "1.2rem"),
            #axis.title = element_text(size = "1.2rem", margin = unit(c(0, 0, 5, 0), "mm")),
            #legend.text = element_text(size = "1.4rem"),
            panel.border = element_rect(linewidth = 1, fill = "transparent"))
    
    # Convert to plotly and disable legend clicking
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(
          itemclick = FALSE, 
          itemdoubleclick = FALSE,
          orientation = "h", 
          x = 0.5, 
          y = -0.2,
          xanchor = "center"),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(title = "Year")
      )
  }
  
  #create anomaly plots
  
  # For NOAA temperature anomalies
  output$NOAAAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = temp_anomaly_data(),
      x_col = "Year-Month",
      y_col = "NOAA Temp Anomaly (°C)",
      hover_text_col = "noaa_hover_text",
      legend_title = NULL,
      break_interval = "15 years"
    )
  })
  
  # For McFarland temperature anomalies
  output$McFarlandAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = temp_anomaly_data(),
      x_col = "Year-Month",
      y_col = "McFarland Temp Anomaly (°C)",
      hover_text_col = "mcfarland_hover_text",
      legend_title = NULL,
      break_interval = "5 years"
    )
  })
  
  # For SERC temperature anomalies
  output$SERCAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = temp_anomaly_data(),
      x_col = "Year-Month",
      y_col = "SERC Temp Anomaly (°C)",
      hover_text_col = "serc_hover_text",
      legend_title = NULL,
      break_interval = "5 years"
    )
  })
  
  # For NOAA precipitation anomalies
  output$NOAAPrecipAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = precip_anomaly_data(),
      x_col = "Year-Month",
      y_col = "NOAA Precip Anomaly (%)",
      hover_text_col = "noaa_precip_hover_text",
      legend_title = NULL,
      break_interval = "10 years"
    )
  })
  
  # For McFarland precipitation anomalies
  output$McFarlandPrecipAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = precip_anomaly_data(),
      x_col = "Year-Month",
      y_col = "McFarland Precip Anomaly (%)",
      hover_text_col = "mcfarland_precip_hover_text",
      legend_title = NULL,
      break_interval = "5 years"
    )
  })
  
  # For SERC precipitation anomalies
  output$SERCPrecipAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = precip_anomaly_data(),
      x_col = "Year-Month",
      y_col = "SERC Precip Anomaly (%)",
      hover_text_col = "serc_precip_hover_text",
      legend_title = NULL,
      break_interval = "5 years"
    )
  })
  
  #-----------------------#
  ####  Records Plots  ####
  #-----------------------# 
  
  # function for record highs
  
  create_record_plot <- function(data, 
                                 date_col1,     # Date column for first variable
                                 date_col2 = NULL, # Date column for second variable (optional)
                                 value_col1,    # First value column to plot
                                 value_col2 = NULL, # Second value column to plot (optional)
                                 min_year,      # Minimum year for filtering
                                 max_year,      # Maximum year for filtering
                                 top_n = 10,    # Number of top records to highlight
                                 y_label = "",  # Y-axis label
                                 label_highlight1 = "Top Records (Var1)",  # Custom highlight label for var1
                                 label_other1 = "Other Records (Var1)",    # Custom non-highlight label for var1
                                 label_highlight2 = "Top Records (Var2)",  # Custom highlight label for var2
                                 label_other2 = "Other Records (Var2)",    # Custom non-highlight label for var2
                                 color_top1 = "black",     # Color for top records (var1)
                                 color_other1 = "grey",    # Color for other records (var1)
                                 color_top2 = "darkred",    # Color for top records (var2, optional)
                                 color_other2 = "orange",
                                 show_var1 = TRUE,
                                 show_var2 = TRUE,
                                 units = "°C",
                                 date_format = "%Y-%m") { # Color for other records (var2, optional)
    
    # Filter data based on year range
    filtered_data <- data %>%
      filter(year >= min_year & year <= max_year)
    
    # Process first variable
    data1 <- filtered_data %>%
      arrange(desc(.data[[value_col1]])) %>%
      mutate(
        highlight1 = ifelse(row_number() <= top_n, label_highlight1, label_other1),
        date1 = as.Date(.data[[date_col1]]),
        hover_text1 = sprintf(
          "Date: %s<br>Value: %.2f %s<br>Rank: %d",
          format(date1, date_format),
          .data[[value_col1]],
          units,
          row_number()
        )
      )
    
    # Check if second variable exists
    if (!is.null(date_col2) && !is.null(value_col2)) {
      data2 <- filtered_data %>%
        arrange(desc(.data[[value_col2]])) %>%
        mutate(
          highlight2 = ifelse(row_number() <= top_n, label_highlight2, label_other2),
          date2 = as.Date(.data[[date_col2]]),
          hover_text2 = sprintf(
            "Date: %s<br>Value: %.2f %s<br>Rank: %d",
            format(date2, date_format),
            .data[[value_col2]],
            units,
            row_number()
          )
        )
    }
    
    # Get date range for x-axis
    min_date <- min(data1$date1, na.rm = TRUE)
    max_date <- max(data1$date1, na.rm = TRUE)
    
    if (!is.null(date_col2) && !is.null(value_col2)) {
      min_date <- min(min_date, min(data2$date2, na.rm = TRUE))
      max_date <- max(max_date, max(data2$date2, na.rm = TRUE))
    }
    
    # Create ggplot object
    p <- ggplot() 
    
    # First variable
    if (show_var1){
      p <- p +
        geom_segment(
          data = data1,
          aes(x = date1, xend = date1,
              y = min(.data[[value_col1]], na.rm = TRUE), 
              yend = .data[[value_col1]],
              color = highlight1),
          linetype = "solid", 
          alpha = 0.6
        ) +
        geom_point(
          data = data1,
          aes(x = date1, 
              y = .data[[value_col1]],
              color = highlight1,
              text = hover_text1),
          size = 2
        )
    } 
    # Add second variable if provided
    if (!is.null(date_col2) && !is.null(value_col2) && show_var2) {
      p <- p +
        geom_segment(
          data = data2,
          aes(x = date2, xend = date2,
              y = min(.data[[value_col2]], na.rm = TRUE), 
              yend = .data[[value_col2]],
              color = highlight2),
          linetype = "solid", 
          alpha = 0.6
        ) +
        geom_point(
          data = data2,
          aes(x = date2, 
              y = .data[[value_col2]],
              color = highlight2,
              text = hover_text2),
          size = 2
        )
    }
    
    # Add color scale and labels
    p <- p +
      scale_color_manual(
        values = c(
          setNames(color_top1, label_highlight1),
          setNames(color_other1, label_other1),
          if (!is.null(color_top2)) setNames(color_top2, label_highlight2) else NULL,
          if (!is.null(color_other2)) setNames(color_other2, label_other2) else NULL
        ),
        name = NULL
      ) +
      scale_x_date(
        breaks = seq(
          from = min_date, 
          to = max_date, 
          by = "10 years"
        ),
        labels = scales::date_format("%Y")
      ) +
      labs(
        x = "Year",
        y = y_label
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    # Convert to plotly and disable legend clicking
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(
          itemclick = FALSE, 
          itemdoubleclick = FALSE,
          orientation = "h", 
          x = 0.5, 
          y = -0.2,
          xanchor = "center")
      )
  }
  
  # max temp record plot output
  output$MaxTempRecordsPlot <- renderPlotly({
    create_record_plot(
      data = records.noaa.monthly,
      date_col1 = "tmean.max.ym",    
      date_col2 = "tmax.max.ym",    
      value_col1 = "tmean.max",
      value_col2 = "tmax.max",
      min_year = input$year_range_records[1],
      max_year = input$year_range_records[2],
      top_n = 10,
      y_label = "Monthly average temperature (°C)",
      label_highlight1 = "Top 10 Highest Mean Temperatures",
      label_other1 = "Highest Mean Temperatures",
      label_highlight2 = "Top 10 Highest Max Temperatures",
      label_other2 = "Highest Max Temperatures",
      color_top1 = "black",
      color_other1 = "grey",
      color_top2 = "darkred",
      color_other2 = "orange",
      show_var1 = "mean_max_temp" %in% input$temp_records_display,
      show_var2 = "max_temp" %in% input$temp_records_display,
      units = "°C",
      date_format = "%Y-%m"
    )
  })
  
  # daily max temp record plot output
  output$DailyMaxRecordsPlot <- renderPlotly({
    create_record_plot(
      data = records.noaa.daily,
      date_col1 = "tmean.max.date",    
      date_col2 = "tmax.max.date",    
      value_col1 = "tmean.max",
      value_col2 = "tmax.max",
      min_year = input$year_range_records3[1],
      max_year = input$year_range_records3[2],
      top_n = 10,
      y_label = "Monthly average temperature (°C)",
      label_highlight1 = "Top 10 Highest Mean Temperatures",
      label_other1 = "Highest Mean Temperatures",
      label_highlight2 = "Top 10 Highest Max Temperatures",
      label_other2 = "Highest Max Temperatures",
      color_top1 = "black",
      color_other1 = "grey",
      color_top2 = "darkred",
      color_other2 = "orange",
      show_var1 = "daily_mean_max_temp" %in% input$daily_max_temp_display,
      show_var2 = "daily_max_temp" %in% input$daily_max_temp_display,
      units = "°C",
      date_format = "%Y-%m-%d"
    )
  })
  
  # max precip record plot output
  output$MaxPrecipRecordsPlot <- renderPlotly({
    create_record_plot(
      data = records.noaa.monthly,
      date_col1 = "ppt.max.ym",    
      date_col2 = NULL,    
      value_col1 = "ppt.max",
      value_col2 = NULL,
      min_year = input$year_range_precip[1],
      max_year = input$year_range_precip[2],
      top_n = 10,
      y_label = "Monthly precipitation (in)",
      label_highlight1 = "Top 10 Highest Precipitation Records",
      label_other1 = "Highest Precipitation Records",
      color_top1 = "darkblue",
      color_other1 = "lightblue",
      # show_var1 = "max_precip" %in% input$precip_records_display,
      units = "in",
      date_format = "%Y-%m"
    )
  })
  
  # function for record lows
  
  record_lows <- function(data, 
                          date_col1,     
                          date_col2 = NULL, # Date column for second variable (optional)
                          value_col1,    # First value column to plot
                          value_col2 = NULL, # Second value column to plot (optional)
                          min_year,      # Minimum year for filtering
                          max_year,      # Maximum year for filtering
                          top_n = 10,    # Number of top records to highlight
                          y_label = "",  # Y-axis label
                          label_highlight1 = "Top Records (Var1)",  # Custom highlight label for var1
                          label_other1 = "Other Records (Var1)",    # Custom non-highlight label for var1
                          label_highlight2 = "Top Records (Var2)",  # Custom highlight label for var2
                          label_other2 = "Other Records (Var2)",    # Custom non-highlight label for var2
                          color_top1 = "black",     # Color for top records (var1)
                          color_other1 = "grey",    # Color for other records (var1)
                          color_top2 = "darkblue",    # Color for top records (var2, optional)
                          color_other2 = "light blue",
                          show_var1 = TRUE,
                          show_var2 = TRUE,
                          units = "°C",
                          date_format = "%Y-%m") { 
    
    # Filter data based on year range
    filtered_data <- data %>%
      filter(year >= min_year & year <= max_year)
    
    # Process first variable
    tmean.min <- filtered_data %>%
      arrange(.data[[value_col1]]) %>%   
      mutate(
        highlight1 = ifelse(row_number() <= top_n, label_highlight1, label_other1),
        date.tmean.min = as.Date(.data[[date_col1]]),
        hover_text_mean_min = sprintf(
          "Date: %s<br>Value: %.2f %s<br>Rank: %d",
          format(date.tmean.min, date_format),
          .data[[value_col1]],
          units,
          row_number()
        )
      )
    
    # Process second variable
    if (!is.null(date_col2) && !is.null(value_col2)) {
      tmin.min <- filtered_data %>%
        arrange(.data[[value_col2]]) %>%   
        mutate(
          highlight2 = ifelse(row_number() <= top_n, label_highlight2, label_other2),
          date.tmin.min = as.Date(.data[[date_col2]]),
          hover_text_min = sprintf(
            "Date: %s<br>Value: %.2f %s<br>Rank: %d",
            format(date.tmin.min, date_format),
            .data[[value_col2]],
            units,
            row_number()
          )
        )
    }
    
    # Get date range for x-axis
    min_date <- min(tmean.min$date.tmean.min, na.rm = TRUE)
    max_date <- max(tmean.min$date.tmean.min, na.rm = TRUE)
    
    if (!is.null(date_col2) && !is.null(value_col2)) {
      min_date <- min(min_date, min(tmin.min$tmin.min, na.rm = TRUE))
      max_date <- max(max_date, max(tmin.min$tmin.min, na.rm = TRUE))
    }
    
    # Create ggplot object
    p <- ggplot() 
    
    # First variable
    if (show_var1){
      p <- p +
        geom_segment(
          data = tmean.min,
          aes(x = date.tmean.min, xend = date.tmean.min,
              y = max(.data[[value_col1]], na.rm = TRUE), 
              yend = .data[[value_col1]],
              color = highlight1),
          linetype = "solid", 
          alpha = 0.6
        ) +
        geom_point(
          data = tmean.min,
          aes(x = date.tmean.min, 
              y = .data[[value_col1]],
              color = highlight1,
              text = hover_text_mean_min),
          size = 2
        )
    }
    
    # Add second variable if provided
    if (!is.null(date_col2) && !is.null(value_col2) && show_var2) {
      p <- p +
        geom_segment(
          data = tmin.min,
          aes(x = date.tmin.min, xend = date.tmin.min,
              y = max(.data[[value_col2]], na.rm = TRUE), 
              yend = .data[[value_col2]],
              color = highlight2),
          linetype = "solid", 
          alpha = 0.6
        ) +
        geom_point(
          data = tmin.min,
          aes(x = date.tmin.min, 
              y = .data[[value_col2]],
              color = highlight2,
              text = hover_text_min),
          size = 2
        )
    }
    
    # Add color scale and labels
    p <- p +
      scale_color_manual(
        values = c(
          setNames(color_top1, label_highlight1),
          setNames(color_other1, label_other1),
          if (!is.null(color_top2)) setNames(color_top2, label_highlight2) else NULL,
          if (!is.null(color_other2)) setNames(color_other2, label_other2) else NULL
        ),
        name = NULL
      ) +
      scale_x_date(
        breaks = seq(
          from = min_date, 
          to = max_date, 
          by = "10 years"
        ),
        labels = scales::date_format("%Y")
      ) +
      labs(
        x = "Year",
        y = y_label
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    # Convert to plotly and disable legend clicking
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(
          itemclick = FALSE, 
          itemdoubleclick = FALSE,
          orientation = "h", 
          x = 0.5, 
          y = -0.2,
          xanchor = "center")
      )
  }
  
  # min temp record plot output
  output$MinTempRecordsPlot <- renderPlotly({
    record_lows(
      data = records.noaa.monthly,
      date_col1 = "tmean.min.ym",    
      date_col2 = "tmin.min.ym",    
      value_col1 = "tmean.min",
      value_col2 = "tmin.min",
      min_year = input$year_range_records2[1],
      max_year = input$year_range_records2[2],
      top_n = 10,
      y_label = "Monthly average temperature (°C)",
      label_highlight1 = "Top 10 Lowest Mean Temperatures",
      label_other1 = "Lowest Mean Temperatures",
      label_highlight2 = "Top 10 Lowest Minimum Temperatures",
      label_other2 = "Lowest Minimum Temperatures",
      color_top1 = "black",
      color_other1 = "grey",
      color_top2 = "darkblue",
      color_other2 = "lightblue",
      show_var1 = "mean_min_temp" %in% input$min_temp_records_display,
      show_var2 = "min_temp" %in% input$min_temp_records_display,
      units = "°C",
      date_format = "%Y-%m"
    )
  })
  
  # daily min temp record plot output
  output$DailyMinRecordsPlot <- renderPlotly({
    record_lows(
      data = records.noaa.daily,
      date_col1 = "tmean.min.date",    
      date_col2 = "tmin.min.date",    
      value_col1 = "tmean.min",
      value_col2 = "tmin.min",
      min_year = input$year_range_records4[1],
      max_year = input$year_range_records4[2],
      top_n = 10,
      y_label = "Daily average temperature (°C)",
      label_highlight1 = "Top 10 Lowest Mean Temperatures",
      label_other1 = "Lowest Mean Temperatures",
      label_highlight2 = "Top 10 Lowest Min Temperatures",
      label_other2 = "Lowest Min Temperatures",
      color_top1 = "black",
      color_other1 = "grey",
      color_top2 = "darkblue",
      color_other2 = "lightblue",
      show_var1 = "daily_mean_min_temp" %in% input$daily_min_temp_display,
      show_var2 = "daily_min_temp" %in% input$daily_min_temp_display,
      units = "°C",
      date_format = "%Y-%m-%d"
    )
  })
  
  # min precip record plot output
  output$MinPrecipRecordsPlot <- renderPlotly({
    record_lows(
      data = records.noaa.monthly,
      date_col1 = "ppt.min.ym",    
      date_col2 = NULL,    
      value_col1 = "ppt.min",
      value_col2 = NULL,
      min_year = input$year_range_precip[1],
      max_year = input$year_range_precip[2],
      top_n = 10,
      y_label = "Monthly precipitation (in)",
      label_highlight1 = "Top 10 Lowest Precipitation Records",
      label_other1 = "Lowest Precipitation Records",
      color_top1 = "black",
      color_other1 = "grey",
      # show_var1 = "min_precip" %in% input$precip_records_display,
      units = "in",
      date_format = "%Y-%m"
    )
  })
  
  
  #---------------------------------#
  ####  Sea Level Plot Function  ####
  #---------------------------------#  
  
  # function for mean sea level plots
  create_sea_level_plot <- function(data,
                                    x_col = "Year-Month",
                                    y_col = "Monthly Mean Sea Level (mm)",
                                    hover_text_col = "monthly_sea_hover_text",
                                    plot_title = NULL,
                                    show_trend = FALSE,
                                    model = NULL,
                                    is_date = TRUE,
                                    line_color = "#000000",
                                    line_label = "Sea Level",
                                    input_check) {
    
    
    # Create base plot
    s <- ggplot(data) +
      labs(title = plot_title,
           x = "Year",
           y = y_col) +
      theme_minimal()
    
    # Add main sea level line ONLY if it is selected
    if (line_label %in% input_check) {
      s <- s + geom_line(aes(
        x = .data[[x_col]],
        y = .data[[y_col]],
        text = .data[[hover_text_col]],
        color = line_label,
        group = 1  # Ensures correct grouping for Plotly
      ), size = 0.3)
    }
    
    # Add appropriate scale based on x-axis type
    if (is_date) {
      s <- s + scale_x_date(
        breaks = scales::breaks_width("10 years"),
        labels = scales::date_format("%Y")
      )
    } else {
      s <- s + scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10)
      )
    }
    
    # Add color scale
    s <- s + scale_color_manual(
      values = c(
        setNames(line_color, line_label)
      ),
      name = NULL
    )
    
    # Add trend line if requested
    if (show_trend && !is.null(model)) {
      s <- s + 
        geom_smooth(
          aes(x = .data[[x_col]], y = .data[[y_col]]),
          method = "lm",
          se = TRUE,
          fill = "grey80",
          alpha = 0.5,
          color = NA
        ) +
        geom_line(
          aes(x = .data[[x_col]], y = .data[[y_col]]),
          stat = "smooth",
          method = "lm",
          color = "black",
          linewidth = 0.8
        )
    }
    
    # Convert to plotly and customize
    ggplotly(s, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(
          itemclick = FALSE, 
          itemdoubleclick = FALSE,
          orientation = "h", 
          x = 0.5, 
          y = -0.2,
          xanchor = "center"),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white")
      )
  }
  
  # Use the function in your outputs
  output$MonthlySeaLevel <- renderPlotly({
    data <- monthly_sea_level_data()
    models <- monthly_sea_model()
    
    create_sea_level_plot(
      data = data,
      x_col = "Year-Month",
      y_col = "Monthly Mean Sea Level (mm)",
      hover_text_col = "monthly_sea_hover_text",
      plot_title = NULL,
      show_trend = "lm_monthly_sea" %in% input$linesToShowMonthlySea,
      model = models$monthly_sea,
      is_date = TRUE,
      line_color = "blue",
      line_label = "Monthly Mean Sea Level (mm)",
      input_check = input$linesToShowMonthlySea
    )
  })
  
  output$AnnualSeaLevel <- renderPlotly({
    data <- annual_sea_level_data()
    models <- annual_sea_model()
    
    create_sea_level_plot(
      data = data,
      x_col = "Year",
      y_col = "Annual Mean Sea Level (mm)",
      hover_text_col = "annual_sea_hover_text",
      plot_title = NULL,
      show_trend = "lm_annual_sea" %in% input$linesToShowAnnualSea,
      model = models$annual_sea,
      is_date = FALSE,
      line_color = "blue",
      line_label = "Annual Mean Sea Level (mm)",
      input_check = input$linesToShowAnnualSea
    )
  })
  
  # sea level model summaries --------------------------------------------------
  
  output$monthly_sea_model_summary <- renderPrint({
    req("lm_monthly_sea" %in% input$linesToShowMonthlySea)
    summary(monthly_sea_model()$monthly_sea)
  })
  
  output$annual_sea_model_summary <- renderPrint({
    req("lm_annual_sea" %in% input$linesToShowAnnualSea)
    summary(annual_sea_model()$annual_sea)
  })
  
}



#--------------------------------------#
####          RUN THE APP           ####
#--------------------------------------#

shinyApp(ui, server)

