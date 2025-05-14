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
              "This dashboard summarizes climate data from Acadia National Park gathered from local weather stations and modeled data that covers the park. The goal of this dashboard is to make it easier to access and view local climate, weather, and sea level data and trends over time. We also try to emphasize extreme events and allow for the comparison to historical records."),
            div(class = "header-update",
                p(class = "updatep", HTML("<i>Updated: March 2025</i>"))
            ),
        ),
    ),
    
    
    ### Tab Content
    navset_card_underline(selected = "Precipitation",
      
      ## ¬ Dashboard Overview ----
      nav_panel(icon = icon("home"), "Dashboard Overview",
                
                div(class = "summary-box",
                    div(class = "sumtxt",
                        h2("About this Dashboard"),
                        p("This dashboard summarizes climate data from Acadia National Park gathered from local weather stations and climate models that cover the park. Local data was gathered from the McFarland Hill Atmospheric Research Station in Bar Harbor, the MesoWest Winter Harbor-SERC Station at Schoodic Point, and the National Oceanography Centre (NOC) Station in Frenchman Bay. We also use the National Oceanic and Atmospheric Administration (NOAA) gridded climate model data for long term trends. These data are compiled and cleaned to produce visualizations of temperature and precipitation long-term trends, anomalies, and extremes as well as long-term sea level trends. The dashboard design was inspired by the Organisation for Economic Co-operation and Development's Climate Action Dashboard.")),
                    tags$img(class = "sumimg",
                             src = "img/IMG_2172.png",
                             alt = "A twig sticking out of the snow in the forest with the afternoon sun in the background.")
                ),
                
                div(class = "map-box",
                    div(class = "maptxt",
                        h2("Station Locations"),
                        p("Explore the map to see the locations of the weather stations that we have included data from in this dashboard. Click on a marker to see the station name and coordinates.")),
                    
                    div(class = "maploc",
                        leafletOutput("LocationMap"))
                ),
                
                div(
                  h2(class = "wstxt", "About the Data"),
                  tags$ul(
                    tags$li(HTML('Climate summaries were created from daily and monthly gridded climate data (NClimGrid Daily and Monthly) downloaded from NOAA\'s National Centers for Environmental Information (<a href="https://www.ncei.noaa.gov" target="_blank">NCEI</a>).')),
                    tags$li(HTML('Climate data was compiled and cleaned using R scripts by Kyle Lima, built from the <a href="https://github.com/KateMMiller/climateNETN" target="_blank">climateNETN package</a> by Kate Miller.')),
                    tags$li("Climate summaries were created from hourly data collected by the McFarland Hill Atmospheric Research Station."),
                    tags$li("Climate summaries were created from 15 minute interval data collected by the MesoWest Winter Harbor-SERC station (ID: D2258)."),
                    tags$li(HTML('Sea level trend visualizations were created from monthly and annual mean sea level data collected by the NOC Station in Frenchman Bay (ID: 525) and documented by the Permanent Service for Mean Sea Level (<a href="https://psmsl.org/" target="_blank">PSMSL</a>), based at the NOC, which specializes in providing global tide gauge data.'))),
                ),
                
                div(
                  h2(class = "wstxt", "Data Access"),
                  p(HTML('Data from all sources used in this app and R scripts for data compiling and cleaning can be downloaded following the instructions on the <a href="https://kylelima21.github.io/acadia_climate_dashboard/" target="_blank">data download page</a>. Instructions for data downloading and R script use are also provided.')),
                ),
                
                div(
                  h2(class = "wstxt", "Contact"),
                  p(HTML('Found an error, have questions, or want to connect? Contact us at klima@schoodicinstitute.org.')),
                )
      ),
      
      
      ## ¬ Temperature Trends ----
      nav_panel(icon = icon("thermometer-half"), "Temperature",
                
                div(class = "summary-box add-marg",
                    div(class = "sumtxt",
                        h2("Temperature Trends, Anomolies, and Extremes"),
                        p(HTML("Maine's average annual temperature has increased by over 3.2 ˚F since 1895 and we've 
                        seen a drastic increase in the rate of warming since 1960 (<a href='https://climatechange.umaine.edu/climate-matters/maines-climate-future/' target='_blank'>Fernandez et al., 2020</a>). Additionally, 
                        the ten warmest years on record have all occurred since 1998. In this section,
                        you can explore these temperature trends, visualize anomalies, and identify temperature records 
                        from here in Acadia National Park."))),
                    tags$img(class = "sumimg",
                             src = "img/acad_back.jpg",
                             alt = "The hot summer sun setting near Cadillac Mountain.")
                    ),
                
                div(class = "trend-box add-p-marg",
                    div(class = "lt-title",
                        h2("Long-term Annual Temperature Trends"),
                        p("View annual average maximum, minimum, and mean temperature trends from 1895 to 2024 for 
                          data derived from NOAA NClimGrid datasets. Also included are McFarland Hill annual average 
                          temperature data which spans from 1999 to 2024 and SERC annual average temperature data which spans from 2009 to 2024. 
                          The data filtering tools can be used to add or remove elements from the plot. If linear models are added, 
                          the corresponding model statistics are calculated and provided in the model statistics box below.")
                    ),
                    
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
                                      "Linear Model for NOAA Average Max Temp." = "lm_noaa_max_temp",
                                      "Linear Model for NOAA Average Mean Temp." = "lm_noaa_temp",
                                      "Linear Model for NOAA Average Min Temp." = "lm_noaa_min_temp",
                                      "Linear Model for McFarland Average Temp." = "lm_mcfarland_temp",
                                      "Linear Model for SERC Average Temp." = "lm_serc_temp"),
                          selected = c("NOAA Average Mean Temp", "NOAA Average Max Temp", "NOAA Average Min Temp", "McFarland Average Temp", "SERC Average Temp")),
    
                          div(class = "slider",
                            sliderInput(
                              inputId = "year_range_temp",
                              label = "Select year range:",
                              min = min(temp.data.merged$year),
                              max = max(temp.data.merged$year),
                              value = c(min(temp.data.merged$year), max(temp.data.merged$year)),
                              sep = "")),
                    ),
                    
                    div(class = "plot-t-box fig-boxes",
                        plotlyOutput("myInteractivePlot")
                    )
                ),
                
                div(class = "anom-box add-p-marg",
                    div(class = "lt-title",
                        h2("Temperature Anomalies"),
                        p("Plotted below are monthly temperature anomalies which represent the difference between observed temperatures and historic baseline temperatures. Positive anomalies (above the baseline in red) indicate temperatures that are above (warmer than) the historic baseline. Negative anomalies (below the baseline in blue) indicate temperatures that are below (cooler than) the historic baseline. 
                        NOAA monthly temperature anomalies were calculated for 1895 to 2024. The baseline was calculated by averaging the mean temperature for each month of the year from 1901-2000 to generate a 20th century baseline. Data was derived from the NOAA NClimGrid Monthly dataset for these calculations and anomaly plot visualization.
                        McFarland Hill monthly temperature anomalies were calculated for 1999 to 2024. The baseline was calculated from the first 20 years of this dataset.
                        SERC monthly temperature anomalies were calculated for 2009 to 2024. The baseline was calculated from the full span of this dataset.")
                    ),
                    
                    div(class = "leg-t-box anom-slider",
                        h3("Data Filtering Tools"),
                        div(class = "slider",
                          sliderInput(
                            inputId = "year_range_temp_anom",
                            label = "Select year range:",
                            min = min(anom.temp.merged$year),
                            max = max(anom.temp.merged$year),
                            value = c(min(anom.temp.merged$year), max(anom.temp.merged$year)),
                            sep = ""))
                    ),
                    
                    div(class = "plot-t-box fig-boxes noaa",
                        plotlyOutput("NOAAAnomPlot")
                    ),
                      
                    div(class = "plot-t-box fig-boxes mcfar",
                      plotlyOutput("McFarlandAnomPlot")
                    ),
                      
                    div(class = "plot-t-box fig-boxes serc",
                      plotlyOutput("SERCAnomPlot")
                    )
                ),
                
                div(class = "xtreme-box add-p-marg",
                    div(class = "lt-title",
                        h2("Temperature Extremes"),
                        p("Explore monthly and daily temperature records and extremes including hottest monthly and daily temperature records of each year and 
                        coldest monthly and daily temperature records of each year. Default plots show the records for the hottest mean monthly and daily temperatures, but
                        you can see the true maximum and minum temperatures by selecting the box in the data filtering tools. Data are derived from NOAA NClimGrid datasets.")
                    ),
                    
                    
                    div(class = "rainnav",
                        navset_card_underline(

                          nav_panel("Annual",

                                    div(class = "plot-t-box fig-boxes urecs",
                                        create_temp_records_panel(
                                          # Annual maximum temps
                                          list(
                                            list(
                                              year_range_id = "year_range_records5",
                                              checkbox_id = "annual_temp_records_display",
                                              plot_id = "AnnualRecordsPlot",
                                              data_source = temp.data.merged,
                                              checkbox_choices = c(
                                                "Highest Annual Mean Temperature Records" = "annual_mean_temp"
                                              ),
                                              default_selected = c("annual_mean_temp")
                                            )
                                          )
                                    )
                                    )
                          ),

                          nav_panel("Monthly",

                                    div(class = "plot-t-box fig-boxes urecs",
                                        create_temp_records_panel(
                                          list(
                                            # Monthly maximum temps
                                            list(
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
  
                                            # Monthly minimum temps
                                            list(
                                              year_range_id = "year_range_records2",
                                              checkbox_id = "min_temp_records_display",
                                              plot_id = "MinTempRecordsPlot",
                                              data_source = records.noaa.monthly,
                                              checkbox_choices = c(
                                                "Lowest Monthly Mean Temperature Records" = "mean_min_temp",
                                                "Lowest Monthly Minimum Temperature Records" = "min_temp"
                                              ),
                                              default_selected = c("mean_min_temp")
                                            )
                                          )
                                        )
                                        )

                          ),
                          
                          nav_panel("Daily",
                                    
                                    div(class = "plot-t-box fig-boxes urecs",
                                        create_temp_records_panel(
                                          list(
                                            # Daily maximum temps
                                            list(
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
                                            
                                            # Daily manimum temps
                                            list(
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
                          
                          
                          
                          
                        )


                    ),
                    
                    # div(class = "plot-t-box fig-boxes urecs",
                    #     
                    #     create_temp_records_panel(
                    #       # First plot (annual maximum temperatures)
                    #       list(
                    #         list(
                    #           year_range_id = "year_range_records5",
                    #           checkbox_id = "annual_temp_records_display",
                    #           plot_id = "AnnualRecordsPlot",
                    #           data_source = temp.data.merged,
                    #           checkbox_choices = c(
                    #             "Highest Annual Mean Temperature Records" = "annual_mean_temp"
                    #           ),
                    #           default_selected = c("annual_mean_temp")
                    #         ),
                    #         
                    #         # Second plot (annual minimum temperatures)
                    #         # list(
                    #         #   year_range_id = "year_range_records6",
                    #         #   checkbox_id = "annual_low_records_display",
                    #         #   plot_id = "AnnualLowRecordsPlot",
                    #         #   data_source = temp.data.merged,
                    #         #   checkbox_choices = c(
                    #         #     "Lowest Annual Mean Temperature Records" = "annual_low_temp"
                    #         #   ),
                    #         #   default_selected = c("annual_low_temp")
                    #         # ),
                    #         
                    #         # Third plot (monthly maximum temperatures)
                    #         list(
                    #           year_range_id = "year_range_records",
                    #           checkbox_id = "temp_records_display",
                    #           plot_id = "MaxTempRecordsPlot",
                    #           data_source = records.noaa.monthly,
                    #           checkbox_choices = c(
                    #             "Highest Monthly Mean Temperature Records" = "mean_max_temp",
                    #             "Highest Monthly Maximum Temperature Records" = "max_temp"
                    #           ),
                    #           default_selected = c("mean_max_temp")
                    #         ),
                    #         
                    #         # Fourth plot (monthly minimum temperatures)
                    #         list(
                    #           year_range_id = "year_range_records2",
                    #           checkbox_id = "min_temp_records_display",
                    #           plot_id = "MinTempRecordsPlot",
                    #           data_source = records.noaa.monthly,
                    #           checkbox_choices = c(
                    #             "Lowest Monthly Mean Temperature Records" = "mean_min_temp",
                    #             "Lowest Monthly Minimum Temperature Records" = "min_temp"
                    #           ),
                    #           default_selected = c("mean_min_temp")
                        #     ),
                        # 
                        #     # Fifth plot (daily maximum temperatures)
                        #     list(
                        #       year_range_id = "year_range_records3",
                        #       checkbox_id = "daily_max_temp_display",
                        #       plot_id = "DailyMaxRecordsPlot",
                        #       data_source = records.noaa.daily,
                        #       checkbox_choices = c(
                        #         "Highest Daily Mean Temperature Records" = "daily_mean_max_temp",
                        #         "Highest Daily Maximum Temperature Records" = "daily_max_temp"
                        #       ),
                        #       default_selected = c("daily_mean_max_temp")
                        #     ),
                        # 
                        #     # Sixth plot (daily maximum temperatures)
                        #     list(
                        #       year_range_id = "year_range_records4",
                        #       checkbox_id = "daily_min_temp_display",
                        #       plot_id = "DailyMinRecordsPlot",
                        #       data_source = records.noaa.daily,
                        #       checkbox_choices = c(
                        #         "Lowest Daily Mean Temperature Records" = "daily_mean_min_temp",
                        #         "Lowest Daily Minimum Temperature Records" = "daily_min_temp"
                        #       ),
                        #       default_selected = c("daily_mean_min_temp")
                        #     )
                        #   )
                        # )
                    # )
                )
                
      ),
      
      
      ## ¬ Precip Trends ----
      nav_panel(icon = icon("cloud-rain"), "Precipitation",
                
                div(class = "summary-box add-marg",
                    div(class = "sumtxt",
                        h2("Precipitation Trends, Anomolies, and Extremes"),
                        p(HTML("Average annual precipitation has increased since the late 1800s by almost 6 inches (<a href='https://climatechange.umaine.edu/climate-matters/maines-climate-future/' target='_blank'>Fernandez et al., 2020</a>).
                          We are seeing more rain and less snow, yet this increased rain is coming in fewer, but more
                          extreme rainfall events. In this section, you can explore precipitation trends, visualize anomalies,
                          and identify rain or snowfall records in Acadia National Park."))),
                    tags$img(class = "sumimg",
                             src = "img/rain_cropped.jpg",
                             alt = "Rain falling against a car windshield.")
                ),
                
                div(class = "trend-box add-p-marg",
                    div(class = "lt-title",
                        h2("Long-term Annual Precipitation Trends"),
                        p("Explore annual average total precipitation trends from 1895 to 2024 for data derived 
                          from NOAA NClimGrid datasets. Also plotted are McFarland Hill annual average total precipitation 
                          data which spans from 1999 to 2024 and SERC annual average total precipitation data which spans from 2009 to 2024. 
                          The data tools can be used to add or remove elements from the plot. If linear models are added, the corresponding 
                          model statistics are calculated and provided in the model statistics box below.")
                    ),
                    
                    div(class = "leg-t-box",
                        h3("Data Filtering Tools"),
                        checkboxGroupInput(
                          inputId = "linesToShowPrecip",
                          label = "Select data to display:",
                          choices = c("NOAA Total Precip." = "NOAA Precip",
                                      "McFarland Total Precip." = "McFarland Precip",
                                      "SERC Total Precip." = "SERC Precip",
                                      "Linear Model for NOAA Precip" = "lm_noaa_precip",
                                      "Linear Model for McFarland Precip" = "lm_mcfarland_precip",
                                      "Linear Model for SERC Precip" = "lm_serc_precip"),
                          selected = c("NOAA Precip", "McFarland Precip", "SERC Precip")),
                        
                        div(class = "slider",
                            sliderInput(
                              inputId = "year_range_precip",
                              label = "Select year range:",
                              min = min(precip.data.merged$year),
                              max = max(precip.data.merged$year),
                              value = c(min(precip.data.merged$year), max(precip.data.merged$year)),
                              sep = "")),
                        ),
                
                    div(class = "plot-t-box fig-boxes",
                        plotlyOutput("PrecipPlot"),
                    ),
                ),
                
                
                div(class = "anom-box",
                    div(class = "lt-title",
                        h2("Precipitation Anomalies"),
                        p("Plotted below are monthly precipitation anomalies which represent the percent difference between observed precipitation and historic baseline precipitation totals. Positive anomalies (red) indicate precipitation totals that are higher or wetter than average conditions, while negative anomalies (blue) indicate precipitation totals that are lower or drier than average conditions.
                        NOAA precipitation anomalies were calculated for 1895 to 2024. The baseline was calculated by averaging the total precipitation for each month of the year from 1901-2000 to generate a 20th century baseline. Data was derived from the Monthly NOAA NClimGrid dataset for these calculations and anomaly plot visualization.
                        McFarland Hill precipitation anomalies were calculated for 1999 to 2024; the baseline was calculated from the first 20 years of this dataset.
                        SERC precipitation anomalies were calculated for 2009 to 2024; the  baseline was calcualted from the full span of this dataset.")
                    ),
                    
                    div(class = "leg-t-box anom-slider",
                        h3("Data Filtering Tools"),
                        div(class = "slider",
                            sliderInput(
                              inputId = "year_range_precip_anom",
                              label = "Select year range:",
                              min = min(anom.precip.merged$year),
                              max = max(anom.precip.merged$year),
                              value = c(min(anom.precip.merged$year), max(anom.precip.merged$year)),
                              sep = "")),
                    ),
                    
                    div(class = "plot-t-box fig-boxes noaa",
                        plotlyOutput("NOAAPrecipAnomPlot")
                    ),
                    
                    div(class = "plot-t-box fig-boxes mcfar",
                        plotlyOutput("McFarlandPrecipAnomPlot")
                    ),
                    
                    div(class = "plot-t-box fig-boxes serc",
                        plotlyOutput("SERCPrecipAnomPlot")
                    )
                ),
                
                div(class = "xtreme-box-rain add-p-marg",
                    div(class = "lt-title",
                        h2("Precipitation Extremes"),
                        p("Plotted below are the highest monthly precipitation records of each year and the lowest monthly precipitation records of each year. Data are derived from NOAA NClimGrid datasets.")
                    ),
                    
                    div(class = "leg-t-box xtreme-slider",
                        h3("Data Filtering Tools"),
                        div(class = "slider",
                            sliderInput(
                              inputId = "year_range_precip_record",
                              label = "Select year range:",
                              min = min(records.noaa.monthly$year),
                              max = max(records.noaa.monthly$year),
                              value = c(min(records.noaa.monthly$year), max(records.noaa.monthly$year)),
                              sep = "")),
                    ),
                    
                    
                    div(class = "rainnav",
                    navset_card_underline(
                      
                      nav_panel("Annual",
                                
                                div(class = "plot-t-box fig-boxes",
                                    plotlyOutput("AnnualPrecipRecordsPlot"),
                                    plotlyOutput("AnnualDroughtRecordsPlot")),
                                ),
                      
                      
                      nav_panel("Monthly",
                                
                                div(class = "plot-t-box fig-boxes thehigh",
                                    plotlyOutput("MaxPrecipRecordsPlot"),
                                    plotlyOutput("MinPrecipRecordsPlot")),
                                ),
                    )
                    )
                )
      ),
      
      
      ## ¬ Sea Level Trends ----
      nav_panel(icon = icon("water"), "Sea Level",
                
                div(class = "summary-box add-marg",
                    div(class = "sumtxt",
                        h2("Sea Level Trends"),
                        p(HTML("Acadia National Park has experienced about 7.5 inches of sea level rise over the last century (<a href='https://climatechange.umaine.edu/climate-matters/maines-climate-future/' target='_blank'>Fernandez et al., 2020</a>) 
                          which has led to freuquent nuisance flooding, rapid coastal eriosion, and sometimes severe damage to homes and infrastructure
                          during storms. Here you can explore the trends in sea level in this region. If you'd like to help monitor sea level rise,
                          consider contributing to the <a href='https://www.anecdata.org/projects/view/59/about' target='_blank'>Gulf of Maine King Tides</a> project"))),
                    tags$img(class = "sumimg",
                             src = "img/PXL_20240110_143245249.jpg",
                             alt = "Waves washing over a road during a storm.")
                ),
                
                div(class = "trend-box add-p-marg",
                    div(class = "lt-title",
                        h2("Long-term Annual Sea Level Trend"),
                        p("Explore annual sea level trends from 1948 to 2024 from the NOC Station in Frenchman Bay. Data are derived from the PSMSL. The data filtering tools can be used to add or remove elements from the plots; if linear models are added, the corresponding model statistics are calculated and provided in the model statistics boxes below the plots.")
                    ),
                    
                    div(class = "leg-t-box",
                        h3("Data Filtering Tools"),
                        checkboxGroupInput(
                          inputId = "linesToShowAnnualSea",
                          label = "Select data to display:",
                          choices = c("Annual Mean Sea Level (mm)" = "Annual Mean Sea Level (mm)",
                                      "Linear Model for Annual Sea Level" = "lm_annual_sea"),
                          selected = c("Annual Mean Sea Level (mm)")),
                        
                        div(class = "slider",
                          sliderInput(
                            inputId = "year_range_annual_sea_level",
                            label = "Select year range:",
                            min = min(frenchman.annual.clean$year),
                            max = max(frenchman.annual.clean$year),
                            value = c(min(frenchman.annual.clean$year), max(frenchman.annual.clean$year)),
                            sep = "")),
                    ),
                    
                    div(class = "plot-t-box fig-boxes",
                        plotlyOutput("AnnualSeaLevel")
                    )
                    
                ),
                
                
                div(class = "trend-box add-p-marg",
                    div(class = "lt-title",
                        h2("Long-term Monthly Sea Level Trend"),
                        p("Explore monthly sea level trends from 1948 to 2024 from the NOC Station in Frenchman Bay. Data are derived from the PSMSL. The data filtering tools can be used to add or remove elements from the plots; if linear models are added, the corresponding model statistics are calculated and provided in the model statistics boxes below the plots.")
                    ),
                    
                    div(class = "leg-t-box",
                        h3("Data Filtering Tools"),
                        checkboxGroupInput(
                          inputId = "linesToShowMonthlySea",
                          label = "Select data to display:",
                          choices = c("Monthly Mean Sea Level (mm)" = "Monthly Mean Sea Level (mm)",
                                      "Linear Model for Monthly Sea Level" = "lm_monthly_sea"),
                          selected = c("Monthly Mean Sea Level (mm)")),
                        
                        div(class = "slider",
                          sliderInput(
                            inputId = "year_range_monthly_sea_level",
                            label = "Select year range:",
                            min = min(frenchman.monthly.clean$year),
                            max = max(frenchman.monthly.clean$year),
                            value = c(min(frenchman.monthly.clean$year), max(frenchman.monthly.clean$year)),
                            sep = "")),
                        ),
                    
                    div(class = "plot-t-box fig-boxes",
                      plotlyOutput("MonthlySeaLevel", height = "600px")
                    ),
                ),
                
      ),
      
      
      ## ¬ Custom Data Exploration ----
      nav_panel(icon = icon("calendar"), "Custom Exploration",
                
                div(class = "summary-box add-marg",
                    div(class = "sumtxt",
                        h2("Explore and Compare Dates"),
                        p("This dashboard")),
                    tags$img(class = "sumimg",
                             src = "img/cadillac_from_schoodic.jpg",
                             alt = "Cadillac Mountain in the distance on a sunny dat through red spruce trees taken from a rocky outcropping on Schoodic Head.")
                ),
                
                div(class = "trend-box add-p-marg",
                    div(class = "lt-title",
                        h2("Date Look-up"),
                        p("Enter a date to start.")
                    ),
                ),
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
              p(HTML("<a href='https://kylelima21.github.io/acadia_climate_dashboard/' target='_blank'>Download and clean data</a>")),
            ),
            div(
              h1("Data Providers"),
              p(HTML("<a href='https://www.ncei.noaa.gov/' target='_blank'>NOAA National Centers for Environmental Information</a>")),
              p(HTML("<a href='https://ard-request.air-resource.com/data.aspx' target='_blank'>NPS McFarland Hill Atmospheric Research Station</a>")),
              p(HTML("<a href='https://mesowest.utah.edu/cgi-bin/droman/meso_base_dyn.cgi?stn=D2258' target='_blank'>MesoWest SERC weather station</a>")),
              p(HTML("<a href='https://psmsl.org/data/obtaining/stations/525.php' target='_blank'>NOC Frenchman Bay tide gauge</a>")),
            ),
            div(
              h1("About"),
              p(HTML("<a href='https://github.com/Kylelima21/acadia_climate_dashboard' target='_blank'>Source code</a>")),
              p(HTML("<a href='https://schoodicinstitute.org/' target='_blank'>Visit our website</a>")),
              p(HTML("<a href='mailto:klima@schoodicinstitute.org' target='_blank'>Contact us</a>")),
            ),
            
            # div(
            #   h1("Source Code"),
            #   p(HTML("<a href='https://github.com/Kylelima21/acadia_climate_dashboard' target='_blank'>GitHub repository</a>")),
            # ),
            # div(
            #   h1("Website"),
            #   p(HTML("<a href='https://schoodicinstitute.org/' target='_blank'>Visit our website</a>")),
            # ),
            # div(
            #   h1("Contact Us"),
            #   p("klima@schoodicinstitute.org"),
            # ),
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
      name = c("McFarland Hill Atmospheric Research Station (Bar Harbor, ME)", 
               "MesoWest Winter Harbor-SERC Station (Schoodic Point, Winter Harbor, ME)", 
               "NOAA Gridded Climate Data (Acadia National Park, ME)",
               "NOC Station (Frenchman Bay, Bar Harbor, ME)"),
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
        linewidth = 0.5,
        linetype = "solid"
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
      scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14)) +
      scale_x_continuous(breaks = pretty(filtered_data$Year)) +
      labs(title = NULL,
           x = "Year",
           y = "Temperature (°C)") +
      theme_classic()
      #theme_minimal() #+
      #theme(axis.text = element_text(size = "1rem"),
            #axis.title = element_text(size = "1rem", margin = unit(c(0, 0, 5, 0), "mm")),
            #legend.text = element_text(size = "1rem"))
    
    # Add temperature lines based on selection
    #add noaa max temp
    if ("NOAA Average Max Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `NOAA Average Max Temp`,
                             color = "NOAA Average Maximum Temp.",
                             linetype = "NOAA Average Maximum Temp."))
      
      if (!is.null(models$noaa_max)) {
        p <- add_model_line(p, models$noaa_max, "NOAA Average Max Temp")
        
      }
    }
    
    #add noaa average temp
    if ("NOAA Average Mean Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `NOAA Average Mean Temp`,
                             color = "NOAA Average Mean Temp.",
                             linetype = "NOAA Average Mean Temp."))
      
      if (!is.null(models$noaa_avg)) {
        p <- add_model_line(p, models$noaa_avg, "NOAA Average Mean Temp")
        
      }
    }
    
    #add noaa min temp
    if ("NOAA Average Min Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `NOAA Average Min Temp`,
                             color = "NOAA Average Minimum Temp.",
                             linetype = "NOAA Average Minimum Temp."))
      
      if (!is.null(models$noaa_min)) {
        p <- add_model_line(p, models$noaa_min, "NOAA Average Min Temp")
        
      }
    }
    
    #add McFarland temp
    if ("McFarland Average Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `McFarland Average Temp`,
                             color = "McFarland Average Temp.",
                             linetype = "McFarland Average Temp."))
      
      if (!is.null(models$mcfarland)) {
        p <- add_model_line(p, models$mcfarland, "McFarland Average Temp")
        
      }
    }
    
    #add SERC temp
    if ("SERC Average Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `SERC Average Temp`,
                             color = "SERC Average Temp.",
                             linetype = "SERC Average Temp."))
      
      if (!is.null(models$serc)) {
        p <- add_model_line(p, models$serc, "SERC Average Temp")
        
      }
    }
    
    # Customize the legend and colors
    p <- p + scale_color_manual(
      values = c(
        "NOAA Average Mean Temp." = "gray50", 
        "NOAA Average Maximum Temp." = "#CC3300", 
        "NOAA Average Minimum Temp." = "#003399", 
        "McFarland Average Temp." = "black",
        "SERC Average Temp." = "black"),
      name = NULL) +
      scale_linetype_manual(
        values = c(
          "NOAA Average Mean Temp." = "solid", 
          "NOAA Average Maximum Temp." = "solid", 
          "NOAA Average Minimum Temp." = "solid", 
          "McFarland Average Temp." = "dashed",
          "SERC Average Temp." = "dotted"),
        name = NULL)
    
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
      theme_classic()
    
    # Add precipitation lines based on selection
    #add noaa precip data
    if ("NOAA Precip" %in% input$linesToShowPrecip) {
      p2 <- p2 + geom_line(aes(x = Year,
                               y = `NOAA Precip`,
                               color = "NOAA Total Precip.",
                               linetype = "NOAA Total Precip."))
      
      if (!is.null(models$noaa_precip)) {
        p2 <- add_model_line(p2, models$noaa_precip, "NOAA Precip")
        
      }
    }
    
    #add McFarland precip data
    if ("McFarland Precip" %in% input$linesToShowPrecip) {
      p2 <- p2 + geom_line(aes(x = Year,
                               y = `McFarland Precip`,
                               color = "McFarland Total Precip.",
                               linetype = "McFarland Total Precip."))
      
      if (!is.null(models$mcfarland_precip)) {
        p2 <- add_model_line(p2, models$mcfarland_precip, "McFarland Precip")
        
      }
    }
    
    #add SERC precip data
    if ("SERC Precip" %in% input$linesToShowPrecip) {
      p2 <- p2 + geom_line(aes(x = Year,
                               y = `SERC Precip`,
                               color = "SERC Average Precip.",
                               linetype = "SERC Average Precip."))
      
      if (!is.null(models$serc_precip)) {
        p2 <- add_model_line(p2, models$serc_precip, "SERC Precip")
        
      }
    }
    
    # Customize the legend and colors
    p2 <- p2 + scale_color_manual(
      values = c(
        "NOAA Total Precip." = "#000000", 
        "McFarland Total Precip." = "darkblue",
        "SERC Average Precip." = "gray30"
      ),
      name = NULL) +
      scale_linetype_manual(
        values = c(
          "NOAA Total Precip." = "solid", 
          "McFarland Total Precip." = "dashed",
          "SERC Average Precip." = "dotted"),
        name = NULL)
    
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
        hoverlabel = list(bgcolor = "white")
        #xaxis = list(hoverformat = "%Y")
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
  
  ## Create anomaly plot function
  create_anomaly_plot <- function(data, 
                                  x_col = "Year-Month", 
                                  y_col = "NOAA Temp Anomaly (°C)", 
                                  hover_text_col = "noaa_hover_text",
                                  legend_title = "Anomaly Data",
                                  plot_title = NULL,
                                  date_start = "1900",
                                  break_interval = "10 years",
                                  type = "temp") {
    
    # Get date range for x-axis
    min_date <- min(data[[x_col]], na.rm = TRUE)
    max_date <- max(data[[x_col]], na.rm = TRUE)
    
    newdat <- data %>% filter(!is.na(.[[y_col]]))
    
    p <- ggplot(newdat, aes(x = .data[[x_col]])) +
      geom_bar(aes(
        y = .data[[y_col]],
        fill = factor(.data[[y_col]] > 0, 
                      levels = c(TRUE, FALSE), 
                      labels = c("Above baseline", "Below baseline")),
        text = .data[[hover_text_col]]), stat = "identity") +
      scale_fill_manual(
        values = c("Above baseline" = "red",
                   "Below baseline" = "blue",
                   "Baseline" = "black"),
        name = legend_title) +
      geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
      scale_x_date(
        breaks = function(x) seq.Date(from = date_start, 
                                                 to = max(x), 
                                                 by = break_interval),  
        labels = scales::date_format("%Y"))  +
      labs(title = plot_title, x = "Year") +
      theme_classic() +
      theme(panel.border = element_rect(linewidth = 1, fill = "transparent"))
    
    
    if (type == "temp") {
      p <- p + scale_y_continuous(breaks = c(-6,-4,-2,0,2,4,6))
    }
    
    
    if (type == "precip") {
      p
    }
    
    
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
        hoverlabel = list(bgcolor = "white")
        #xaxis = list(title = "Year")
      )
  }
  
  
  ## Create anomaly plots
  
  # For NOAA temperature anomalies
  output$NOAAAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = temp_anomaly_data(),
      x_col = "Year-Month",
      y_col = "NOAA Temp Anomaly (°C)",
      hover_text_col = "noaa_hover_text",
      legend_title = NULL,
      plot_title = "NOAA Temperature Anomalies",
      date_start = as.Date("1900-01-01"),
      break_interval = "20 years",
      type = "temp"
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
      plot_title = "McFarland Hill Temperature Anomalies",
      date_start = as.Date("2000-01-01"),
      break_interval = "5 years",
      type = "temp"
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
      plot_title = "SERC Temperature Anomalies",
      date_start = as.Date("2010-01-01"),
      break_interval = "2 years",
      type = "temp"
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
      plot_title = "NOAA Precipitation Anomalies",
      date_start = as.Date("1901-01-01"),
      break_interval = "20 years",
      type = "precip"
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
      plot_title = "McFarland Hill Precipitation Anomalies",
      date_start = as.Date("2000-01-01"),
      break_interval = "5 years",
      type = "precip"
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
      plot_title = "SERC Precipitation Anomalies",
      date_start = as.Date("2010-01-01"),
      break_interval = "2 years",
      type = "precip"
    )
  })
  
  #-----------------------#
  ####  Records Plots  ####
  #-----------------------# 
  
  # function for record highs
  
  create_record_plot <- function(data, 
                                 annual = F,
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
                                 color_top2 = "darkred",   # Color for top records (var2, optional)
                                 color_other2 = "orange",  # Color for other records (var2, optional)
                                 show_var1 = TRUE,
                                 show_var2 = TRUE,
                                 units = "°C",
                                 ptitle,
                                 date_format = "%Y-%m") { 
    
    # Filter data based on year range
    filtered_data <- data %>%
      filter(year >= min_year & year <= max_year)
    
    # Process first variable
    if(annual == F) {
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
    }
    
    if(annual == T) {
      data1 <- filtered_data %>% 
        arrange(desc(.data[[value_col1]])) %>%
        mutate(date = paste0(year, "-01-01"),
               highlight1 = ifelse(row_number() <= top_n, label_highlight1, label_other1),
               date1 = as.Date(date),
               hover_text1 = sprintf(
                 "Date: %s<br>Value: %.2f %s<br>Rank: %d",
                 format(date1, date_format),
                 .data[[value_col1]],
                 units,
                 row_number())
               )
    }
    
    
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
          if (!is.null(color_other2)) setNames(color_other2, label_other2) else NULL),
        name = NULL) +
      scale_x_date(breaks = seq(from = as.Date("1900-01-01"),
                                to = as.Date("2020-01-01"),
                                by = "20 years"),
                   labels = scales::date_format("%Y")) +
      labs(x = "Year", y = y_label, title = ptitle) +
      theme_classic() +
      theme(
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom")
    
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
  
  # Annual max temp record plot output
  output$AnnualRecordsPlot <- renderPlotly({
    create_record_plot(
      data = temp.data.merged,
      annual = T,
      date_col1 = "year",    
      value_col1 = "noaa.temp",
      min_year = input$year_range_records5[1],
      max_year = input$year_range_records5[2],
      top_n = 10,
      y_label = "Temperature (°C)",
      label_highlight1 = "Top 10 Highest Mean Temperatures",
      label_other1 = "Highest Mean Temperatures",
      label_highlight2 = "Top 10 Highest Max Temperatures",
      label_other2 = "Highest Max Temperatures",
      color_top1 = "black",
      color_other1 = "grey",
      color_top2 = "darkred",
      color_other2 = "orange",
      show_var1 = "annual_mean_temp" %in% input$annual_temp_records_display,
      units = "°C",
      date_format = "%Y",
      ptitle = "Highest Annual NOAA Temperature Records")
  })
  
  # Monthly max temp record plot output
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
      y_label = "Temperature (°C)",
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
      date_format = "%Y-%m",
      ptitle = "Highest Monthly NOAA Temperature Records")
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
      y_label = "Temperature (°C)",
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
      date_format = "%Y-%m-%d",
      ptitle = "Highest Daily NOAA Temperature Records"
    )
  })
  
  # Annual precip record plot output
  output$AnnualPrecipRecordsPlot <- renderPlotly({
    create_record_plot(
      data = precip.data.merged,
      annual = T,
      date_col1 = "year",    
      date_col2 = NULL,    
      value_col1 = "noaa.precip",
      value_col2 = NULL,
      min_year = input$year_range_precip_record[1],
      max_year = input$year_range_precip_record[2],
      top_n = 10,
      y_label = "Total precipitation (in)",
      label_highlight1 = "Top 10 Highest Precipitation Records",
      label_other1 = "Highest Precipitation Records",
      color_top1 = "darkblue",
      color_other1 = "lightblue",
      # show_var1 = "max_precip" %in% input$precip_records_display,
      units = "in",
      date_format = "%Y",
      ptitle = "Highest Annual Precipitation Records"
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
      min_year = input$year_range_precip_record[1],
      max_year = input$year_range_precip_record[2],
      top_n = 10,
      y_label = "Monthly precipitation (in)",
      label_highlight1 = "Top 10 Highest Precipitation Records",
      label_other1 = "Highest Precipitation Records",
      color_top1 = "darkblue",
      color_other1 = "lightblue",
      # show_var1 = "max_precip" %in% input$precip_records_display,
      units = "in",
      date_format = "%Y-%m",
      ptitle = "Highest Monthly Precipitation Records"
    )
  })
  
  
  
  # Function for record lows
  record_lows <- function(data, 
                          annual = F,
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
                          date_format = "%Y-%m",
                          ptitle) { 
    
    # Filter data based on year range
    filtered_data <- data %>%
      filter(year >= min_year & year <= max_year)
    
    # Process first variable
    if(annual == F) {
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
    }
    
    if(annual == T) {
      tmean.min <- filtered_data %>% 
        arrange(.data[[value_col1]]) %>%
        mutate(date = paste0(year, "-01-01"),
               highlight1 = ifelse(row_number() <= top_n, label_highlight1, label_other1),
               date.tmean.min = as.Date(date),
               hover_text_mean_min = sprintf(
                 "Date: %s<br>Value: %.2f %s<br>Rank: %d",
                 format(date.tmean.min, date_format),
                 .data[[value_col1]],
                 units,
                 row_number())
        )
    }
    
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
        name = NULL) +
      scale_x_date(
        breaks = seq(from = as.Date("1900-01-01"), 
                     to = as.Date("2020-01-01"),
                     by = "20 years"),
        labels = scales::date_format("%Y")) +
      labs(x = "Year", y = y_label, title = ptitle) +
      theme_classic() +
      theme(
        plot.subtitle = element_text(size = 12),
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
  
  # Annual max temp record plot output
  output$AnnualLowRecordsPlot <- renderPlotly({
    record_lows(
      data = temp.data.merged,
      annual = T,
      date_col1 = "year",    
      value_col1 = "noaa.temp",
      min_year = input$year_range_records6[1],
      max_year = input$year_range_records6[2],
      top_n = 10,
      y_label = "Temperature (°C)",
      label_highlight1 = "Top 10 Lowest Mean Temperatures",
      label_other1 = "Lowest Mean Temperatures",
      label_highlight2 = "Top 10 Lowest Minimum Temperatures",
      label_other2 = "Lowest Minimum Temperatures",
      color_top1 = "black",
      color_other1 = "grey",
      color_top2 = "darkblue",
      color_other2 = "lightblue",
      show_var1 = "annual_low_temp" %in% input$annual_low_records_display,
      units = "°C",
      date_format = "%Y",
      ptitle = "Lowest Annual NOAA Temperature Records")
  })
  
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
      y_label = "Temperature (°C)",
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
      date_format = "%Y-%m",
      ptitle = "Lowest Monthly NOAA Temperature Records"
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
      y_label = "Temperature (°C)",
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
      date_format = "%Y-%m-%d",
      ptitle = "Lowest Daily NOAA Temperature Records"
    )
  })
  
  # Annual precip record plot output
  output$AnnualDroughtRecordsPlot <- renderPlotly({
    record_lows(
      data = precip.data.merged,
      annual = T,
      date_col1 = "year",    
      date_col2 = NULL,    
      value_col1 = "noaa.precip",
      value_col2 = NULL,
      min_year = input$year_range_precip_record[1],
      max_year = input$year_range_precip_record[2],
      top_n = 10,
      y_label = "Total precipitation (in)",
      label_highlight1 = "Top 10 Lowest Precipitation Records",
      label_other1 = "Lowest Precipitation Records",
      color_top1 = "black",
      color_other1 = "grey",
      # show_var1 = "min_precip" %in% input$precip_records_display,
      units = "in",
      date_format = "%Y",
      ptitle = "Lowest Annual Precipitation Records"
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
      min_year = input$year_range_precip_record[1],
      max_year = input$year_range_precip_record[2],
      top_n = 10,
      y_label = "Monthly precipitation (in)",
      label_highlight1 = "Top 10 Lowest Precipitation Records",
      label_other1 = "Lowest Precipitation Records",
      color_top1 = "black",
      color_other1 = "grey",
      # show_var1 = "min_precip" %in% input$precip_records_display,
      units = "in",
      date_format = "%Y-%m",
      ptitle = "Lowest Monthly Precipitation Records"
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
    s <- data %>% 
      ggplot() +
      labs(title = plot_title,
           x = "Year",
           y = y_col) +
      theme_classic()
    
    # Add main sea level line ONLY if it is selected
    if (line_label %in% input_check) {
      s <- s + 
        geom_line(aes(x = .data[[x_col]], y = .data[[y_col]],
                      text = .data[[hover_text_col]], color = line_label, 
                      group = 1),  # Ensures correct grouping for Plotly
                  linewidth = 0.3)
    }
    
    # Add appropriate scale based on x-axis type
    if (is_date) {
      s <- s + 
        scale_x_date(breaks = scales::breaks_width("10 years"),
                     labels = scales::date_format("%Y"))
    } else {
      s <- s + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }
    
    # Add color scale
    s <- s + 
      scale_color_manual(values = c(setNames(line_color, line_label)), 
                         name = NULL)
    
    # Add trend line if requested
    if (show_trend && !is.null(model)) {
      s <- s + 
        geom_smooth(
          aes(x = .data[[x_col]], y = .data[[y_col]]),
          method = lm,
          linewidth = 0.5,
          #se = TRUE,
          #fill = "grey80",
          #alpha = 0.8,
          #color = "black"
        #) +
        # geom_line(
        #   aes(x = .data[[x_col]], y = .data[[y_col]], text = .data[[hover_text_col]],
        #       group = 1),
        #   stat = "smooth",
        #   method = "lm",
        #   color = "black",
        #   linewidth = 0.8
        )
    }
    
    # Convert to plotly and customize
    plot <- ggplotly(s, tooltip = "text") %>%
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

