# server component

server <- function(input, output) {
  
  #----------------------#
  ####    Map Output  ####
  #----------------------#  
  
  output$LocationMap <- renderLeaflet({
  # Define location data
  station_locations <- data.frame(
    name = c("McFarland Hill Atmospheric Research Station", "Winter Harbor-SERC Weather Station", "NOAA - Acadia National Park"),
    lat = c(44.3772, 44.33567, 44.372907),
    lng = c(-68.2608, -68.062, -68.258257)
  )
  
  buoy_locations <- data.frame(
    name = c("Bar Harbor, Frenchman Bay Station"),
    lat = c(44.391667),
    lng = c(-68.205)
  )
  
  # Create the map
  leaflet() %>%
    # Add both base layers
    addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
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
    
    # Add marker for buoys
    addCircleMarkers(
      data = buoy_locations,
      lng = ~lng, 
      lat = ~lat,
      radius = 6,  # Adjust marker size
      color = "green",  # Outline color
      fillColor = "yellow",  # Fill color
      fillOpacity = 0.8,
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
        `NOAA Average Temp` = noaa.temp,
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
        lm(`NOAA Average Temp` ~ Year, data = data),
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
  
  # Temperature plot output ------------------------------------------
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
      theme_minimal()
    
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
    if ("NOAA Average Temp" %in% input$linesToShow) {
          p <- p + geom_line(aes(x = Year,
                                 y = `NOAA Average Temp`,
                                 color = "NOAA Average Temp."))
          
      if (!is.null(models$noaa_avg)) {
        p <- add_model_line(p, models$noaa_avg, "NOAA Average Temp")
        
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
        "NOAA Average Temp." = "#000000", 
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
  
  
    # Temp model summaries -----------------------------------------

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
  
  # Precipitation plot output ----------------------------------------
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
    
    # Precip model summaries -----------------------------------------
    
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
      geom_hline(yintercept = 0, color = "black") +
      scale_x_date(
        breaks = scales::breaks_width(break_interval),  
        labels = scales::date_format("%Y")
      )  +
      xlab("Year") +
      theme_minimal()
    
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
      break_interval = "10 years"
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
      label_highlight2 = "Top 10 Lowest Max Temperatures",
      label_other2 = "Lowest Max Temperatures",
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
  
  
  #-----------------------------#
  ####  Sea Level Plot Function  ####
  #-----------------------------#  
  
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
  
  # sea level model summaries -----------------------------------------
  
  output$monthly_sea_model_summary <- renderPrint({
    req("lm_monthly_sea" %in% input$linesToShowMonthlySea)
    summary(monthly_sea_model()$monthly_sea)
  })
  
  output$annual_sea_model_summary <- renderPrint({
    req("lm_annual_sea" %in% input$linesToShowAnnualSea)
    summary(annual_sea_model()$annual_sea)
  })
  
  
}

#### shinyApp function (fuse ui and server)

shinyApp(ui, server)
