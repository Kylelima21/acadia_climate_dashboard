# server component

server <- function(input, output) {
  
  #---------------------------------------#
  ####  Reactive data transformations  ####
  #---------------------------------------#   

  # Reactive for temperature data
  temperature_data <- reactive({
    shiny.merged.temp %>% 
      rename(
        Year = year, 
        `NOAA Average Temp` = temp.noaa,
        `NOAA Average Max Temp` = max.noaa, 
        `NOAA Average Min Temp` = min.noaa, 
        `McFarland Average Temp` = mcfarland
      )
  })
  
  # Reactive for precipitation data
  precipitation_data <- reactive({
    shiny.merged.precip %>% 
      rename(
        Year = year, 
        `NOAA Precip` = noaa.precip,
        `McFarland Precip` = McFarland.precip
      )
  })
  
  # Reactive for temperature anomaly data
  temp_anomaly_data <- reactive({
    shiny.merged.anom %>%
      rename(
        Year = year, 
        `Year-Month` = noaa.year.month, 
        `NOAA Temp Anom` = noaa.anom,
        `McFarland Temp Anom` = mcfarland.anom
      ) %>%
      mutate(
        `Year-Month` = as.Date(`Year-Month`),
        noaa_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>NOAA Temp Anomaly:", round(`NOAA Temp Anom`, 4)
        ),
        mcfarland_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>McFarland Temp Anomaly:", round(`McFarland Temp Anom`, 4)
        )
      )
  })
  
  # Reactive for precipitation anomaly data
  precip_anomaly_data <- reactive({
    shiny.merged.precip.anom %>%
      rename(
        Year = year, 
        `Year-Month` = noaa.year.month, 
        `NOAA Precip Anom` = noaa.percent.precip.anom,
        `McFarland Precip Anom` = mcfarland.percent.precip.anom
      ) %>%
      mutate(
        `Year-Month` = as.Date(`Year-Month`),
        noaa_precip_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>NOAA Precip Anomaly:", round(`NOAA Precip Anom`, 4)
        ),
        mcfarland_precip_hover_text = paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>McFarland Precip Anomaly:", round(`McFarland Precip Anom`, 4)
        )
      )
  })

  # Reactive for linear models
  temp_models <- reactive({
    data <- temperature_data()
    list(
      noaa_avg = if ("lm_noaa_temp" %in% input$linesToShow) 
        lm(`NOAA Average Temp` ~ Year, data = data),
      noaa_max = if ("lm_noaa_max_temp" %in% input$linesToShow) 
        lm(`NOAA Average Max Temp` ~ Year, data = data),
      noaa_min = if ("lm_noaa_min_temp" %in% input$linesToShow) 
        lm(`NOAA Average Min Temp` ~ Year, data = data),
      mcfarland = if ("lm_mcfarland_temp" %in% input$linesToShow) 
        lm(`McFarland Average Temp` ~ Year, data = data)
    )
  })
  
  # Reactive for precipitation models
  precip_models <- reactive({
    data <- precipitation_data()
    list(
      noaa_precip = if ("lm_noaa_precip" %in% input$linesToShowPrecip) 
        lm(`NOAA Precip` ~ Year, data = data),
      mcfarland_precip = if ("lm_mcfarland_precip" %in% input$linesToShowPrecip) 
        lm(`McFarland Precip` ~ Year, data = data)
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
    
    p <- ggplot(data, aes(x = Year)) +
      scale_x_continuous(breaks = pretty(data$Year)) +
      labs(title = "Average Temperature (1895-2024)",
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
    
    #add mcfarland temp
    if ("McFarland Average Temp" %in% input$linesToShow) {
      p <- p + geom_line(aes(x = Year,
                             y = `McFarland Average Temp`,
                             color = "McFarland Average Temp."))
      
      if (!is.null(models$mcfarland)) {
        p <- add_model_line(p, models$mcfarland, "McFarland Average Temp")
        
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
    
    # Customize the legend and colors
    p <- p + scale_color_manual(
      values = c(
        "NOAA Average Temp." = "#000000", 
        "NOAA Average Maximum Temp." = "#CC3300", 
        "NOAA Average Minimum Temp." = "#003399", 
        "McFarland Average Temp." = "#00CC00"
      ),
      name = "Temperature Type"
    )
    
    # Convert to plotly and customize hover text
    temp_plt <- ggplotly(p) %>%
      layout(hovermode = "x unified") %>%
      customize_hover_text(units = "°C") %>%
      layout(
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(hoverformat = "%Y")
      )
  })
  
  # Precipitation plot output ----------------------------------------
  output$PrecipPlot <- renderPlotly({
    data <- precipitation_data()
    models <- precip_models()
    
    p2 <- ggplot(data, aes(x = Year)) +
      scale_x_continuous(breaks = pretty(data$Year)) +
      labs(title = "Total Precipitation (1895-2024)",
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
  
  # Customize the legend and colors
  p2 <- p2 + scale_color_manual(
    values = c(
      "NOAA Total Precip." = "#000000", 
      "McFarland Total Precip." = "#00CC00"
    ),
    name = "Precipitation Data"
  )
  
  # Convert to plotly and customize hover text
  precip_plt <- ggplotly(p2) %>%
    layout(hovermode = "x unified") %>%
    customize_hover_text(units = "in") %>%
    layout(
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "white"),
      xaxis = list(hoverformat = "%Y")
    )
  })


  #-----------------------#
  ####  Anomaly Plots  ####
  #-----------------------#  
  
  #create anomaly plot function
  create_anomaly_plot <- function(data, 
                                  x_col = "Year-Month", 
                                  y_col = "NOAA Temp Anom", 
                                  hover_text_col = "noaa_hover_text",
                                  legend_title = "Anomaly Data") {
    
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
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  }
  
#create anomaly plots
  # For NOAA temperature anomalies
  output$NOAAAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = temp_anomaly_data(),
      x_col = "Year-Month",
      y_col = "NOAA Temp Anom",
      hover_text_col = "noaa_hover_text",
      legend_title = "NOAA Anomaly Data"
    )
  })
  
  # For McFarland temperature anomalies
  output$McFarlandAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = temp_anomaly_data(),
      x_col = "Year-Month",
      y_col = "McFarland Temp Anom",
      hover_text_col = "mcfarland_hover_text",
      legend_title = "McFarland Anomaly Data"
    )
  })
  
  # For NOAA precipitation anomalies
  output$NOAAPrecipAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = precip_anomaly_data(),
      x_col = "Year-Month",
      y_col = "NOAA Precip Anom",
      hover_text_col = "noaa_precip_hover_text",
      legend_title = "NOAA Precipitation Anomaly Data"
    )
  })
  
  # For McFarland precipitation anomalies
  output$McFarlandPrecipAnomPlot <- renderPlotly({
    create_anomaly_plot(
      data = precip_anomaly_data(),
      x_col = "Year-Month",
      y_col = "McFarland Precip Anom",
      hover_text_col = "mcfarland_precip_hover_text",
      legend_title = "McFarland Precipitation Anomaly Data"
    )
  })
  
  #-----------------------#
  ####  Records Plots  ####
  #-----------------------# 
  
  # function for record highs
  
  create_record_plot <- function(data, 
                                 date_col1,     # Date column for first variable
                                 date_col2,     # Date column for second variable
                                 value_col1,    # First value column to plot
                                 value_col2,    # Second value column to plot
                                 min_year,      # Minimum year for filtering
                                 max_year,      # Maximum year for filtering
                                 top_n = 10,    # Number of top records to highlight
                                 y_label = "",  # Y-axis label
                                 color_top1 = "black",     # Color for top records (var1)
                                 color_other1 = "grey",    # Color for other records (var1)
                                 color_top2 = "darkred",    # Color for top records (var2)
                                 color_other2 = "orange") { # Color for other records (var2)
    
    # Filter and prepare data for both variables
    filtered_data <- data %>%
      filter(year >= min_year & year <= max_year)
    
    # Process first variable
    data1 <- filtered_data %>%
      arrange(desc(.data[[value_col1]])) %>%
      mutate(
        highlight1 = ifelse(row_number() <= top_n, paste("Top", top_n, "Highest Monthly Mean Temp"), "Highest Monthly Mean Temp"),
        date1 = as.Date(.data[[date_col1]])
      )
    
    # Process second variable
    data2 <- filtered_data %>%
      arrange(desc(.data[[value_col2]])) %>%
      mutate(
        highlight2 = ifelse(row_number() <= top_n, paste("Top", top_n, "Highest Monthly Max Temp"), "Highest Monthly Max Temp"),
        date2 = as.Date(.data[[date_col2]])
      )
    
    # Get date range for x-axis
    min_date <- min(c(data1$date1, data2$date2))
    max_date <- max(c(data1$date1, data2$date2))
    
    # Create the ggplot
    p <- ggplot() +
      # First variable
      geom_segment(
        data = data1,
        aes(x = date1, xend = date1,
            y = min(.data[[value_col1]]), 
            yend = .data[[value_col1]],
            color = highlight1),
        linetype = "solid", 
        alpha = 0.6
      ) +
      geom_point(
        data = data1,
        aes(x = date1, 
            y = .data[[value_col1]],
            color = highlight1),
        size = 2
      ) +
      # Second variable
      geom_segment(
        data = data2,
        aes(x = date2, xend = date2,
            y = min(.data[[value_col2]]), 
            yend = .data[[value_col2]],
            color = highlight2),
        linetype = "solid", 
        alpha = 0.6
      ) +
      geom_point(
        data = data2,
        aes(x = date2, 
            y = .data[[value_col2]],
            color = highlight2),
        size = 2
      ) +
      scale_color_manual(
        values = c(
          setNames(color_top1, paste("Top", top_n, "Highest Monthly Mean Temp")),
          setNames(color_other1, "Highest Monthly Mean Temp"),
          setNames(color_top2, paste("Top", top_n, "Highest Monthly Max Temp")),
          setNames(color_other2, "Highest Monthly Max Temp")
        ),
        name = "Records"
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
    
    # Convert to plotly
    ggplotly(p)
  }
  
  # max temp record plot output
  output$MaxTempRecordsPlot <- renderPlotly({
    create_record_plot(
      data = shiny.monthly.records,
      date_col1 = "tmean.max.ym",    
      date_col2 = "tmax.max.ym",    
      value_col1 = "tmean.max",
      value_col2 = "tmax.max",
      min_year = input$year_range[1],
      max_year = input$year_range[2],
      top_n = 10,
      y_label = "Monthly average temperature (°C)",
      color_top1 = "black",
      color_other1 = "grey",
      color_top2 = "darkred",
      color_other2 = "orange"
    )
  })
  
  # max precip record plot output
  output$MaxPrecipRecordsPlot <- renderPlotly({
    create_record_plot(
      data = shiny.monthly.precip.records,
      date_col1 = "ppt.max.ym",    
      date_col2 = NULL,    
      value_col1 = "ppt.max",
      value_col2 = NULL,
      min_year = input$year_range[1],
      max_year = input$year_range[2],
      top_n = 10,
      y_label = "Monthly average temperature (°C)",
      color_top1 = "black",
      color_other1 = "grey",
      color_top2 = NULL,
      color_other2 = NULL
    )
  })
  
  # function for record lows
  
  record_lows <- function(data, 
                                 date_col1,     
                                 date_col2,     
                                 value_col1,    
                                 value_col2,    
                                 min_year,      
                                 max_year,      
                                 top_n = 10,    
                                 y_label = "",  
                                 color_top1 = "black",     
                                 color_other1 = "grey",    
                                 color_top2 = "darkblue",    
                                 color_other2 = "lightblue") { 
    
    # Filter and prepare data for both variables
    filtered_data <- data %>%
      filter(year >= min_year & year <= max_year)
    
    # # Get the maximum value to start the lines from
    # max_value <- max(c(filtered_data[[value_col1]], filtered_data[[value_col2]]))
    
    # Process first variable
    tmean.min <- filtered_data %>%
      arrange(.data[[value_col1]]) %>%   
      mutate(
        highlight1 = ifelse(row_number() <= top_n, 
                            paste("Top", top_n, "Lowest Monthly Mean Temp"), 
                            "Lowest Monthly Mean Temp"),
        date.tmean.min = as.Date(.data[[date_col1]])
      )
    
    # Process second variable
    tmin.min <- filtered_data %>%
      arrange(.data[[value_col2]]) %>%   
      mutate(
        highlight2 = ifelse(row_number() <= top_n, 
                            paste("Top", top_n, "Lowest Monthly Min Temp"), 
                            "Lowest Monthly Min Temp"),
        date.tmin.min = as.Date(.data[[date_col2]])
      )
    
    # Get date range for x-axis
    min_date <- min(c(tmean.min$date.tmean.min, tmin.min$date.tmin.min))
    max_date <- max(c(tmean.min$date.tmean.min, tmin.min$date.tmin.min))
    
    # Create the ggplot
    p <- ggplot() +
      # First variable - lines now drop from the top
      geom_segment(
        data = tmean.min,
        aes(x = date.tmean.min, xend = date.tmean.min,
            y = max(.data[[value_col1]]),  
            yend = .data[[value_col1]],
            color = highlight1),
        linetype = "solid", 
        alpha = 0.6
      ) +
      geom_point(
        data = tmean.min,
        aes(x = date.tmean.min, 
            y = .data[[value_col1]],
            color = highlight1),
        size = 2
      ) +
      # Second variable - lines now drop from the top
      geom_segment(
        data = tmin.min,
        aes(x = date.tmin.min, xend = date.tmin.min,
            y = max(.data[[value_col2]]),  
            yend = .data[[value_col2]],
            color = highlight2),
        linetype = "solid", 
        alpha = 0.6
      ) +
      geom_point(
        data = tmin.min,
        aes(x = date.tmin.min, 
            y = .data[[value_col2]],
            color = highlight2),
        size = 2
      ) +
      scale_color_manual(
        values = c(
          setNames(color_top1, paste("Top", top_n, "Lowest Monthly Mean Temp")),
          setNames(color_other1, "Lowest Monthly Mean Temp"),
          setNames(color_top2, paste("Top", top_n, "Lowest Monthly Min Temp")),
          setNames(color_other2, "Lowest Monthly Min Temp")
        ),
        name = "Records"
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
    
    # Convert to plotly
    ggplotly(p)
  }
  
  # min temp record plot output
  output$MinTempRecordsPlot <- renderPlotly({
    record_lows(
      data = shiny.monthly.records,
      date_col1 = "tmean.min.ym",    
      date_col2 = "tmin.min.ym",     
      value_col1 = "tmean.min",      
      value_col2 = "tmin.min",       
      min_year = input$year_range[1],
      max_year = input$year_range[2],
      top_n = 10,
      y_label = "Monthly average temperature (°C)",
      color_top1 = "black",
      color_other1 = "grey",
      color_top2 = "darkblue",       
      color_other2 = "lightblue"     
    )
  })

  
}

#### shinyApp function (fuse ui and server)

shinyApp(ui, server)
