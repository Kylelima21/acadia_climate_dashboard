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
  
}

#### shinyApp function (fuse ui and server)

shinyApp(ui, server)
