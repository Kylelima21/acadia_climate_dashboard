# server component

server <- function(input, output) {
  # Reactive data transformations
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
  
  # Reactive for precipitation data
  precipitation_data <- reactive({
    shiny.merged.precip %>% 
      rename(
        Year = year, 
        `NOAA Precip` = noaa.precip,
        `McFarland Precip` = McFarland.precip
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
      noaa = if ("lm_noaa_precip" %in% input$linesToShowPrecip) 
        lm(`NOAA Precip` ~ Year, data = data),
      mcfarland = if ("lm_mcfarland_precip" %in% input$linesToShowPrecip) 
        lm(`McFarland Precip` ~ Year, data = data)
    )
  })
  
  # Temperature plot output
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
    plt <- ggplotly(p) %>%
      layout(hovermode = "x unified")
    
    # Customize hover template for each trace
    for(i in seq_along(plt$x$data)) {
      if(!is.null(plt$x$data[[i]]$name)) {
        plt$x$data[[i]]$hovertemplate <- paste0(
          "Year: %{x}<br>",
          plt$x$data[[i]]$name, ": %{y:.1f}°C<br>",
          "<extra></extra>"
        )
      }
    }
    
    plt
  })
  
  # Temperature anomaly plot output
  output$NOAAAnomPlot <- renderPlotly({
    data <- temp_anomaly_data()
    
    p2 <- ggplot(data, aes(x = `Year-Month`)) +
      geom_bar(aes(
        y = `NOAA Temp Anom`,
        fill = factor(`NOAA Temp Anom` > 0, levels = c(TRUE, FALSE), labels = c("Above baseline", "Below baseline")),
        text = noaa_hover_text
      ), stat = "identity") +
      scale_fill_manual(
        values = c("Above baseline" = "red",
                   "Below baseline" = "blue",
                   "Baseline" = "black"),
        name = "NOAA Anomaly Data") +
      geom_hline(yintercept = 0, color = "black") +
      theme_minimal()
    
    ggplotly(p2, tooltip = "text")
  })
  
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
}

#### shinyApp function (fuse ui and server)

shinyApp(ui, server)
