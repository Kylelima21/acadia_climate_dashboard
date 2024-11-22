#### R shiny app notes and extras ####

#alternative to anom plot

output$AnomPlot <- renderPlotly({
  
  # Prepare the data
  anom.rev <- shiny.merged.anom %>% 
    rename(Year = year, `Year-Month` = noaa.year.month, `NOAA Temp Anom` = noaa.anom, `McFarland Temp Anom` = mcfarland.anom) %>% 
    mutate(`Year-Month` = as.Date(`Year-Month`))
  
  # Separate NOAA data into above and below baseline
  noaa_above <- anom.rev %>%
    filter(!is.na(`NOAA Temp Anom`) & `NOAA Temp Anom` > 0) %>%
    mutate(
      hover_text = paste(
        "Year-Month:", format(`Year-Month`, "%Y-%m"),
        "<br>NOAA Temp Anomaly:", round(`NOAA Temp Anom`, 2),
        "<br>Status: Above Baseline"
      )
    )
  
  noaa_below <- anom.rev %>%
    filter(!is.na(`NOAA Temp Anom`) & `NOAA Temp Anom` <= 0) %>%
    mutate(
      hover_text = paste(
        "Year-Month:", format(`Year-Month`, "%Y-%m"),
        "<br>NOAA Temp Anomaly:", round(`NOAA Temp Anom`, 2),
        "<br>Status: Below Baseline"
      )
    )
  
  # Separate McFarland data into above and below baseline
  mcfarland_above <- anom.rev %>%
    filter(!is.na(`McFarland Temp Anom`) & `McFarland Temp Anom` > 0) %>%
    mutate(
      hover_text = paste(
        "Year-Month:", format(`Year-Month`, "%Y-%m"),
        "<br>McFarland Temp Anomaly:", round(`McFarland Temp Anom`, 2),
        "<br>Status: Above Baseline"
      )
    )
  
  mcfarland_below <- anom.rev %>%
    filter(!is.na(`McFarland Temp Anom`) & `McFarland Temp Anom` <= 0) %>%
    mutate(
      hover_text = paste(
        "Year-Month:", format(`Year-Month`, "%Y-%m"),
        "<br>McFarland Temp Anomaly:", round(`McFarland Temp Anom`, 2),
        "<br>Status: Below Baseline"
      )
    )
  
  # Start a blank plotly object
  plot <- plot_ly()
  
  # Add NOAA above baseline
  if ("NOAA Temp Anom" %in% input$linesToShow) {
    plot <- plot %>%
      add_bars(
        data = noaa_above,
        x = ~`Year-Month`,
        y = ~`NOAA Temp Anom`,
        text = ~hover_text,
        hoverinfo = "text",
        name = "NOAA Above Baseline",
        marker = list(color = "red")
      ) %>%
      add_bars(
        data = noaa_below,
        x = ~`Year-Month`,
        y = ~`NOAA Temp Anom`,
        text = ~hover_text,
        hoverinfo = "text",
        name = "NOAA Below Baseline",
        marker = list(color = "blue")
      )
  }
  
  # Add McFarland above baseline
  if ("McFarland Temp Anom" %in% input$linesToShow) {
    plot <- plot %>%
      add_bars(
        data = mcfarland_above,
        x = ~`Year-Month`,
        y = ~`McFarland Temp Anom`,
        text = ~hover_text,
        hoverinfo = "text",
        name = "McFarland Above Baseline",
        marker = list(color = "#990000")
      ) %>%
      add_bars(
        data = mcfarland_below,
        x = ~`Year-Month`,
        y = ~`McFarland Temp Anom`,
        text = ~hover_text,
        hoverinfo = "text",
        name = "McFarland Below Baseline",
        marker = list(color = "#000066")
      )
  }
  
  # Finalize the layout
  plot <- plot %>%
    layout(
      title = "Monthly Temperature Anomalies (1895-2024)",
      xaxis = list(title = "Year"),
      yaxis = list(title = "Temperature Anomaly (°C)"),
      barmode = "relative", # Bars will stack; use 'group' if you want side-by-side bars
      legend = list(title = list(text = "Data Source"))
    )
  
  return(plot)
})

