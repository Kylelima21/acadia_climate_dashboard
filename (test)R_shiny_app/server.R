# server component 

server <- function(input, output) {
  # Reactive data preparations
  temperature_data <- reactive({
    prepare_temperature_data(shiny.merged.temp)
  })
  
  temp_anomaly_data <- reactive({
    prepare_anomaly_data(shiny.merged.anom)
  })
  
  precipitation_data <- reactive({
    prepare_precipitation_data(shiny.merged.precip)
  })
  
  precip_anomaly_data <- reactive({
    prepare_anomaly_data(shiny.merged.precip.anom)
  })
  
  # Temperature models
  temp_models <- reactive({
    calculate_linear_models(temperature_data(), input$linesToShow)
  })
  
  # Temperature trend plot
  output$myInteractivePlot <- renderPlotly({
    plot <- create_temperature_plot(
      data = temperature_data(),
      selected_lines = input$linesToShow,
      models = temp_models()
    )
    
    ggplotly(plot, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
  
  # NOAA temperature anomaly plot
  output$NOAAAnomPlot <- renderPlotly({
    plot <- create_anomaly_plot(
      data = temp_anomaly_data(),
      station = "NOAA"
    )
    
    ggplotly(plot, tooltip = "text")
  })
  
  # McFarland temperature anomaly plot
  output$McFarlandAnomPlot <- renderPlotly({
    plot <- create_anomaly_plot(
      data = temp_anomaly_data(),
      station = "McFarland"
    )
    
    ggplotly(plot, tooltip = "text")
  })
  
  # Precipitation trend plot
  output$PrecipPlot <- renderPlotly({
    plot <- create_precipitation_plot(
      data = precipitation_data(),
      selected_lines = input$linesToShowPrecip,
      models = precip_models()
    )
    
    ggplotly(plot, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
  
  # Precipitation anomaly plots
  output$NOAAPrecipAnomPlot <- renderPlotly({
    plot <- create_precip_anomaly_plot(
      data = precip_anomaly_data(),
      station = "NOAA"
    )
    
    ggplotly(plot, tooltip = "text")
  })
  
  output$McFarlandPrecipAnomPlot <- renderPlotly({
    plot <- create_precip_anomaly_plot(
      data = precip_anomaly_data(),
      station = "McFarland"
    )
    
    ggplotly(plot, tooltip = "text")
  })
}


shinyApp(ui, server)
