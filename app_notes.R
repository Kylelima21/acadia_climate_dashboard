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


#OLD TEMP ANOM CODE - NOAA and McFarland on same graph

tabPanel(
  "Temperature Anomalies", 
  
  #NOAA anom plot
  fluidRow(
    column(
      width = 4,
      box(
        title = "Select Temperature Anomaly Data to Display:",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        checkboxGroupInput(
          inputId = "linesToShowAnomalies",
          label = NULL,
          choices = c("NOAA Temp Anomaly" = "NOAA Temp Anom",
                      "McFarland Temp Anomaly" = "McFarland Temp Anom"),
          selected = c("NOAA Temp Anom", "McFarland Temp Anom")
        )
      )
    ),
    
    column(
      width = 8,
      box(
        title = "Temperature Anomalies",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput("AnomPlot", height = "600px")
      )
    )
  ),
)))


##Temperature trend plot before removing the points and transparent lines:

#### Server function (server)

server <- function(input, output) {
  
  #### Temp plot
  output$myInteractivePlot <- renderPlotly({
    
    #Revised data - column naming for plot
    temp.rev <- shiny.merged.temp %>% 
      rename(`Year` = year, `NOAA Average Temp` = temp.noaa,`NOAA Average Max Temp` = max.noaa, `NOAA Average Min Temp` = min.noaa, `McFarland Average Temp` = mcfarland) 
    
    #Base plot
    p <- ggplot(temp.rev, aes(x = Year)) +
      scale_x_continuous(breaks = pretty(temp.rev$Year)) +
      labs(title = "Average Temperature (1895-2024)",
           x = "Year",
           y = "Temperature (°C)") +
      theme_minimal()
    
    #Add lines based on checkbox input
    if("NOAA Average Max Temp" %in% input$linesToShow && "NOAA Average Max Temp" %in% colnames(temp.rev)) {
      p <- p + geom_line(aes(y = `NOAA Average Max Temp`, color = "NOAA Average Maximum Temp."))#, alpha = 0.5) 
      # +
      #   geom_point(aes(
      #     y = `NOAA Average Max Temp`, 
      #     color = "NOAA Average Maximum Temp."),
      #     size = 0.8)
      
      # add linear model for noaa max
      if ("lm_noaa_max_temp" %in% input$linesToShow) {
        p <- p + 
          geom_smooth(
            aes(y = `NOAA Average Max Temp`), 
            method = "lm",
            se = TRUE,
            fill = "grey80",   
            alpha = 0.5,       
            color = NA         
          ) +
          geom_line(           
            aes(y = `NOAA Average Max Temp`),
            stat = "smooth",
            method = "lm",
            color = "black",   
            linewidth = 0.8    
          )
      }
    }
    
    if("NOAA Average Temp" %in% input$linesToShow && "NOAA Average Temp" %in% colnames(temp.rev)) {
      p <- p + geom_line(aes(y = `NOAA Average Temp`, color = "NOAA Average Temp."))#, alpha = 0.5) 
      # +
      #   geom_point(aes(
      #     y = `NOAA Average Temp`, 
      #     color = "NOAA Average Temp."),
      #     size = 0.8)
      
      #add linear model for noaa avg
      # LR.max <- lm(max.noaa ~ year, data = shiny.merged.temp)
      # max.intercept <- coef(LR.max)[1]
      # max.slope <- coef(LR.max)[2]
      # max.r.squared <- summary(LR.max)$r.squared
      # max.p.value <- summary(LR.max)$coefficients[2,4]
      # #format info
      # LR.max.info <- paste0(
      #   "y = ", round(max.slope, 2), "x", "+", round(max.intercept, 2),
      #   "<br>R² = ", round(max.r.squared, 3),
      #   "<br>p-value = ", signif(max.p.value, 3)
      # )
      # 
      #graph the linear model
      if ("lm_noaa_temp" %in% input$linesToShow) {
        p <- p + 
          geom_smooth(
            aes(y = `NOAA Average Temp`), #adds confidence interval
            method = "lm",
            se = TRUE,
            fill = "grey80",   
            alpha = 0.5,       
            color = NA         # suppresses confidence interval border
          ) +
          geom_line(           # adds the regression line separately
            aes(y = `NOAA Average Temp`,
                #text = LR.max.info
            ),
            stat = "smooth",
            method = "lm",
            color = "black",   
            linewidth = 0.8    
          )
      }
    }
    
    if("McFarland Average Temp" %in% input$linesToShow && "McFarland Average Temp" %in% colnames(temp.rev)) {
      p <- p + geom_line(aes(y = `McFarland Average Temp`, color = "McFarland Average Temp."))#, alpha = 0.5) 
      # +
      #   geom_point(aes(
      #     y = `McFarland Average Temp`, 
      #     color = "McFarland Average Temp."),
      #     size = 0.8)
      
      # Add linear model for mcfarland 
      if ("lm_mcfarland_temp" %in% input$linesToShow) {
        p <- p + 
          geom_smooth(
            aes(y = `McFarland Average Temp`), 
            method = "lm",
            se = TRUE,
            fill = "grey80",   
            alpha = 0.5,       
            color = NA         
          ) +
          geom_line(           
            aes(y = `McFarland Average Temp`),
            stat = "smooth",
            method = "lm",
            color = "black",   
            linewidth = 0.8    
          )
      }
    }
    
    if("NOAA Average Min Temp" %in% input$linesToShow && "NOAA Average Min Temp" %in% colnames(temp.rev)) {
      p <- p + geom_line(aes(y = `NOAA Average Min Temp`, color = "NOAA Average Minimum Temp."))#, alpha = 0.5) 
      # +
      #   geom_point(aes(
      #     y = `NOAA Average Min Temp`, 
      #     color = "NOAA Average Minimum Temp."),
      #     size = 0.8)
      
      # Add linear model for noaa min
      if ("lm_noaa_min_temp" %in% input$linesToShow) {
        p <- p + 
          geom_smooth(
            aes(y = `NOAA Average Min Temp`), 
            method = "lm",
            se = TRUE,
            fill = "grey80",   
            alpha = 0.5,       
            color = NA         
          ) +
          geom_line(           
            aes(y = `NOAA Average Min Temp`),
            stat = "smooth",
            method = "lm",
            color = "black",   
            linewidth = 0.8    
          )
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
    
    # Convert ggplot2 plot to an interactive plotly plot
    ggplotly(p, tooltip = c("text", "NOAA Average Temp", "NOAA Average Max Temp", "NOAA Average Min Temp", "McFarland Average Temp")) %>%   layout(hovermode = "x unified")
    
  })
  
  
## trying to figure out baseline label
  
  #### Temp anom plot
  output$NOAAAnomPlot <- renderPlotly({
    
    #Revised data - column naming for plot
    anom.rev <- shiny.merged.anom %>%
      rename(Year = year, `Year-Month` = noaa.year.month, `NOAA Temp Anom` = noaa.anom) %>%
      mutate(
        `Year-Month` = as.Date(`Year-Month`),
        hover_text = case_when(
          !is.na(`NOAA Temp Anom`) ~ paste(
            "Year-Month:", format(`Year-Month`, "%Y-%m"),
            "<br>NOAA Temp Anomaly:", round(`NOAA Temp Anom`, 4)
          )
        )
      )
    
    #Base plot
    p2 <- ggplot(anom.rev, aes(x = `Year-Month`)) +
      scale_x_date(
        breaks = seq(from = min(anom.rev$`Year-Month`),
                     to = max(anom.rev$`Year-Month`),
                     by = "10 years"),
        labels = scales::date_format("%Y"),
        limits = c(min(anom.rev$`Year-Month`), max(anom.rev$`Year-Month`))
      ) +
      labs(title = "NOAA Monthly Temperature Anomalies (1895-2024)",
           x = "Year",
           y = "Temperature Anomaly (°C)") +
      theme_minimal()
    
    #Add lines based on checkbox input
    p2 <- p2 + geom_bar(aes(y = `NOAA Temp Anom`, fill = factor(`NOAA Temp Anom` > 0, levels = c(TRUE, FALSE), labels = c("NOAA above baseline", "NOAA below baseline")), text = hover_text), stat = "identity") +
      geom_hline(
        aes(yintercept = 0, color = "Baseline"), linetype = "solid")
    
    # Customize the legend and colors
    p2 <- p2 +
      scale_fill_manual(
        values = c("NOAA above baseline" = "red",
                   "NOAA below baseline" = "blue",
                   "Baseline" = "black"),
        name = "NOAA Anomaly Data") 
    # +
    # scale_color_manual(
    #   values = c("Baseline" = "black"),
    #   name = NULL  # Prevent duplicate legend titles
    # )
    
    # Convert ggplot2 plot to an interactive plotly plot
    plot <- ggplotly(p2, tooltip = "text")
    
    # # Adjust Plotly legend
    # plot <- plot %>% layout(
    #   legend = list(
    #     traceorder = "normal"  # Maintain logical order
    #   )
    # )
    # 
    # plot
  })
  

## server file - option
  
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
      if ("NOAA Average Max Temp" %in% input$linesToShow) {
        p <- p + geom_line(aes(y = `NOAA Average Max Temp`, 
                               color = "NOAA Average Maximum Temp."))
        
        if (!is.null(models$noaa_max)) {
          p <- add_model_line(p, models$noaa_max, "NOAA Average Max Temp")
        }
      }
      
      # Similar blocks for other temperature lines...
      
      ggplotly(p, tooltip = "text")
    })
    
    # Temperature anomaly plot output
    output$NOAAAnomPlot <- renderPlotly({
      data <- temp_anomaly_data()
      
      p <- ggplot(data, aes(x = `Year-Month`)) +
        geom_bar(aes(
          y = `NOAA Temp Anom`,
          fill = `NOAA Temp Anom` > 0,
          text = noaa_hover_text
        ), stat = "identity") +
        scale_fill_manual(
          values = c("TRUE" = "red", "FALSE" = "blue"),
          labels = c("Above baseline", "Below baseline")
        ) +
        geom_hline(yintercept = 0, color = "black") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
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
  
  
## customize hover text for long-term temp plot
  
  # Convert to plotly and customize hover text
  plt <- ggplotly(p) %>%
    layout(hovermode = "x unified")
  
  # Customize hover template for each trace
  for(i in seq_along(plt$x$data)) {
    if(!is.null(plt$x$data[[i]]$name)) {
      # Check if this is a confidence interval or linear model line
      if(grepl("smooth", plt$x$data[[i]]$name, ignore.case = TRUE) || 
         plt$x$data[[i]]$mode == "lines" && plt$x$data[[i]]$line$color == "black") {
        # Hide hover text for smoothed lines and confidence intervals
        plt$x$data[[i]]$hoverinfo <- "skip"
      } else {
        # For regular temperature lines, show the name and value
        plt$x$data[[i]]$hovertemplate <- paste0(
          "%{data.name}: %{y:.1f}°C<br>",
          "<extra></extra>"
        )
        # Remove any trailing periods from names
        plt$x$data[[i]]$name <- gsub("\\.$", "", plt$x$data[[i]]$name)
      }
    }
  }
  
  # Update layout to show year in the unified hover
  plt <- plt %>%
    layout(
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "white"),
      xaxis = list(
        hoverformat = "%Y"  # Format for the year
      )
    )
})

#######

# Customize hover template for each trace
for(i in seq_along(plt$x$data)) {
  if(!is.null(plt$x$data[[i]]$name)) {
    # Check if this is a confidence interval
    if(grepl("smooth", plt$x$data[[i]]$name, ignore.case = TRUE) && 
       !identical(plt$x$data[[i]]$line$color, "black")) {
      # Hide hover text only for confidence intervals
      plt$x$data[[i]]$hoverinfo <- "skip"
    } else if(identical(plt$x$data[[i]]$line$color, "black")) {
      # For linear model lines, modify the name to indicate it's a trend line
      plt$x$data[[i]]$name <- paste(gsub("\\.$", "", plt$x$data[[i]]$name), "(trend)")
      plt$x$data[[i]]$hovertemplate <- paste0(
        "%{data.name}: %{y:.1f}°C<br>",
        "<extra></extra>"
      )
    } else {
      # For temperature lines
      plt$x$data[[i]]$hovertemplate <- paste0(
        "%{data.name}: %{y:.1f}°C<br>",
        "<extra></extra>"
      )
      # Remove any trailing periods from names
      plt$x$data[[i]]$name <- gsub("\\.$", "", plt$x$data[[i]]$name)
    }
  }
}

# Update layout to show year in the unified hover
plt <- plt %>%
  layout(
    hovermode = "x unified",
    hoverlabel = list(bgcolor = "white"),
    xaxis = list(
      hoverformat = "%Y"  # Format for the year
    )
  )
})


#######

# Convert to plotly and customize hover text
plt <- ggplotly(p) %>%
  layout(hovermode = "x unified")

# Customize hover template for each trace
for(i in seq_along(plt$x$data)) {
  if(!is.null(plt$x$data[[i]]$name)) {
    # Only modify hover text for the main temperature lines
    if(!grepl("smooth", plt$x$data[[i]]$mode, ignore.case = TRUE)) {
      plt$x$data[[i]]$hovertemplate <- paste0(
        "%{data.name}: %{y:.1f}°C<br>",
        "<extra></extra>"
      )
      # Remove any trailing periods from names
      plt$x$data[[i]]$name <- gsub("\\.$", "", plt$x$data[[i]]$name)
    }
  }
}

# Update layout to show year in the unified hover
plt <- plt %>%
  layout(
    hovermode = "x unified",
    hoverlabel = list(bgcolor = "white"),
    xaxis = list(
      hoverformat = "%Y"  # Format for the year
    )
  )



#####

# Convert to plotly and customize hover text
plt <- ggplotly(p) %>%
  layout(hovermode = "x unified")

# Customize hover template for each trace
for(i in seq_along(plt$x$data)) {
  if(!is.null(plt$x$data[[i]]$name)) {
    # Check for linear model lines (black lines with mode "lines")
    if(!is.null(plt$x$data[[i]]$mode) && 
       !is.null(plt$x$data[[i]]$line$color) && 
       plt$x$data[[i]]$mode == "lines" && 
       identical(plt$x$data[[i]]$line$color, "black")) {
      
      # Get the base name without trailing period
      base_name <- gsub("\\.$", "", plt$x$data[[i]]$name)
      # Remove "fitted values" and add "trend"
      base_name <- gsub("fitted values", paste(base_name, "trend"), base_name)
      
      plt$x$data[[i]]$hovertemplate <- paste0(
        base_name, ": %{y:.1f}°C<br>",
        "<extra></extra>"
      )
    } else if(!is.null(plt$x$data[[i]]$fill) && 
              plt$x$data[[i]]$fill == "tonexty") {
      # This is a confidence interval
      plt$x$data[[i]]$hovertemplate <- paste0(
        "95% Confidence Interval: %{y:.1f}°C<br>",
        "<extra></extra>"
      )
    } else if(!is.null(plt$x$data[[i]]$mode) && 
              plt$x$data[[i]]$mode == "lines") {
      # These are the main temperature lines
      plt$x$data[[i]]$hovertemplate <- paste0(
        "%{data.name}: %{y:.1f}°C<br>",
        "<extra></extra>"
      )
      # Remove any trailing periods from names
      plt$x$data[[i]]$name <- gsub("\\.$", "", plt$x$data[[i]]$name)
    }
  }
}

# Update layout to show year in the unified hover
plt <- plt %>%
  layout(
    hovermode = "x unified",
    hoverlabel = list(bgcolor = "white"),
    xaxis = list(
      hoverformat = "%Y"  # Format for the year
    )
  )


#############

# Convert to plotly and customize hover text
plt <- ggplotly(p) %>%
  layout(hovermode = "x unified")

# Customize hover template for each trace
for(i in seq_along(plt$x$data)) {
  if(!is.null(plt$x$data[[i]]$name)) {
    # Check for linear model lines (black lines with mode "lines")
    if(!is.null(plt$x$data[[i]]$mode) && 
       !is.null(plt$x$data[[i]]$line$color) && 
       plt$x$data[[i]]$mode == "lines" && 
       identical(plt$x$data[[i]]$line$color, "black")) {
      
      # Get the base name without trailing period
      base_name <- gsub("\\.$", "", plt$x$data[[i]]$name)
      # Remove "fitted values" and add "trend"
      base_name <- gsub("fitted values", paste(base_name, "trend"), base_name)
      
      plt$x$data[[i]]$hovertemplate <- paste0(
        base_name, ": %{y:.1f}°C<br>",
        "<extra></extra>"
      )
    } else if(!is.null(plt$x$data[[i]]$fill) && 
              plt$x$data[[i]]$fill == "tonexty") {
      # This is a confidence interval
      plt$x$data[[i]]$hovertemplate <- paste0(
        "95% Confidence Interval: %{y:.1f}°C<br>",
        "<extra></extra>"
      )
    } else if(!is.null(plt$x$data[[i]]$mode) && 
              plt$x$data[[i]]$mode == "lines") {
      # These are the main temperature lines
      plt$x$data[[i]]$hovertemplate <- paste0(
        "%{data.name}: %{y:.1f}°C<br>",
        "<extra></extra>"
      )
      # Remove any trailing periods from names
      plt$x$data[[i]]$name <- gsub("\\.$", "", plt$x$data[[i]]$name)
    }
  }
}

# Update layout to show year in the unified hover
plt <- plt %>%
  layout(
    hovermode = "x unified",
    hoverlabel = list(bgcolor = "white"),
    xaxis = list(
      hoverformat = "%Y"  # Format for the year
    )
  )

###############

# # NOAA temperature anomaly plot output
# output$NOAAAnomPlot <- renderPlotly({
#   data <- temp_anomaly_data()
#   
#   p2 <- ggplot(data, aes(x = `Year-Month`)) +
#     geom_bar(aes(
#       y = `NOAA Temp Anom`,
#       fill = factor(`NOAA Temp Anom` > 0, levels = c(TRUE, FALSE), labels = c("Above baseline", "Below baseline")),
#       text = noaa_hover_text
#     ), stat = "identity") +
#     scale_fill_manual(
#       values = c("Above baseline" = "red",
#                  "Below baseline" = "blue",
#                  "Baseline" = "black"),
#       name = "NOAA Anomaly Data") +
#     geom_hline(yintercept = 0, color = "black") +
#     theme_minimal()
#   
#   ggplotly(p2, tooltip = "text")
# })
