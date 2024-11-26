#### R shiny dashboard displaying climate data from NOAA (nClimGrid) and McFarland Hill station in Acadia NP ####

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(shinydashboard)
library(fresh)
library(ggplot2)
library(readr)
library(plotly)
library(tidyverse)
library(dplyr)

#-----------------------#
####    Read Data    ####
#-----------------------#

shiny.merged.temp <- read.csv("data/processed_data/shiny_merged_temp.csv")

shiny.merged.precip <- read.csv("data/processed_data/shiny_merged_precip.csv")

shiny.merged.anom <- read.csv("data/processed_data/shiny_merged_anom.csv")

#--------------------------------------#
####    Build R Shiny Dashboard     ####
#--------------------------------------# 

#### User interface (ui)

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Acadia Climate Dashboard",
    titleWidth = 325
    ),
  
  dashboardSidebar(
    width = 325,
    
   # Add sidebar menu with tabs
     sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Temperature Trends", tabName = "temp", icon = icon("chart-line")),
      menuItem("Precipitation Trends", tabName = "precip", icon = icon("chart-line"))
     )
    ),
    
  
  dashboardBody(
    tags$head(tags$link(type = "text/css", rel = "stylesheet", href = "css/style.css")),
    
    # Define tab items
    tabItems(
      # Tab for dashboard overview (add content here later)
      tabItem(tabName = "overview",
              h2("Dashboard Overview"),
              p("Content for the dashboard overview can go here.")
      ),
      
      # Tab for interactive temperature plot
      tabItem(tabName = "temp",
              
              tabsetPanel(
                tabPanel(
                  "Temperature Trends",
                
              
              # Add fluidRow for the checkbox group and plot
              fluidRow(
                # Column for the checkbox group input
                column(
                  width = 4, 
                box(
                  title = "Select Temperature Data to Display:",
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  # Add checkbox group for line selection
                  checkboxGroupInput(
                    inputId = "linesToShow",
                    label = NULL,
                    choices = c("NOAA Average Temp." = "NOAA Average Temp",
                                "NOAA Average Maximum Temp." = "NOAA Average Max Temp",
                                "NOAA Average Minimum Temp." = "NOAA Average Min Temp",
                                "McFarland Average Temp." = "McFarland Average Temp",
                                "Linear Model for NOAA Average Temp" = "lm_noaa_temp",
                                "Linear Model for NOAA Average Max Temp" = "lm_noaa_max_temp",
                                "Linear Model for NOAA Average Min Temp" = "lm_noaa_min_temp",
                                "Linear Model for McFarland Average Temp" = "lm_mcfarland_temp"
                                ),
                    selected = c("NOAA Average Temp", "NOAA Average Max Temp", "NOAA Average Min Temp", "McFarland Average Temp")
                    )
                  )
                ),
                
                # Column for the plot
                column(
                  width = 8,
                  box(
                    title = "Long-Term Temperature Trends", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotlyOutput("myInteractivePlot", height = "600px")
              )
            )
          )
        ),
        
        tabPanel(
          "Temperature Anomalies", 
          
          #NOAA anom plot
          fluidRow(
            column(
              width = 12,
              box(
                title = "NOAA Temperature Anomalies",
                status = "primary",
                solidHeader = TRUE,
                width = 15,
                plotlyOutput("NOAAAnomPlot", height = "600px")
              )
            )
          ),
         
          #McFarland anom plot 
          fluidRow(
            column(
              width = 12,
              box(
                title = "McFarland Temperature Anomalies",
                status = "primary",
                solidHeader = TRUE,
                width = 15,
                plotlyOutput("McFarlandAnomPlot", height = "600px")
              )
            )
          ),
          
      )
    )
  ),
      
      #Tab for interactive precipitation plot
      tabItem(tabName = "precip",
              
              fluidRow(
                column(
                  width = 4,
                  box(
                    title = "Select Precipitation Data to Display:",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    checkboxGroupInput(
                      inputId = "linesToShowPrecip",
                      label = NULL,
                      choices = c("NOAA Total Precip." = "NOAA Precip",
                                  "McFarland Total Precip." = "McFarland Precip",
                                  "Linear Model for NOAA Precip" = "lm_noaa_precip",
                                  "Linear Model for McFarland Precip" = "lm_mcfarland_precip"),
                      selected = c("NOAA Precip", "McFarland Precip")
                    )
                  )
                ),
                
                column(
                  width = 8,
                  box(
                    title = "Long-Term Precipitation Trends",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotlyOutput("PrecipPlot", height = "600px")
                  )
                )
              )
            )
      
      )
  )
)


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
      p2 <- p2 + geom_bar(aes(y = `NOAA Temp Anom`, fill = factor(`NOAA Temp Anom` > 0, labels = c("NOAA below baseline", "NOAA above baseline")), text = hover_text), stat = "identity") +
        geom_hline(yintercept = 0, linetype = "solid", color = "black")

    # Customize the legend and colors
    p2 <- p2 +
      scale_fill_manual(
        values = c("NOAA above baseline" = "red",
                   "NOAA below baseline" = "blue"),
      name = "NOAA Anomaly Data"
    )

    # Convert ggplot2 plot to an interactive plotly plot
    ggplotly(p2, tooltip = "text")

  })
  
  #McFarland anom plot
  
  output$McFarlandAnomPlot <- renderPlotly({
    
    #Revised data - column naming for plot
    mcfarland.anom.rev <- shiny.merged.anom %>%
      rename(Year = year, `Year-Month` = mcfarland.year.month,`McFarland Temp Anom` = mcfarland.anom) %>%
      mutate(
        `Year-Month` = as.Date(`Year-Month`),
        hover_text = case_when(
          !is.na(`McFarland Temp Anom`) ~ paste(
            "Year-Month:", format(`Year-Month`, "%Y-%m"),
            "<br>McFarland Temp Anomaly:", round(`McFarland Temp Anom`, 4)
          )
        )
      )
    
    
    #Base plot
    p2.1 <- ggplot(mcfarland.anom.rev, aes(x = `Year-Month`)) +
      scale_x_date(
        breaks = seq(from = min(mcfarland.anom.rev$`Year-Month`, na.rm = TRUE),
                     to = max(mcfarland.anom.rev$`Year-Month`, na.rm = TRUE),
                     by = "5 years"),
        labels = scales::date_format("%Y"),
        limits = c(min(mcfarland.anom.rev$`Year-Month`), max(mcfarland.anom.rev$`Year-Month`))
      ) +
      labs(title = "McFarland Monthly Temperature Anomalies (1895-2024)",
           x = "Year",
           y = "Temperature Anomaly (°C)") +
      theme_minimal()
    
    #Add lines based on checkbox input
      p2.1 <- p2.1 + geom_bar(aes(y = `McFarland Temp Anom`, fill = factor(`McFarland Temp Anom` > 0, labels = c("McFarland below baseline", "McFarland above baseline")), text = hover_text), stat = "identity") 
    
    # Customize the legend and colors
    p2.1 <- p2.1 +
      scale_fill_manual(
        values = c("McFarland above baseline" = "red",
                   "McFarland below baseline" = "blue"),
        name = "McFarland Anomaly Data"
      )
    
    # Convert ggplot2 plot to an interactive plotly plot
    ggplotly(p2.1, tooltip = "text")
    
  })

#### Precip plots  
  output$PrecipPlot <- renderPlotly({
    
    #Revised data - column naming for plot
    precip.rev <- shiny.merged.precip %>% 
      rename(Year = year, `NOAA Precip` = noaa.precip,`McFarland Precip` = McFarland.precip) 
    
    #Base plot
    p3 <- precip.rev %>% 
      ggplot(., aes(x = Year)) +
      scale_x_continuous(breaks = pretty(precip.rev$Year)) +
      labs(title = "Total Precipitation (1895-2024)",
           x = "Year",
           y = "Total Precipitation (in)") +
      theme_minimal()
    
    #Add lines based on checkbox input
    if("NOAA Precip" %in% input$linesToShowPrecip && "NOAA Precip" %in% colnames(precip.rev)) {
      p3 <- p3 + geom_line(aes(y = `NOAA Precip`, color = "NOAA Total Precip."))#, alpha = 0.5) 
      # +
      #   geom_point(aes(
      #     y = `NOAA Precip`, 
      #     color = "NOAA Total Precip."),
      #     size = 0.8)
      
      # add linear model for noaa precip
      if ("lm_noaa_precip" %in% input$linesToShowPrecip) {
        p3 <- p3 + 
          geom_smooth(
            aes(y = `NOAA Precip`), 
            method = "lm",
            se = TRUE,
            fill = "grey80",   
            alpha = 0.5,       
            color = NA         
          ) +
          geom_line(           
            aes(y = `NOAA Precip`),
            stat = "smooth",
            method = "lm",
            color = "black",   
            linewidth = 0.8  
          )
      }
    }
    
    if("McFarland Precip" %in% input$linesToShowPrecip && "McFarland Precip" %in% colnames(precip.rev)) {
      p3 <- p3 + geom_line(aes(y = `McFarland Precip`, color = "McFarland Total Precip."))#, alpha = 0.5) 
      # +
      #   geom_point(aes(
      #     y = `McFarland Precip`, 
      #     color = "McFarland Total Precip."),
      #     size = 0.8)
      
      # add linear model for mcfarland precip
      if ("lm_mcfarland_precip" %in% input$linesToShowPrecip) {
        p3 <- p3 + 
          geom_smooth(
            aes(y = `McFarland Precip`), 
            method = "lm",
            se = TRUE,
            fill = "grey80",   
            alpha = 0.5,       
            color = NA         
          ) +
          geom_line(           
            aes(y = `McFarland Precip`),
            stat = "smooth",
            method = "lm",
            color = "black",   
            linewidth = 0.8  
          )
      }
    }
    
    # Customize the legend and colors
    p3 <- p3 + scale_color_manual(
      values = c(
        "NOAA Total Precip." = "#000000", 
        "McFarland Total Precip." = "#00CC00"
      ),
      name = "Precipitation Data"
    )
    
    # Convert ggplot2 plot to an interactive plotly plot
    ggplotly(p3, tooltip = c("Year", "NOAA Precip", "McFarland Precip"))
    
  })
  
}

#### shinyApp function (fuse ui and server)

shinyApp(ui, server)
