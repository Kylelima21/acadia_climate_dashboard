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

#-----------------------#
####    Read Data    ####
#-----------------------#

shiny.merged.temp <- read.csv("data/shiny_merged_temp.csv")

shiny.merged.precip <- read.csv("data/shiny_merged_precip.csv")

shiny.merged.anom <- read.csv("data/shiny_merged_anom.csv")

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
      menuItem("Long-Term Temperature Trends", tabName = "temp", icon = icon("chart-line")),
      menuItem("Long-Term Precipitation Trends", tabName = "precip", icon = icon("chart-line"))
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
                                "McFarland Average Temp." = "McFarland Average Temp"),
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
                      inputId = "linesToShow",
                      label = NULL,
                      choices = c("NOAA Total Precip." = "NOAA Precip",
                                  "McFarland Total Precip." = "McFarland Precip"),
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
    if("NOAA Average Temp" %in% input$linesToShow && "NOAA Average Temp" %in% colnames(temp.rev)) {
      p <- p + geom_line(aes(y = `NOAA Average Temp`, color = "NOAA Average Temp.")) +
        geom_point(aes(
          y = `NOAA Average Temp`, 
          color = "NOAA Average Temp."))
    }
    
    if("NOAA Average Max Temp" %in% input$linesToShow && "NOAA Average Max Temp" %in% colnames(temp.rev)) {
      p <- p + geom_line(aes(y = `NOAA Average Max Temp`, color = "NOAA Average Maximum Temp.")) +
        geom_point(aes(
          y = `NOAA Average Max Temp`, 
          color = "NOAA Average Maximum Temp."))
    }
    
    if("NOAA Average Min Temp" %in% input$linesToShow && "NOAA Average Min Temp" %in% colnames(temp.rev)) {
      p <- p + geom_line(aes(y = `NOAA Average Min Temp`, color = "NOAA Average Minimum Temp.")) +
        geom_point(aes(
          y = `NOAA Average Min Temp`, 
          color = "NOAA Average Minimum Temp."))
    }
    
    if("McFarland Average Temp" %in% input$linesToShow && "McFarland Average Temp" %in% colnames(temp.rev)) {
      p <- p + geom_line(aes(y = `McFarland Average Temp`, color = "McFarland Average Temp.")) +
        geom_point(aes(
          y = `McFarland Average Temp`, 
          color = "McFarland Average Temp."))
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
    ggplotly(p, tooltip = c("Year", "NOAA Average Temp", "NOAA Average Max Temp", "NOAA Average Min Temp", "McFarland Average Temp"))
    
  })
  
  output$PrecipPlot <- renderPlotly({
    
    #Revised data - column naming for plot
    precip.rev <- shiny.merged.precip %>% 
      rename(Year = year, `NOAA Precip` = noaa.precip,`McFarland Precip` = McFarland.precip) 
    
    #Base plot
    p2 <- precip.rev %>% 
      ggplot(., aes(x = Year)) +
      scale_x_continuous(breaks = pretty(precip.rev$Year)) +
      labs(title = "Total Precipitation (1895-2024)",
           x = "Year",
           y = "Total Precipitation (in)") +
      theme_minimal()
    
    #Add lines based on checkbox input
    if("NOAA Precip" %in% input$linesToShow && "NOAA Precip" %in% colnames(precip.rev)) {
      p2 <- p2 + geom_line(aes(y = `NOAA Precip`, color = "NOAA Total Precip.")) +
        geom_point(aes(
          y = `NOAA Precip`, 
          color = "NOAA Total Precip."))
    }
    
    if("McFarland Precip" %in% input$linesToShow && "McFarland Precip" %in% colnames(precip.rev)) {
      p2 <- p2 + geom_line(aes(y = `McFarland Precip`, color = "McFarland Total Precip.")) +
        geom_point(aes(
          y = `McFarland Precip`, 
          color = "McFarland Total Precip."))
    }
    
    # Customize the legend and colors
    p2 <- p2 + scale_color_manual(
      values = c(
        "NOAA Total Precip." = "#000000", 
        "McFarland Total Precip." = "#00CC00"
      ),
      name = "Precipitation Data"
    )
    
    # Convert ggplot2 plot to an interactive plotly plot
    ggplotly(p2, tooltip = c("Year", "NOAA Precip", "McFarland Precip"))
    
  })
  
}

#### shinyApp function (fuse ui and server)

shinyApp(ui, server)
