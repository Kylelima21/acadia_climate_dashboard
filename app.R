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
                    choices = c("NOAA Average Temp." = "temp.noaa",
                                "NOAA Average Maximum Temp." = "max.noaa",
                                "NOAA Average Minimum Temp." = "min.noaa",
                                "McFarland Average Temp." = "mcfarland"),
                    selected = c("temp.noaa", "max.noaa", "min.noaa", "mcfarland")
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
    
    #Base plot
    p <- ggplot(shiny.merged.temp, aes(x = year)) +
             scale_x_continuous(breaks = pretty(shiny.merged.temp$year)) +
             labs(title = "Average Temperature (1895-2024)",
                  x = "Year",
                  y = "Temperature (°C)") +
             theme_minimal()
    
    #Add lines based on checkbox input
    if("temp.noaa" %in% input$linesToShow && "temp.noaa" %in% colnames(shiny.merged.temp)) {
      p <- p + geom_line(aes(y = temp.noaa, color = "NOAA Average Temp.")) +
        geom_point(aes(
          y = temp.noaa, 
          color = "NOAA Average Temp.",
          text = paste("Year:", year, "<br>Average Temp:", round(temp.noaa, 2))))
    }
    
    if("max.noaa" %in% input$linesToShow && "max.noaa" %in% colnames(shiny.merged.temp)) {
      p <- p + geom_line(aes(y = max.noaa, color = "NOAA Average Maximum Temp.")) +
        geom_point(aes(
          y = max.noaa, 
          color = "NOAA Average Maximum Temp.",
          text = paste("Year:", year, "<br>Average Max Temp:", round(max.noaa, 2))))
    }
    
    if("min.noaa" %in% input$linesToShow && "min.noaa" %in% colnames(shiny.merged.temp)) {
      p <- p + geom_line(aes(y = min.noaa, color = "NOAA Average Minimum Temp.")) +
        geom_point(aes(
          y = min.noaa, 
          color = "NOAA Average Minimum Temp.",
          text = paste("Year:", year, "<br>Average Min Temp:", round(min.noaa, 2))))
    }
    
    if("mcfarland" %in% input$linesToShow && "mcfarland" %in% colnames(shiny.merged.temp)) {
      p <- p + geom_line(aes(y = mcfarland, color = "McFarland Average Temp.")) +
        geom_point(aes(
          y = mcfarland, 
          color = "McFarland Average Temp.",
          text = paste("Year:", year, "<br>McFarland Average Temp:", round(mcfarland, 2))))
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
    ggplotly(p, tooltip = "text")
    
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
