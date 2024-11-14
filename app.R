library(shiny)
library(shinydashboard)
library(fresh)
library(ggplot2)
library(readr)
library(plotly)

#load data
shiny.merged.temp <- read.csv("data/shiny_merged_temp.csv")

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
      menuItem("Long-Term Temperature Trends", tabName = "plot", icon = icon("chart-line"))
     )
    ),
    
  
  dashboardBody(
    tags$head(tags$link(type = "text/css", rel = "stylesheet", href = "css/style.css")),
    
    # Define tab items
    tabItems(
      # Tab for Dashboard Overview (can add content here later)
      tabItem(tabName = "overview",
              h2("Dashboard Overview"),
              p("Content for the dashboard overview can go here.")
      ),
      
      # Tab for Interactive Plot
      tabItem(tabName = "plot",
              
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
        )
      )
    )
  )

server <- function(input, output) {
  output$myInteractivePlot <- renderPlotly({
    
    #Base plot
    p <- ggplot(shiny.merged.temp, aes(x = year)) +
             scale_x_continuous(breaks = pretty(merged.temp.noaa.McFarland$year)) +
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
          text = paste("Year:", year, "Average Temp:", temp.noaa)))
    }
    
    if("max.noaa" %in% input$linesToShow && "max.noaa" %in% colnames(shiny.merged.temp)) {
      p <- p + geom_line(aes(y = max.noaa, color = "NOAA Average Maximum Temp.")) +
        geom_point(aes(
          y = max.noaa, 
          color = "NOAA Average Maximum Temp.",
          text = paste("Year:", year, "Average Max Temp:", max.noaa)))
    }
    
    if("min.noaa" %in% input$linesToShow && "min.noaa" %in% colnames(shiny.merged.temp)) {
      p <- p + geom_line(aes(y = min.noaa, color = "NOAA Average Minimum Temp.")) +
        geom_point(aes(
          y = min.noaa, 
          color = "NOAA Average Minimum Temp.",
          text = paste("Year:", year, "Average Min Temp:", min.noaa)))
    }
    
    if("mcfarland" %in% input$linesToShow && "mcfarland" %in% colnames(shiny.merged.temp)) {
      p <- p + geom_line(aes(y = mcfarland, color = "McFarland Average Temp.")) +
        geom_point(aes(
          y = mcfarland, 
          color = "McFarland Average Temp.",
          text = paste("Year:", year, "McFarland Average Temp:", mcfarland)))
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
  
}


shinyApp(ui, server)
