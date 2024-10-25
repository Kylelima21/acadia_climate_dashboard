library(shiny)
library(shinydashboard)
library(fresh)


ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Acadia Climate Dashboard",
    titleWidth = 325
    ),
  
  dashboardSidebar(
    width = 325
  ),
  
  dashboardBody(
    tags$head(tags$link(type = "text/css", rel = "stylesheet", href = "css/style.css"))
  )
  
)


server <- function(input, output) {
  
}


shinyApp(ui, server)