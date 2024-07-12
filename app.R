library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  
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