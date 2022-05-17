library(shiny)
library(tmap)
library(leaflet)

source("map-make.R")

# define the UI
ui <- fluidPage(
  titlePanel(
    h1("Self-Reported Health Behaviors During the COVID-19 Pandemic: A cross-cultural study")
  ), 
  leafletOutput("map"),
  selectInput("variable", "Health Behavior",
              c("Hand-washing" = "HandWash", "Hand-disinfecting" = "HandDisinfect", "Staying at home" = "StayHome", 
                "Using antibacterial products" = "Antibact", "Frequency of Mask-Wearing" ="MaskFreq", 
                "Avoiding public gatherings"= "AvoidPublic", "Stocking up on food and other resources" =" Stockup", 
                "Using alternative remedies"= "AltRem", "Covering own cough"= "CoverCough", "Social Distancing" ="SocDist"))
)
 
# define the server                  
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    # map(behavior)
    map(input$variable)
  })
}	
  
shinyApp(ui = ui, server = server)
