#app is hosted & published on shinyapps.io 
#authorize computer to deploy applications to your shinyapps.io account. 


#deployApp() #this publishes the App or updates published App online
library(rsconnect)
rsconnect::setAccountInfo(name='ferriss', token='687BB500E2F21AEC9942DB5EE0A6EC7D',
                          secret='lUim37yc4hSHdjUFLUpQMrHPLzK9kLNl7w67GXv8')


library(devtools)
library(shiny)
library(leaflet)
library(dplyr)

ui<-fluidPage (
  #App title
  titlePanel("Noise Maps for Beacon Hill, Seattle (Spring/Summer 2018)"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    #Sidebar panel for inputs
    sidebarPanel (
    
      #Input: radio Buttons for noise metric selection
      radioButtons (inputId="metric" , label="Select noise metric for map", choices=list("LEQ24","LDN24"))
      
    ),
  
  #Main Panel for displaying outputs
  mainPanel(
    
  #Output Map
    leafletOutput(outputId="map",height = 1000) 
  )
)
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    data=read.csv("MapDatComb_20180927.csv") # Note I created this csv file in Excel by combining outputs from "noise_calcs.R" for LDN, LDEN, LEQ, %AboveThreshold, with lat and lon from Site ID file. [this could be coded in R]
    head(data)
    
    NoiseDat=as.data.frame(data)
    
    MapMetric=paste("NoiseDat$",input$metric,sep="")
   
     if(input$metric=="LEQ24"){
      value=NoiseDat$LEQ24
       } else if ( input$metric=="LDN24"){ 
         value=NoiseDat$LDN24
      }
    
    Longitude=NoiseDat$Lon
    Latitude=NoiseDat$Lat
    
    map1 <- leaflet(data = NoiseDat) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(data=NoiseDat, lng = ~Lon, lat = ~Lat,
                 popup = paste(input$metric,":",value))
    map1
  })
}

shinyApp(ui=ui, server=server)


  



