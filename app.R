library(shiny)
library(leaflet)

universities = read.csv(file = "universities_per.csv")

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
    leafletOutput("map", height = "85%"),
    absolutePanel(
        bottom = 140, left = 10,
        sliderInput("range", "University Seniority (Lima, Peru)", 1550, 2021,
                    value = range(1550, 2020), step = 30 ),

        h5("Date range:"),
        textOutput("ranking"),
    ),
    h4("Instructions:"),
    h5(" Some Peruvian universities geolocations are shown as a blue circle, according to the date when they were founded."),
    h5(" Clicking on any of the blue circles displays the name of the university"),
    h5(" The slider allows to filter the universities. Also, the date range of the slider is mirrored in a texbox.")
)

server <- function(input, output, session) {
    filteredData = reactive({
        universities[universities$founded >= input$range[1] & universities$founded <= input$range[2],] })
    
    output$map = renderLeaflet({
        leaflet(universities) %>% addTiles() %>% 
            fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    })

    observe({
        leafletProxy("map", data = filteredData()) %>% clearShapes() %>% 
            addCircles(radius = ~10^4/60, weight = 12, color = "blue", 
                   fillOpacity = 0.1,
                   popup = ~paste(universities$university)
            )
    })
    output$ranking = renderText({ paste( input$range[1] ," - ",  input$range[2]) })
}

shinyApp(ui, server)
