library(shiny)
library(leaflet)
library(RColorBrewer)

# create data sheet
# dd = read.csv(file = '~/Data/corona/ch.csv')
# dd = dd[!duplicated(dd$admin),c("city", "lat", "lng", "admin", "population")]
# dd$cases = 0
# write.csv(dd, file="~/Data/corona/data.csv")
# dd$cases = sample(10:20, nrow(dd), replace = TRUE)

dd = read.csv(file = 'data.csv')
myConst = 5000

# adjust colors
myColors = rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
myColors[1]  = "OrRd"
myColors[17] = "BrBG"

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Cases", min(dd$cases), max(dd$cases),
                            value = range(dd$cases), step = 1
                ),
                selectInput("colors", "Color Scheme",
                            myColors
                            # put OrRd at first
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    dd[dd$cases >= input$range[1] & dd$cases <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, dd$cases)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(dd) %>% addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~cases*myConst, weight = 1, color = "#777777",
                 fillColor = ~pal(cases), fillOpacity = 0.7, popup = ~paste(cases, admin)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = dd)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~cases
      ) 
    }
  })
}

shinyApp(ui, server)