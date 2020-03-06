library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)

# rsconnect::deployApp('Data/corona/')

# Load data
tab = read.csv('data.csv')
d.time = read.csv('timeCourse.csv')
d.time$date = as.Date(d.time$date)
update = readLines('lastupdate')
# convert to case format
dd = tab[rep(1:nrow(tab), tab$cases), ]

# myConst = 100

# adjust colors
myColors = rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
myColors  = c("Reds", "Oranges", "Blues", "Greens", "Purples")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10, width = 220, draggable = TRUE, 
                wellPanel(tags$b("COVID-19 cases in Switzerland (per canton) and world countries."),
                          "Data are available",
                          tags$a(href = "https://github.com/schw4b/corona", 
                                 "here."),
                          "Data sources from",
                          tags$a(href = "https://www.swissinfo.ch/eng/covid-19_switzerland-confirms-second-coronavirus-case/45582788", 
                                 "swissinfo.ch"),
                          "and",
                          tags$a(href = "https://github.com/CSSEGISandData/COVID-19", 
                                 "JHU CSSE."), tags$br(),
                          paste("Last update:", update), 
                          style = "opacity: 0.80; font-size: 70%")
  ),
  
  absolutePanel(top = 120, right = 10, width = 220, draggable = TRUE, 
         
                  plotOutput("plot", height = "100px"),
                          style = "opacity: 0.80; font-size: 70%")
                
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    input$newplot
    # Add a little noise to the cars data
    myBreaks = log(c(10,50,100,250,500,1000,3000))
    ggplot(d.time, aes(x = date, y = log(cases), group=Country, col=Country)) +
      geom_line() + geom_point() + 
      scale_x_date(date_breaks = "4 day") +
      scale_y_continuous(breaks=myBreaks,
                         labels=exp(myBreaks)) + theme_minimal()+
      ylab("no. of cases") + xlab("March") + theme(legend.title = element_blank(), axis.text.x = element_blank())
    }, bg="#f0f0f0") 
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    # dd[dd$cases >= input$range[1] & dd$cases <= input$range[2],]
    dd

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
      # fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
      fitBounds(6.15, 46.01, 9.53, 47.56)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    #pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      # addCircles(radius = ~cases*myConst, weight = 1, color = "#777777",
      #            fillColor = ~pal(cases), fillOpacity = 0.7, popup = ~paste(cases, admin)
      # )
      addMarkers(
        clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE, zoomToBoundsOnClick = FALSE,
                                              removeOutsideVisibleBounds = TRUE,
                                              singleMarkerMode = TRUE)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = dd)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (FALSE) { # (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~cases
      ) 
    }
  })
}

shinyApp(ui, server)