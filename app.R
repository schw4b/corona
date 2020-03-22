library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)

# rsconnect::deployApp('~/Data/corona/')
# setwd('~/Data/corona/')

# Load data
tab = read.csv('data.csv')

d.time = read.csv('timeCourse.csv')
d.time$date = as.Date(d.time$date)
# reorder levels
tmp = d.time[( nrow(d.time) - nlevels(d.time$Country) + 1):nrow(d.time),]
d.time$Country = factor(d.time$Country, levels = tmp$Country[order(tmp$cases, decreasing = TRUE)])

update = readLines('lastupdate')
# convert to case format
dd = tab[rep(1:nrow(tab), tab$cases), ]

# myConst = 100

# adjust colors
myColors = rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
myColors  = c("Reds", "Oranges", "Blues", "Greens", "Purples")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "120%"),

  absolutePanel(top = 10, right = 10, width = 280, draggable = TRUE, 
                tags$b("COVID-19 cases in Switzerland (per canton) and world countries."),
                          "Data are available",
                          tags$a(href = "https://github.com/schw4b/corona", 
                                 "here."),
                          "Data sources from",
                          tags$a(href = "https://github.com/openZH/covid_19", 
                                 "open.zh.ch"),
                          "and",
                          tags$a(href = "https://github.com/CSSEGISandData/COVID-19", 
                                 "JHU CSSE."),
                          paste("Last update:", update), 
                          style = "opacity: 0.70; font-size: 70%; background-color: #f0f0f0"),
  
  bootstrapPage(
    absolutePanel(top = 55, right = 10, width = 280, draggable = TRUE, 
                  HTML('<button data-toggle="collapse" data-target="#fig">Show/hide figures</button>'),
                  tags$div(id = 'fig',  class="collapse",
                           plotOutput("plot_cases", height = "150px"),
                           plotOutput("plot_rate", height = "150px"),
                           style = "opacity: 0.90; font-size: 70%")
    ))
                
)

server <- function(input, output, session) {
  
  output$plot_cases <- renderPlot({
    input$newplot
    
    # --- code for plot cases ---
    myBreaks = log(c(10,20,50,100,200,500,1000,2000,5000,10000,20000,40000))
    ggplot(d.time, aes(x = date, y = log(cases), group=Country, col=Country)) +
      geom_point(size=1.5, shape=1) +
      geom_line(size=0.8) +
      # geom_smooth(method = "loess", se = FALSE, size=0.8) +
      # geom_smooth(method = "glm", formula=y ~ poly(x, 5), se = FALSE, size=0.8) +
      scale_x_date(breaks=c(min(d.time$date), median(d.time$date)-0.5, 
                            max(d.time$date)), date_labels = "%d %b") +
      scale_y_continuous(breaks=myBreaks,
                         labels=exp(myBreaks)) +
      theme_minimal() + ylab("total cases") + xlab("day") +
      theme(legend.title = element_blank(), 
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.6, 'lines'),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(colour = "gray80", linetype = "solid"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(colour = "gray80", linetype = "solid")
      ) + scale_color_brewer(palette="Dark2")
    # --- end code for plot ---
    
    }, bg="#f0f0f0") 
  
  output$plot_rate <- renderPlot({
    input$newplot
    
    # --- code for plot rate ---
    d.time_ = subset(d.time, subset = !is.na(rate))
    myBreaks = log(c(5,20,50,100,300,800,2000,5000))
    ggplot(d.time_, aes(x = date, y = log(rate), group=Country, col=Country)) +
      geom_point(size=1.5, shape=1, position = position_jitter(width = 0.2, height = 0)) +
      geom_smooth(method = "glm", formula=y ~ poly(x, 5), se = FALSE, size=0.8) +
      #geom_smooth(method = "loess", se = FALSE, size=0.8) +
      scale_x_date(breaks=c(min(d.time_$date), median(d.time_$date), 
                            max(d.time_$date)), date_labels = "%d %b") +
      scale_y_continuous(breaks=myBreaks,
                         labels=exp(myBreaks)) +
      theme_minimal() + ylab("cases per day") + xlab("day") +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.6, 'lines'),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(colour = "gray80", linetype = "solid"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(colour = "gray80", linetype = "solid")
      ) + scale_color_brewer(palette="Dark2")
    # --- end code for plot ---
    
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