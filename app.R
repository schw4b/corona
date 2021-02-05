library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)

# rsconnect::deployApp('~/Data/corona/')
# setwd('~/Data/corona/')

# Load data
tab = read.csv('data.csv')
tab.time = read.csv('data_time.csv')
tab.time$date = as.Date(tab.time$date)

update = readLines('lastupdate')
# convert to case format
dd = tab[rep(1:nrow(tab), tab$rate), ]

# prepare plot data
tab.plot = tab.time[grep("Switzerland|Germany|Austria|Italy|Japan|US|United Kingdom|Brazil", tab.time$country),]
tab.plot = tab.plot[tab.plot$date >= as.Date("2020-03-01"),] # start 1 March

# order levels
tmp = tab.plot[order(tab.plot$cases, decreasing = TRUE),]
tmp = tmp[!duplicated(tmp$country),]
tab.plot$country = factor(tab.plot$country, levels =  tmp$country)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "120%"),
  
  absolutePanel(top = 10, right = 10, width = 280, draggable = TRUE, 
                tags$b("Yesterdays newly reported COVID-19 cases in Switzerland and the world."),
                "Data are available",
                tags$a(href = "https://github.com/schw4b/corona", 
                       "here."),
                "Data sources from",
                #tags$a(href = "https://github.com/openZH/covid_19", "open.zh.ch"),
                #"and",
                tags$a(href = "https://github.com/CSSEGISandData/COVID-19", 
                       "JHU CSSE."),
                paste("Last update:", update), 
                style = "opacity: 0.70; font-size: 70%; background-color: #f0f0f0"),
  
  bootstrapPage(
    absolutePanel(top = 55, right = 10, width = 280, draggable = TRUE, 
                  HTML('<button data-toggle="collapse" data-target="#fig">Show/hide figures</button>'),
                  tags$div(id = 'fig',  class="collapse",
                           plotOutput("plot_cases", height = "180px"),
                           plotOutput("plot_rate", height = "180px"),
                           style = "opacity: 0.90; font-size: 70%")
    ))
  
)

server <- function(input, output, session) {
  
  output$plot_cases <- renderPlot({
    input$newplot
    
    # --- code for plot cases ---
    COLORS20 = c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', 
                 '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabed4', 
                 '#469990', '#dcbeff', '#9A6324', '#fffac8', '#800000', 
                 '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9')
    myBreaks = log(c(10,100,1000,10000,100000,10^6,10^7,5*10^7))
    myLabels = c("10", "100", "1000", "10k", "100k", "1mio", "10mio", "50mio")
    ggplot(tab.plot, aes(x = date, y = log(cases), group=country, col=country)) +
      geom_line(size=0.8, alpha = 0.8) +
      scale_x_date(breaks=c(min(tab.plot$date), as.Date("2020-05-01"), as.Date("2020-07-01"), as.Date("2020-09-01"), 
                            as.Date("2020-11-01"), as.Date("2021-01-01"), max(tab.plot$date)), date_labels = "%d %b") +
      scale_y_continuous(breaks=myBreaks,
                         labels=myLabels) +
      theme_minimal() + ylab("total cases") + xlab("day") +
      theme(legend.title = element_blank(), 
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.6, 'lines'),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(colour = "gray80", linetype = "solid"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(colour = "gray80", linetype = "solid")
      ) + scale_colour_manual(values=COLORS20)
    # --- end code for plot ---
    
  }, bg="#f0f0f0") 
  
  output$plot_rate <- renderPlot({
    input$newplot
    
    # --- code for plot rate ---
    COLORS20 = c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', 
                 '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabed4', 
                 '#469990', '#dcbeff', '#9A6324', '#fffac8', '#800000', 
                 '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9')
    tab.plot_ = subset(tab.plot, subset = !is.na(rate) & rate > 0)
    myBreaks = log(c(10,20,50,100,200,500,1000, 2000,5000,10000,20000,50000,100000, 2*10^5))
    ggplot(tab.plot_, aes(x = date, y = log(rate), group=country, col=country)) +
      geom_line(aes(y=log(ma))) +
      # geom_point(size=0.5, shape=1, position = position_jitter(width = 0, height = 0), alpha = 0.5) +
      scale_x_date(breaks=c(min(tab.plot$date), as.Date("2020-05-01"), as.Date("2020-07-01"), as.Date("2020-09-01"), 
                            as.Date("2020-11-01"), as.Date("2021-01-01"), max(tab.plot$date)), date_labels = "%d %b") +
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
      ) + scale_colour_manual(values=COLORS20)
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
    colorNumeric(input$colors, dd$rate)
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
        clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE,
                                              zoomToBoundsOnClick = FALSE,
                                              removeOutsideVisibleBounds = TRUE,
                                              singleMarkerMode = TRUE,
                                              showCoverageOnHover = FALSE
        )
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
                          pal = pal, values = ~rate
      ) 
    }
  })
}

shinyApp(ui, server)