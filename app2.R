
library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(shinycustomloader)
library(shinydashboard)
library(dataRetrieval)
library(nhdplusTools)
library(nwmHistoric)
library(dygraphs)
library(plotly)
source('R/helper.R')

# 
# pt <- data.frame(lng = -91.0106, lat = 32.23325)
# nldi <- findNLDI(location = pt)
# pt$comid <- nldi$comid
# 
# nhd_sub <- get_nhdplus(comid = 100, realization = "catchment")
# nwm_df = readNWMdata(comid = 101)

# rownames(nwm_df) = nwm_df$dateTime

# make_ts <- function(pt) {
#   nwm <- readNWMdata(comid = pt$comid)
#   ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
#   dygraph(ts)
# }

basemap <- basemap()
second_map <- second_map()

comid <- reactiveValues(comid = NULL)
dateTSInput <- reactive(input$dateTS)
reactiveValues(pt)

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(width = 7,
             
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("catchMap", height = 650))
      ),
      column(width = 4,
             box(width = NULL, solidHeader = TRUE,
                 plotlyOutput("catchTS"),
                 # box(title = "Controls",
                 #     dateRangeInput ("date_range", 
                 #         start="2010-01-01",
                 #         end="2015-01-01",
                 #        # start = nwm$dateTime[1],
                 #        # end = tail(nwm$dateTime, n =1),
                 #        label = h3("Date range")))))
                 box(width = NULL, solidHeader = TRUE,
                     leafletOutput("catchMap_2")),
                 dateRangeInput("dateTS", label = "range",
                                start = "1993-01-01",
                                end = "2018-12-31"),
                 verbatimTextOutput("dateTSText")
                 
             )
      )
    )))




# ui <- fluidPage(
#   
#   titlePanel('National Water Model'),
#   
#   sidebarPanel(
#     textOutput("catchMessage", container = h3)
#   ),
#   
#   mainPanel(
#     leafletOutput('catchMap'),
#     dygraphOutput("catchTS")
#   )

server <- function(input, output) {
  
  k   <- reactiveValues(txt = "Catchment COMID: ")
  
  output$catchMap     <- renderLeaflet({ basemap })
  output$catchMap_2     <- renderLeaflet({ second_map })
  # output$catchTS      <- renderDygraph( {make_ts(pt)} )
  output$catchTS      <- renderPlotly( {
    nwm <- readNWMdata(comid = comid)
    plotly::plot_ly(rangeTS(),
      nwm, x = ~dateTime,
      y = ~flow_cms, type = "scatter", 
      mode = "lines")
  } )
  
  output$catchMessage <- renderText(k$txt)
 
  rangeTS <- reactive({
    range <-  filter(date >= as.Date(input$dateTS[1]) & date < as.Date(input$dateTS[2]))
  })
  
  output$dateTSText <- renderText({
    paste(as.character(input$dateTS[1]), as.character(input$dateTS[2]), sep = "/")
  })
  
  
  
  observeEvent(input$catchMap_click, {
    # pt <<- NULL
    click <<- input$catchMap_click %>% 
      data.frame() %>% 
      dplyr::select(lat,lng)
    
    print(click)
    pt <- sf::st_as_sf(click, 
                       coords = c("lng", "lat"), 
                       crs = 4326)
    
    
    nldi <- findNLDI(location = pt)
    
    comid = nldi$comid
    print(pt$comid)
    # output$catchTS <- renderDygraph({ make_ts(comid) })
    output$catchTS <- renderPlotly( {
      nwm <- readNWMdata(comid = comid)
      plotly::plot_ly(rangeTS(),
                      nwm, x = ~dateTime,
                      y = ~flow_cms, type = "scatter", 
                      mode = "lines")
    })
    catch_df <- get_nhdplus(comid = comid, realization = "catchment")
    leafletProxy("catchMap") %>% 
      clearMarkers() %>%
      clearShapes() %>% 
      addMarkers(data = pt, 
                 layerId = ~comid) %>% 
      addPolygons( data = catch_df,
                   col = 'blue')
    leafletProxy("catchMap_2") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(data = pt,
                 layerId = ~comid) %>%
      addPolygons( data = catch_df,
                   col = 'blue')
  })
  
  observe({ # Observer to respond to zoom / pan of map1 and apply to map2
    coords <- input$catchMap_center
    zoom <- input$catchMap_zoom
    print(coords)
    print(zoom)
    if (!is.null(coords)) {
      leafletProxy("catchMap_2") %>%
        setView(lat = coords$lat, lng = coords$lng, zoom = zoom)
    }
  })
}



shinyApp(ui, server)
