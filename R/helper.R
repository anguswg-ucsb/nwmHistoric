



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


basemap <- function() {
  # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addScaleBar("bottomleft") %>% 
    setView(-95,40,4) %>% 
    leafem::addMouseCoordinates()
}

second_map <- function() {
  # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addScaleBar("bottomleft") %>% 
    setView(-95,40,4) %>% 
    leafem::addMouseCoordinates()
}




make_ts <- function(comid) {
  nwm <- readNWMdata(comid = comid)
  ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
  dygraph(ts)  %>% 
  dyHighlight(highlightCircleSize = 2,
              highlightSeriesBackgroundAlpha = .3) %>%
    dyOptions(colors = c("darkcyan"),
              fillGraph = TRUE)
}


# 
# 
# leaflet(options = leafletOptions(preferCanvas = TRUE, 
#                                  updateWhenIdle = FALSE)) %>%
# 
#   setView(lon,lat,8) %>% 
#   addTiles(group = "OSM") %>% 
#   addProviderTiles(
#     "CartoDB.Positron",    group = "Grayscale") %>%
#   addProviderTiles(
#     "Esri.WorldImagery",   group = "Imagery") %>%
#   addWMSTiles(
#     "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
#     layers = "nexrad-n0r-900913",
#     options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = .15), 
#     group = "Rainfall"
#   ) %>% 
#   addTerminator(group = "daylight") %>% 
#   addLayersControl(
#     baseGroups = c("Grayscale", "OSM", "Imagery"), 
#     overlayGroups = c("Rainfall", "daylight"),
#     options = layersControlOptions(collapsed = TRUE),
#     position = 'topleft') %>% 
#   addScaleBar("bottomleft") %>%
#   addMiniMap( toggleDisplay = TRUE, minimized = TRUE) %>%
#   addMeasure(
#     position = "bottomleft",
#     primaryLengthUnit = "feet",
#     primaryAreaUnit = "sqmiles",
#     activeColor = "red",
#     completedColor = "green" ) %>% 
#   addEasyButtonBar(
#     easyButton(
#       icon='fa-crosshairs', title='Zoom to Risk Points',
#       onClick=JS(paste0("function(btn, map){ map.setView(new L.LatLng(",lat,", ", lon,"), 8);}")))
#   ) %>%
#   hideGroup(c("daylight", "Rainfall")) %>% 
#   leafem::addMouseCoordinates()




make_plots = function(x){
  
  vals = aggregate_ymd(x)
  
  p = ggplot(data = vals, aes(x = ymd, y = flow)) +
    geom_line(size = .1) +
    labs(x = 'Date', y = 'Streamflow (cms)') +
    theme_classic()
  
  return(p)
}

make_graph = function(df1, FIP){
  df1$dateTime <- xts::as.xts(df1$dateTime)
  rownames(df1) <- df1$dateTime
  
  ggplot(df1, aes(x = dateTime, y = flow_cms))+
    geom_bar(stat="identity")
  dygraph(data = df1,
          ylab = '',
          xlab = '') %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = .3,
                highlightSeriesOpts = list(strokeWidth = 3.5)) %>%
    
    dyOptions(colors = c("darkcyan"),
              drawGrid = FALSE,
              fillGraph = TRUE,
              strokeWidth = 3.5,
              stackedGraph = TRUE)
}
