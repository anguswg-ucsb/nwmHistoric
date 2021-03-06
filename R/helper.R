


# 
# library(shiny)
# library(leaflet)
# library(sf)
# library(ggplot2)
# library(dplyr)
# library(shinycustomloader)
# library(shinydashboard)
# library(dataRetrieval)
# library(nhdplusTools)
# library(nwmHistoric)
# library(dygraphs)
# library(DT)
# library(formattable)


make_ts2 <- function(comid) {
  nwm <- readNWMdata(comid = comid)
  plotly::plot_ly(
    nwm, x = ~dateTime,
    y = ~flow_cms, type = "scatter", 
    mode = "lines")
}

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
  nwm <- readNWMdata(comid = comid) %>% 
    head(10000)
  ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
  dygraph(ts)  %>% 
  dyHighlight(highlightCircleSize = 2,
              highlightSeriesBackgroundAlpha = .3) %>%
    dyOptions(colors = c("darkcyan"),
              fillGraph = TRUE)
}

make_ts3 <- function(comid, startDate, endDate) {
f = ts[NULL]
  nwm <- readNWMdata(comid = comid, startDate = NULL, endDate = NULL)
    ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
    dygraph(ts)  %>% 
    dyHighlight(highlightCircleSize = 2,
                highlightSeriesBackgroundAlpha = .3) %>%
    dyOptions(colors = c("darkcyan"),
              fillGraph = TRUE)

    # ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
    # dygraph(ts)  %>% 
    #   dyHighlight(highlightCircleSize = 2,
    #             highlightSeriesBackgroundAlpha = .3) %>%
    #   dyOptions(colors = c("darkcyan"),
    #           fillGraph = TRUE)
  
}




make_ts4 <- function(nwm) {
  nwm <- nwm 
  ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
  dygraph(ts)  %>% 
    dyHighlight(highlightCircleSize = 2,
                highlightSeriesBackgroundAlpha = .3) %>%
    dyOptions(colors = c("darkcyan"),
              fillGraph = TRUE) %>% 
    dyRangeSelector()
  
  #if(is.null(range)) {
  # nwm <- nwm %>% 
  #   head(10000)
  # ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
  # dygraph(ts)  %>% 
  #   dyHighlight(highlightCircleSize = 2,
  #               highlightSeriesBackgroundAlpha = .3) %>%
  #   dyOptions(colors = c("darkcyan"),
  #             fillGraph = TRUE) %>% 
  #   dyRangeSelector()
  # } else {
  # nwm <- nwm %>% 
  #   head(10000)
  # ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
  # ts2 <- ts[as.character(range)]
  # dygraph(ts2)  %>% 
  #   dyHighlight(highlightCircleSize = 2,
  #               highlightSeriesBackgroundAlpha = .3) %>%
  #   dyOptions(colors = c("darkcyan"),
  #             fillGraph = TRUE) %>% 
  #   dyRangeSelector()
  }
  # ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
  # dygraph(ts)  %>% 
  #   dyHighlight(highlightCircleSize = 2,
  #             highlightSeriesBackgroundAlpha = .3) %>%
  #   dyOptions(colors = c("darkcyan"),
  #           fillGraph = TRUE)
make_ts6 <- function(nwm, date) {
  nwm <- readNWMdata(comid = 101)
  ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')

  dygraph(ts)  %>% 
    dyHighlight(highlightCircleSize = 2,
                highlightSeriesBackgroundAlpha = .3) %>%
    dyOptions(colors = c("darkcyan"),
              fillGraph = TRUE) %>% 
    dyRangeSelector(dateWindow = c(1996-01-01, 1995-6-12))
}
# make_ts6(nwm, date = '1996-01-01/1996-6-12')
# make_ts6(nwm, date = NULL)
make_ts5 <- function(ts) {
    ts <- ts
    dygraph(ts)  %>% 
      dyHighlight(highlightCircleSize = 2,
                  highlightSeriesBackgroundAlpha = .3) %>%
      dyOptions(colors = c("darkcyan"),
                fillGraph = TRUE) %>% 
      dyRangeSelector()
}

make_table <- function(comid) {
  nwm <- readNWMdata(comid = comid)
  
  
  nwm$flow_cms <- round(nwm$flow_cms, 2)
  
  nwm <- head(nwm, 300)
  
  nwm <- rename(nwm, Date = "dateTime",
                "Flow rate (C/m/s)" = "flow_cms",
                COMID = comid,
                Model = model)
  
  as.datatable(formattable(nwm, align = c("l", rep("r", NCOL(nwm) - 1)),
                                        list(`Model` = formatter("span", style = ~ style(font.weight = "bold")),
                                             # `comid` = formatter("span", style = ~ style(border = "black", background = "gold", font.weight = "bold")),
                                             `COMID` = color_bar("honeydew"),
                                             
                                             `Date` = formatter("span", style = ~ style(font.weight = "bold")),
                                             `Flow rate (C/m/s)` = color_tile("azure1", "cadetblue4"))),
               options = list(paging = TRUE, searching = TRUE))

}


make_table2 <- function(comid) {
  
  catch_df <- get_nhdplus(comid = comid, realization = "catchment")

  conus <- USAboundaries::us_counties() %>% 
    select(name, state_name)

  tmp1 <- st_intersection(catch_df, conus) %>% 
    st_drop_geometry()
  
  tmp1$areasqkm <- round(tmp1$areasqkm, 2)
  
  tmp1 <- tmp1 %>% 
    select(COMID = featureid,
           "Area (sq. km)" = areasqkm,
           State = state_name,
           County = name) %>% 
    slice(n=1) %>% 
    mutate(across(1:4, as.character)) %>% 
    tidyr::pivot_longer(1:4, names_to = " ", values_to = "  ")

  
  knitr::kable(tmp1, col.names = c('', " "), booktabs= T,
      table.attr ='class="table" style="color: black"', escape = F, align = "c") %>%
    kableExtra::kable_classic("striped") %>% 
    # kableExtra::kable_material(c("striped", "hover")) %>% 
    kableExtra::kable_styling(position = "center") %>% 
    kableExtra::column_spec(1, background = "#BCD4E6") %>% 
    kableExtra::column_spec(2, background = "azure")
  
  
}
  
# as.datatable(formattable(tmp1, align = c("l", rep("r", NCOL(nwm) - 1)),
#                          list(`Model` = formatter("span", style = ~ style(font.weight = "bold")),
#                               # `comid` = formatter("span", style = ~ style(border = "black", background = "gold", font.weight = "bold")),
#                               `COMID` = color_bar("honeydew"),
#                               
#                               `Date` = formatter("span", style = ~ style(font.weight = "bold")),
#                               `Flow rate (C/m/s)` = color_tile("azure1", "cadetblue4"))),
#              options = list(paging = TRUE, searching = TRUE))


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
