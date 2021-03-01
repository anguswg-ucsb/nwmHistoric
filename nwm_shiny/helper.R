


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


# USGS Water use data
water_use <- dataRetrieval::readNWISuse(stateCd = "CA", countyCd = "Contra Costa") %>% 
  janitor::clean_names()

water_use <- water_use %>% 
  select(1:6, contains("total_self_supplied_withdrawals_surface_water"), contains("surface_water_withdrawals_for_golf"))

water_use <- water_use %>% 
  mutate(across(5:22, as.numeric)) %>% 
  replace(is.na(.), 0)

water_use <- water_use %>% 
  rename(statefips = state_cd,
         countyfips = county_cd,
         pop = total_population_total_population_of_area_in_thousands,
         livestock1 = livestock_stock_total_self_supplied_withdrawals_surface_water_in_mgal_d,
         livestock2 = livestock_animal_specialties_total_self_supplied_withdrawals_surface_water_in_mgal_d,
         therm1 = thermoelectric_power_closed_loop_cooling_total_self_supplied_withdrawals_surface_water_in_mgal_d,
         therm2 = thermoelectric_power_once_through_cooling_total_self_supplied_withdrawals_surface_water_in_mgal,
         aquacultere1 = aquaculture_total_self_supplied_withdrawals_surface_water_in_mgal_d, 
         irrigation1 = irrigation_total_total_self_supplied_withdrawals_surface_water_in_mgal_d,
         irrigation2 = irrigation_golf_courses_self_supplied_surface_water_withdrawals_for_golf_courses_fresh_in_mgal_d) 

for ( col in 1:ncol(water_use)){
  colnames(water_use)[col] <-  sub("_.*", "", colnames(water_use)[col])
}

water_use <- water_use %>% 
  mutate(public_supply = public + domestic,
         thermoelectric = total,
         irrigation = livestock1 + livestock2 + aquacultere1 + irrigation1 + irrigation2) %>% 
  select(1:6, public_supply, irrigation, industrial, mining,  thermoelectric)

water_use <- water_use %>% 
  tidyr::unite('fips', statefips, countyfips) 

water_use$fips <- gsub("_", "", water_use$fips)
water_use$fips <- as.numeric(water_use$fips)




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
    addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldShadedRelief, group = "Relief") %>%
    addLayersControl(options = layersControlOptions(collapsed = FALSE),
                     baseGroups = c('Topographic', "Relief")) %>% 
    addScaleBar("bottomleft") %>%
    setView(-95,40,4) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green" ) %>%
    leafem::addMouseCoordinates()
}

second_map <- function() {
  # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addScaleBar("bottomleft") %>% 
    setView(-95,40,4) %>% 
    addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "feet",
        primaryAreaUnit = "sqmiles",
        activeColor = "red",
        completedColor = "green" ) %>% 
    leafem::addMouseCoordinates()
}



#
# US census API key
# census_api_key("e35e9a066fde271fb3779cceb453bebc82fa3498",overwrite=TRUE, install = TRUE)
# Sys.getenv("CENSUS_API_KEY")
# readRenviron("~/.Renviron")
pop_map <- function(pop_df) {
  pop <- get_acs(geography = "county",
                 variables = "B01003_001",
                 geometry = TRUE)
  
  pop$area <- st_area(pop)
  
  pop$area <- units::set_units(pop$area, "mi^2")
  pop <- rename(pop, population = estimate)
  pop <- pop %>%
    mutate(pop_density = population/area)
  pop <- pop %>% filter(!grepl("^02", GEOID))
  pop <- pop %>% filter(!grepl("^15", GEOID))
  pop <- st_transform(pop, 4326)
  
  pal <- colorQuantile(palette = "viridis", domain = pop$pop_density, n = 10)
  pal <- colorQuantile(palette = "viridis", domain = pop$pop_density, n = 10)
  leaflet(data = pop_df) %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(stroke = TRUE,
                weight = 0.3,
                smoothFactor = 0,
                fillOpacity = 0.6,
                color = ~ RColorBrewer::pal(pop_density)) %>%
    addLegend("bottomright",
                pal = pal,
                values = ~ pop_density,
                title = "Population percentiles",
                opacity = 1) %>%
    leafem::addMouseCoordinates()
}


# pop <- get_acs(geography = "county",
#                      variables = "B01003_001",
#                      geometry = TRUE)
# 
# pop$area <- st_area(pop)
# 
# pop$area <- units::set_units(pop$area, "mi^2")
# pop <- rename(pop, population = estimate)
# pop <- pop %>%
#   mutate(pop_density = population/area)
# pop <- pop %>% filter(!grepl("^02", GEOID))
# pop <- pop %>% filter(!grepl("^15", GEOID))
# pop <- st_transform(pop, 4326)
# 
# #intersect clicked catchment with county polygons
# nldi <- findNLDI(location = pt) %>% 
#   st_transform(5070)
# buff <- st_buffer(nldi, 50000) %>% 
#   st_transform(4326)
# overlap_counties <- st_filter(pop, buff, .predicate = st_intersects)
# 
# # Pop density leaflet
# pal <- colorQuantile(palette = "viridis", domain = pop$pop_density, n = 10)



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




make_ts4 <- function(ts) {
  # nwm <- nwm 
  # ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
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


zoom_to_catch = function(map, df, catchment){
  # Filter the counties to the input FIP code
  shp = filter(df, comid == catchment)
  # Build a buffered bounding box to center the map on:
  bounds = shp %>%
    # make bounding box
    st_bbox() %>%
    # Make spatial
    st_as_sfc() %>%
    # Buffer to .1 degree
    st_buffer(.1) %>%
    # make new bounding box
    st_bbox() %>%
    # extract coordinates as vector
    as.vector()
  # Clear all current shapes (remember county centroids are currently markers!)
  clearShapes(map) %>%
    # Add the county shape making the outline color red and the fill an opaque white
    addPolygons(data = shp,
                color = "red",
                fillColor  = "grey",
                fillOpacity = .3) %>%
    # Fly the leaflet map to the buffered boundary
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}

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
