


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





# library(climateR)

# ### TEST POINT FOR CLIMATE DATA ####
# lat = 35.6643
# lng = -96.91935
# pt <- data.frame(lat, lng)
# pt <- sf::st_as_sf(pt,
#                    coords = c("lng", "lat"),
#                    crs = 4326)
# nldi <- findNLDI(location = pt)
# nldi <- st_centroid(nldi)
# 
# evap <- climateR::getTerraClim(AOI = nldi, param = "aet",
#                                 startDate = "1993-01-01",
#                                 endDate = "2015-01-01")
# 
# 
# 
# # evap2$date <- zoo::as.yearmon(evap2$date,"%Y-%m")
# # evap2$date <- as.Date(paste0(evap2$date, "-01"))
# 
# evap$date <- paste0(evap$date, "-01")
# evap$date <- as.Date(evap$date)
# 
# rownames(evap) <- evap$date
# 
# evap <- select(evap,aet)
# 
# dygraph(data = evap)

# 
# highchart() %>%
#   hc_yAxis_multiples(list(title = list(text = "Accumulated Precipitation (mm)"),
#                           min=0,
#                           max = max(terra$prcp),
#                           showFirstLabel = TRUE,
#                           showLastLabel = TRUE,
#                           opposite = FALSE),
#                      list(title = list(text = "Actual Evapotranspiration (mm)"),
#                           showLastLabel=FALSE,
#                           opposite = TRUE)) %>%
#   hc_add_series(terra, type = "column",
#                 hcaes(x = date, y = prcp), yAxis = 1 ) %>%
#   hc_add_series(terra, type = "line",
#                 hcaes(x = date, y = aet), yAxis = 0) %>%
#   hc_colors(c("darkcyan", "darkred")) %>% 
#   hc_size(height = 450)
# nwm <- readNWMdata(comid = 17595443)
# 
# max_flow <- nwm %>% 
#   mutate(days = as.Date(dateTime)) %>% 
#   group_by(days) %>% 
#   summarize(total_flows = sum(flow_cms)) %>% 
#   arrange(-total_flows) %>% 
#   slice(n =1)
# 
# min_flow <- nwm %>% 
#   mutate(days = as.Date(dateTime)) %>% 
#   group_by(days) %>% 
#   summarize(total_flows = sum(flow_cms)) %>% 
#   arrange(total_flows) %>% 
#   slice(n =1)
make_table <- function(comid) {
  nwm <- readNWMdata(comid = comid)
  nwm$flow_cms <- round(nwm$flow_cms, 2)
  nwm <- head(nwm, 300)
  nwm <- rename(nwm, Date = "dateTime",
                "Flow rate (C/m/s)" = "flow_cms",
                COMID = comid,
                Model = model)
  as.datatable(formattable(tmp2, align = c("l", rep("r", NCOL(tmp2) - 1)),
                           list(`Model` = formatter("span", style = ~ style(font.weight = "bold")),
                                # `comid` = formatter("span", style = ~ style(border = "black", background = "gold", font.weight = "bold")),
                                `COMID` = color_bar("honeydew"),
                                `Date` = formatter("span", style = ~ style(font.weight = "bold")),
                                `Flow rate (C/m/s)` = color_tile("azure1", "cadetblue4"))),
               options = list(paging = TRUE, searching = TRUE))
  
}

make_table3 <- function(comid) {
  catch_df <- get_nhdplus(comid = comid, realization = c("catchment", "flowline"))
  catch_df <- bind_rows(catch_df) %>% 
    select(1:3, 5:7, 13, 53:55)
  catch_df <- catch_df %>% 
    mutate(str_length = lengthkm[2], max_elev = maxelevsmo[2], min_elev = minelevsmo[2], slope2 = slope[2]) %>% 
    slice(n = 1) %>% 
    select(1:6, 11:14)
  conus <- us_counties() %>% 
    select(name, state_name)
  tmp1 <- st_intersection(catch_df, conus) %>% 
    st_drop_geometry()
  tmp1$areasqkm <- round(tmp1$areasqkm, 2)
  tmp1$str_length <- round(tmp1$str_length, 2)
  tmp1$slope <- round(tmp1$slope, 2)
  
  tmp2 <- tmp1 %>% 
    select(COMID = featureid,
           State = state_name,
           County = name,
           "Area (sq. km)" = areasqkm,
           Length = str_length,
           "Max elevation" = max_elev,
           "Min elevation" = min_elev) %>% 
    slice(n=1) %>%
    mutate(across(1:7, as.character)) %>%
    tidyr::pivot_longer(1:7, names_to = "tag", values_to = "vals")
  
  nwm <- readNWMdata(comid = comid)
  
  max_flow <- nwm %>%  
    arrange(-flow_cms) %>% 
    slice(n = 1) %>% 
    rename(maxflow = flow_cms) %>% 
    select(date_max = dateTime, maxflow)
  max_flow$maxflow <- round(max_flow$maxflow, 2)
  max_flow$date_max <- as.Date(max_flow$date_max)
  max_flow <- max_flow %>% 
    rename("Date max flow" = date_max,
           "Max flow rate" = maxflow)
  max_flow <- max_flow %>% 
    mutate(across(1:2, as.character)) %>%
    tidyr::pivot_longer(1:2, names_to = "tag", values_to = "vals") 
  
  min_flow <- nwm %>%  
    arrange(flow_cms) %>% 
    slice(n = 1) %>% 
    rename(minflow = flow_cms) %>% 
    select(date_min = dateTime, minflow)
  min_flow$minflow <- round(min_flow$minflow, 2)
  min_flow$date_min <- as.Date(min_flow$date_min)
  min_flow <- rename(min_flow, "Min flow rate" = minflow,
                     "Date min flow" = date_min,)
  min_flow <- min_flow %>% 
    mutate(across(1:2, as.character)) %>%
    tidyr::pivot_longer(1:2, names_to = "tag", values_to = "vals") 
  
  tmp2 <- bind_rows(tmp2, max_flow)
  tmp2 <- bind_rows(tmp2, min_flow)
  
  DT::datatable(tmp2, colnames = c(" " = "tag", " " = "vals"), options = list(dom = "t"), rownames = FALSE) 
#   gt::gt(tmp2, rowname_col = "tag") %>%
#     tab_stubhead(label = "") %>%
#     # tab_spanner(label = "COMID", columns = vars(tag, vals)) %>%
#     tab_header(
#       title = "Summary"
#     )  %>%
#     cols_align("right") %>%
#     cols_label(
#       vals = " "
#     )
# knitr::kable(tmp2, col.names = c('', " "), booktabs= T,
#              table.attr ='class="table" style="color: black"', escape = F, align = "c") %>%
#   kableExtra::kable_classic("striped") %>%
#   # kableExtra::kable_material(c("striped", "hover")) %>%
#   kableExtra::kable_styling(position = "center") %>%
#   kableExtra::column_spec(1, background = "#BCD4E6") %>%
#   kableExtra::column_spec(2, background = "azure")


}


#   formattable(tmp2, align = c("l", rep("r", NCOL(tmp2) - 1)),
#                            list(`tag` = formatter("span", style = ~ style(font.weight = "bold")),
#                                 # `comid` = formatter("span", style = ~ style(border = "black", background = "gold", font.weight = "bold")),
#                                 # `vals` = color_bar("honeydew")),
#                                 `vals` = formatter("span", style = ~ style(font.weight = "bold")),
#   `Flow rate (C/m/s)` = color_tile("azure1", "cadetblue4"),
#   options = list(paging = TRUE, searching = TRUE)))
 
   # formattable(tmp2, align = c("l", rep("r", NCOL(tmp2) - 1)),
   #            list(`tag` = formatter("span", style = ~ style(font.weight = "bold")),
   #                 # `comid` = formatter("span", style = ~ style(border = "black", background = "gold", font.weight = "bold")),
   #                 `vals` = color_bar("honeydew")))



#  hchart(type = "line", hcaes(x = date, y = aet)) %>%
#   hc_colors(c("darkcyan")) %>%
#   hc_add_series(data = precip) %>%
#   hc_colors(c("red"))
# stat_smooth(col = "red") +
# theme_linedraw() +
# scale_color_viridis_c()

#
#####################
# USGS Water use data
water_use_data <- function(state, county) {
  water_use <- dataRetrieval::readNWISuse(stateCd = state, countyCd = county) %>%
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
    mutate(Public = public + domestic,
           Thermoelectric = total,
           Irrigation = livestock1 + livestock2 + aquacultere1 + irrigation1 + irrigation2) %>%
    select(1:6, Public, Irrigation, Industrial = industrial, Mining = mining,  Thermoelectric)
  water_use <- water_use %>%
    tidyr::unite('fips', statefips, countyfips)
  water_use$fips <- gsub("_", "", water_use$fips)
  water_use$fips <- as.numeric(water_use$fips)
  water_use <- water_use %>%
    tidyr::pivot_longer(6:10, names_to = "sector", values_to = "withdrawals")
  # 
  # water_use <- rename(water_use, YEAR = year)
  # water_use <- group_by(water_use, Sector, YEAR)
}
# water_use_data("CA", "Santa Barbara")

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


make_ts <- function(comid) {
  # nwm <- readNWMdata(comid = comid) 
  # ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
  dygraph(ts)  %>% 
  dyHighlight(highlightCircleSize = 2,
              highlightSeriesBackgroundAlpha = .3) %>%
    dyOptions(colors = c("darkcyan"),
              fillGraph = TRUE)
}

make_ts2 <- function(ts) {
  # nwm <- nwm 
  # ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
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
  
# USGS Water use data
water_use_data <- function(state, county) {
  water_use <- dataRetrieval::readNWISuse(stateCd = state, countyCd = county) %>%
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
    mutate("Public supply" = public + domestic,
           Thermoelectric = total,
           Irrigation = livestock1 + livestock2 + aquacultere1 + irrigation1 + irrigation2) %>%
    select(1:6, "Public supply", Irrigation, Industrial = industrial, Mining = mining,  Thermoelectric)
  water_use <- water_use %>%
    tidyr::unite('fips', statefips, countyfips)
  water_use$fips <- gsub("_", "", water_use$fips)
  water_use$fips <- as.numeric(water_use$fips)
  water_use <- water_use %>%
    tidyr::pivot_longer(6:10, names_to = "sector", values_to = "withdrawals")
  water_use <- group_by(water_use, sector, year)
}

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
