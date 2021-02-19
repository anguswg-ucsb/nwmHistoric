
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


 pt <- data.frame(lng = -91.0106, lat = 32.23325)
nldi <- findNLDI(location = pt)
pt$comid <- nldi$comid
#
# nhd_sub <- get_nhdplus(comid = 100, realization = "catchment")
# nwm_df = readNWMdata(comid = 101)

# rownames(nwm_df) = nwm_df$dateTime


make_ts(pt)
  

make_ts <- function(pt, min=NULL, max=NULL) {
  if(!is.null(min))
  nwm3 <- readNWMdata(comid = pt$comid)
  ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
  dygraph(ts)  %>% 
    dyHighlight(highlightCircleSize = 2,
                highlightSeriesBackgroundAlpha = .3) %>%
    dyOptions(colors = c("darkcyan"),
              fillGraph = TRUE)
}



nwm <- readNWMdata(comid = 101)
  ts = xts::xts(as.data.frame(nwm$flow_cms), order.by = nwm$dateTime, tz= 'UTC')
  tmp1 <- dygraph(filter(ts, dateTime >= ))  %>% 
    dyHighlight(highlightCircleSize = 2,
                highlightSeriesBackgroundAlpha = .3) %>%
    dyOptions(colors = c("darkcyan"),
              fillGraph = TRUE)
  start <- nwm2$dateTime[1]
  end <- tail(nwm2$dateTime, n =1)
  class(end)
nwm2 <- nwm %>% 
  filter(dateTime >= "1993-01-01 01:00:00", dateTime <= "1993-01-01 03:00:00")
ts2 = xts::xts(as.data.frame(nwm2$flow_cms), order.by = nwm2$dateTime, tz= 'UTC')











