library(rgdal)
library(RODBC)
library(dplyr)
library(odeqstatusandtrends)
library(AWQMSdata)
library(dataRetrieval)
library(ggplot2)

start.date = "2000-01-01"
end.date = "2019-01-01"

# Name <- "Upper Muddy"

basin_shp <- readOGR(dsn = "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/SIAs/2019-SIA-Upper-Muddy/GIS",
                          layer = 'UpperMuddy', integer64="warn.loss", verbose = FALSE)

ws_stations_AWQMS <- get_stations_AWQMS(basin_shp)


ws_data <- odeqstatusandtrends::GetData(parameters = c("Temperature", "Bacteria", "TSS", "DO", "TP", "pH"),
                   stations_AWQMS = ws_stations_AWQMS,
                   start.date = start.date,
                   end.date = end.date)
ws_data <- CleanData(ws_data)


status <- status_stns(ws_data)

trend <- trend_stns(ws_data)

