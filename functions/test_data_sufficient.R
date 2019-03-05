library(rgdal)
library(RODBC)
library(dplyr)
library(odeqstatusandtrends)
library(AWQMSdata)
library(dataRetrieval)
library(ggplot2)

start.date = "2000-01-01"
end.date = "2019-01-01"

# Name <- "Upper Malheur Drewsey"

basin_shp <- readOGR(dsn = "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/SIAs/GIS",
                          layer = 'UpperMalheur_SIA', integer64="warn.loss", verbose = FALSE)

umd_stations_AWQMS <- get_stations_AWQMS(basin_shp)


umd_data <- odeqstatusandtrends::GetData(parameters = c("Temperature", "Bacteria", "TSS", "DO", "TP", "pH"),
                   stations_AWQMS = umd_stations_AWQMS,
                   start.date = start.date,
                   end.date = end.date)
umd_data <- CleanData(ws_data)


status <- status_stns(umd_data)

trend <- trend_stns(umd_data)

