library(rgdal)
library(RODBC)
library(dplyr)
library(odeqstatusandtrends)
library(AWQMSdata)
library(dataRetrieval)
library(ggplot2)

start.date = "2000-01-01"
end.date = "2019-01-01"

# Name <- "Walker Stout"
# support_files_dir <- "//deqhq1/GISLIBRARY/Base_Data/Hydrography/Watershed_Boundaries/WBD_OR.gdb/WBD_OR.gdb/WBD_OR"
basin_shp <- readOGR(dsn = "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/SIAs/2019-SIA-Walker-Stout/GIS",
                          layer = 'WalkerStout_SIA', integer64="warn.loss", verbose = FALSE)

ws_stations_AWQMS <- get_stations_AWQMS(basin_shp)


ws_data <- odeqstatusandtrends::GetData(parameters = c("Temperature", "Bacteria", "TSS", "DO", "TP", "pH"),
                   stations_AWQMS = clack_stations_AWQMS,
                   start.date = start.date,
                   end.date = end.date)
ws_data <- CleanData(ws_data)


status <- status_stns(ws_data)

trend <- trend_stns(ws_data)

