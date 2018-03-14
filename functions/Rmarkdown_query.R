query<-function(select, dates, db, parms) {
  
  ##for testing
  #select = input$select

library(RCurl)
library(XML)
library(dataRetrieval)
library(plyr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(plotGoogleMaps)
library(DT)
library(wq)
library(chron)
library(reshape)
library(ggplot2)
library(zoo)
library(spatialEco)
library(dplyr)
library(lubridate)
#library(xlsx)
#library(RODBC)

options(stringsAsFactors = FALSE)

#source('functions/01_DataQuery.R')

 #agwqma <- readOGR(dsn = 'app/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
 #hucs <- readOGR(dsn = 'app/GIS', layer = 'WBD_HU8', verbose = FALSE)
# #agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
 # HUClist <- read.csv('app/PlanHUC_LU.csv')
 # stations_huc <- read.csv('app/station_wbd_12132016.csv')
 # ph_crit <- read.csv('app/PlanOWRDBasinpH_LU.csv')
 # ph_crit <- merge(ph_crit, HUClist, by.x = 'plan_name', by.y = 'PlanName', all.x = TRUE)
 # parms <- read.csv('app/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)
# wq_limited <- read.csv('app/GIS/wq_limited_df_temp_bact_ph_DO_2012.csv')

# #For app purposes set up input 
# input <- list(action_button = c(0))
# input$action_button <- 1
# input$parms <- c('Total Phosphorus', 'Total Suspended Solids',
#                  'Total Suspended Solids', 'Bacteria', 'Temperature', 'pH', 'Dissolved Oxygen', 'Total Nitrogen')
# input$select <- "Tualatin River Subbasin"
# input$dates <- c("2000-01-01", "2017-01-01")
# input$db <- c('DEQ', 'Water Quality Portal')
# input$selectStation <-  "10764 - "
# input$selectParameter <- 'Total Phosphorus'
# input$selectLogScale <- FALSE
# input$selectSpawning <- 'No spawning'#'January 1-May 15'
# input$selectUse <- 'Core Cold Water Habitat'
# input$selectpHCrit <- 'Willamette - All other basin waters'#'John Day - All other basin waters'
# input$plotTrend <- TRUE
# input$selectUseDO<-'Cold-Water Aquatic Life'
# input$checkSpawning<-TRUE
# input$selectWQSTSS<- 0


wqpData <- NULL
lasarData <- NULL
elmData <- NULL
nwisData <- NULL
df.all <- NULL
prog <- 0
wqp_message <- ""

if ('Water Quality Portal' %in% db) {
  print(paste0('WQP'))
  wqpData <- tryCatch(wqpQuery(planArea = select,
                               HUClist = HUClist,
                               inParms = input$parms,
                               luParms = lu_parms,
                               startDate = input$dates[1],
                               endDate = input$dates[2]),
                      error = function(err) {err <- geterrmessage()})

  if (any(c('Temperature', 'pH', 'Dissolved Oxygen', 'Total Suspended Solids', 'Total Phosphorus') %in% input$parms)) {
    print(paste0('NWIS'))
    nwisData <- tryCatch(nwisQuery(planArea = select,
                                   HUClist = HUClist,
                                   inParms = input$parms,
                                   startDate = input$dates[1],
                                   endDate = input$dates[2]),
                         error = function(err) {err <- geterrmessage()})
  }
  
  if (is.null(wqpData) & is.null(nwisData)) {
    wqp_message <- 'Your query returned no results from the Water Quality Portal.'
  } else if (!is.data.frame(wqpData) & !is.null(wqpData)) {
    if (grepl("307", wqpData)) {
      wqp_message <- 'Water Quality Portal is busy. Please try again in a few minutes.'
    }
  }
}

if ('DEQ' %in% db) {
  print(paste0('LASAR'))
  lasarData <- lasarQuery(planArea = select,
                          HUClist = HUClist,
                          inParms = input$parms,
                          startDate = input$dates[1],
                          endDate = input$dates[2],
                          stations_wbd = stations_huc)
  odbcCloseAll()
  if (nrow(lasarData) == 0) lasarData <- NULL
  
  print(paste0('ELEMENT'))
  elmData <- elementQuery(planArea = select,
                          HUClist = HUClist,
                          inParms = input$parms,
                          startDate = input$dates[1],
                          endDate = input$dates[2],
                          stations_wbd = stations_huc)
  odbcCloseAll()
  if (nrow(elmData) == 0) elmData <- NULL
}


if (wqp_message != 'Water Quality Portal is busy. Please try again in a few minutes.') {
  
  df.all <- tryCatch(combine(E=elmData,L=lasarData,W=wqpData,N=nwisData),
                     error = function(err) 
                     {err <- geterrmessage()})
}

return(df.all)

}
