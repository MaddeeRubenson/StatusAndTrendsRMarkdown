query<-function(select, HUClist, stations_huc, dates, db, parms, lu_parms) {
  
  ##for testing
  #select <-  input$select
  #HUClist <- HUClist
  #parms <- input$parms
  #dates <- input$dates
  #db <- input$db
  #lu_parms<-lu_parms
  
  library(RCurl)
  library(XML)
  library(dataRetrieval)
  library(plyr)
  library(sp)
  library(rgdal)
  library(raster)
  library(rgeos)
  #library(plotGoogleMaps)
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
  library(RODBC)
  
  options(stringsAsFactors = FALSE)

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
                                 inParms = parms,
                                 luParms = lu_parms,
                                 startDate = dates[1],
                                 endDate = dates[2]),
                        error = function(err) {err <- geterrmessage()})
    
    if (any(c('Temperature', 'pH', 'Dissolved Oxygen', 'Total Suspended Solids', 'Total Phosphorus') %in% input$parms)) {
      print(paste0('NWIS'))
      nwisData <- tryCatch(nwisQuery(planArea = select,
                                     HUClist = HUClist,
                                     inParms = parms,
                                     startDate = dates[1],
                                     endDate = dates[2]),
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
                            inParms = parms,
                            startDate = dates[1],
                            endDate = dates[2],
                            stations_wbd = stations_huc)
    odbcCloseAll()
    
    if (nrow(lasarData) == 0) {
      lasarData <- NULL
    } else {
      lasarData$SAMPLE_DATE_TIME <- as.POSIXct(strptime(lasarData$SAMPLE_DATE_TIME, format = '%Y-%m-%d %H:%M:%S'))
      lasarData <- filter(lasarData, !is.na(SAMPLE_DATE_TIME))
    }
    
    print(paste0('ELEMENT'))
    elmData <- elementQuery(planArea = select,
                            HUClist = HUClist,
                            inParms = input$parms,
                            startDate = dates[1],
                            endDate = dates[2],
                            stations_wbd = stations_huc)
    odbcCloseAll()
    if (nrow(elmData) == 0) elmData <- NULL
  }
  
  
  if (wqp_message != 'Water Quality Portal is busy. Please try again in a few minutes.') {
    
    df.all <- tryCatch(combine(E=elmData,L=lasarData, W=wqpData,N=nwisData),
                       error = function(err) 
                       {err <- geterrmessage()})
  }
  
  return(df.all)
  
}
