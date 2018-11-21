library(RODBC)
library(plyr)
library(dplyr)


#### Redefine Stations_in_poly lat/lon columns ####

Stations_in_poly_AWQMS <- function(df.all, poly_shp, outside=FALSE) {
  # Returns a vector of stations that fall within or outside a polygon boundary
  # Arguments:
  # df.all = data frame of data with column names "Station_ID", "DATUM", "DECIMAL_LAT", "DECIMAL_LONG"
  # poly = polygon shapefile
  # Outside = TRUE if the stations outside the polygon should be returned instead, default is FALSE
  
  library(sp)
  
  # make a spatial object
  df.shp <- df.all[,c("MLocID", "Datum", "Lat_DD", "Long_DD")]         
  coordinates(df.shp)=~Long_DD+Lat_DD
  
  # Datums to search for
  # NAD83 : EPSG:4269 <- This is assumed if it is not one of the other two
  # NAD27 : EPSG:4267
  # WGS84 : EPSG:4326
  
  df.nad83 <- df.shp[!grepl("NAD27|4267|WGS84|4326",toupper(df.shp$Datum)), ]
  df.nad27 <- df.shp[grepl("NAD27|4267",toupper(df.shp$Datum)), ]
  df.wgs84 <- df.shp[grepl("WGS84|4326",toupper(df.shp$Datum)), ]
  
  proj4string(df.nad27) <- CRS("+init=epsg:4267")
  proj4string(df.nad83) <- CRS("+init=epsg:4269")
  proj4string(df.wgs84) <- CRS("+init=epsg:4326")
  
  # convert to NAD 83
  if (nrow(df.nad27)>0) {
    df.nad.27.nad83 <- spTransform(df.nad27, CRS("+init=epsg:4269")) 
    df.nad83 <- rbind(df.nad83, df.nad.27.nad83)
  }
  
  if (nrow(df.wgs84)>0) {
    df.wgs84.nad83 <- spTransform(df.wgs84, CRS("+init=epsg:4269")) 
    df.nad83 <- rbind(df.nad83, df.wgs84.nad83)
  }
  
  poly.nad83 <- spTransform(poly_shp, CRS("+init=epsg:4269"))
  
  
  if(outside) {
    # stations outside polygon
    df.out <- df.nad83[!complete.cases(over(df.nad83, poly.nad83)),]@data
    stations.out <- unique(df.out$MLocID)
    return(stations.out)
  } else {
    stations.in <- unique(df.nad83[poly.nad83,]@data$MLocID)
    return(stations.in)
  }
}

AWQMS_Query <- function(planArea = NULL, 
                        area.Shp = agwqma_shp, 
                        inParms, 
                        luParms, 
                        startDate,
                        endDate,
                        stations.Channel.Name = "STATIONS") {
  library(RCurl)
  library(XML)
  library(dataRetrieval)
  library(plyr)
  library(sp)
  library(rgdal)
  library(raster)
  library(rgeos)
  library(RODBC)
  
  # planArea <- agwqma
  # area.Shp <- agwqma_shp
  # inParms <- input$parms
  # luParms <- lu_parms
  # startDate <- paste(min(input$dates))
  # endDate <- paste(max(input$dates))
  # stations.Channel.Name <- "STATIONS"
  
  options(stringsAsFactors = FALSE)
  
  # Get Stations within Hucs from station database
  stations_channel <- odbcConnect(stations.Channel.Name)
  HUC_List <- HUClist[HUClist$PlanName == planArea, "HUC8"]
  stations_query <- paste0("SELECT * FROM VWStationsFinal WHERE HUC8 IN ('", paste(HUC_List, collapse = "', '"), "')")
  paste(stations_query)
  stations <- sqlQuery(stations_channel, stations_query, na.strings = "NA")
  
  agwqma_stations <- stations[stations$MLocID %in% Stations_in_poly_AWQMS(stations, area.Shp),]
  station_list <- unique(agwqma_stations$MLocID)
  
  ## connect to element and get data (must set up ODBC connection first)
  channel <- odbcConnect("AWQMS")
  table <- "VW_AWQMS_Results"
  ## get the names of all the tables in the database
  TableNames <- sqlTables(channel,errors = FALSE)
  columnNames <- sqlColumns(channel, table, errors = FALSE)
  
  #Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
  siteType = "'River/Stream', 'Lake', 'Other-Surface Water', 'Reservoir'"
  
  #### Get characteristics ####
  # The entire list of parameters that match to a criteria
  parms <- luParms
  
  # Take the inputs and write them to another vector for editing
  myParms <- inParms
  
  # Expand bacteria to include fecal and enterococcus
  if(any(inParms == 'Bacteria')) {
    myParms <- c(myParms, c('E. coli','Fecal coliform','Enterococci'))
    myParms <- myParms[-which(myParms == "Bacteria")] 
  }
  
  # Grab just the parameters we want
  characteristics <- paste(parms[parms$DEQ.Table.name %in% myParms,'AWQMS.Name'],collapse="', '")
  
  #### Define sample media to query ####
  sampleMedia <- 'Water'
  
  #### Pass the query to AWQMS ####
  
  myQuery <- paste0("SELECT * FROM VW_AWQMS_Results WHERE MLocID IN ('", paste(station_list, collapse = "', '"),
                    "') AND Char_Name IN ('", paste(characteristics, collapse = "', '"),
                    "') AND SampleStartDate BETWEEN '", startDate, "' AND '", endDate,
                    "' AND SampleMedia='", sampleMedia,
                    "' AND MonLocType IN (", siteType, ")"
                    )
  
  AWQMS_data <- sqlQuery(channel, myQuery, errors = FALSE)
  
  return(AWQMS_data)
}