Snapped_Stations <- function(df.all) {
  #This function connects to the snapped stations database and adds new columns to df.all (snap_LAT, snap_LONG) for 
  #each unique monitoring station within df.all. The snapped Lat and Long should represent the monitoring station 
  #on the NHD flowline and therefore properly delineate the upstream catchment. 
  
  #df.all is a dataframe containing:
         #Station_ID : monitoring station ID
  
  library(RODBC)
  
  #connect to microsoft Access 2007 database
  #table : STATIONS
  #Note: must have R set up to 32 bit, via Tools -> options 
  channel <- odbcConnectAccess2007("//deqlead03/GIS_WA/X-fer/LASAR_to_NHD/Test_StationInfo.accdb") #connect to Microsoft Access Database
  data <- sqlQuery(channel, paste("select * from STATIONS")) #create table from STATIONS table
  data <- data[,c(2,4,5)] #reduce data to just three columns: monitoring station id, snapped lat and snapped long
  names(data) <- c('Station_ID','snap_Lat', 'snap_Long') #rename columns
  
  df.all <- merge(df.all, data, by = "Station_ID")
  
  return(df.all)
  
}