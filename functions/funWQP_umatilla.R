
# Umatilla data from WQP is too large for memory so it was downloaded using this script.
# The query dates are broken down into smaller ranges and the data for the entire period is 
# put back together at the end. The output is saved as an R dataframe and imported again when the 
# wqpQuery function is called (within 01_DataQuery.R).



library(dataRetrieval)
library(rgdal)
source("//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/Umatilla Basin/2018-Umatilla-Basin/functions/funHelpers.R")

options(stringsAsFactors = FALSE)

startDate <- "2000-01-01"
endDate <- "2018-08-15"


query_dates <-list(c("2000-01-01","2003-12-31"),
                   c("2004-01-01","2006-12-31"),
                   c("2007-01-01","2011-12-31"),
                   c("2012-01-01","2018-08-15"))


project_dir <- "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/Umatilla Basin/2018-Umatilla-Basin"
Rdata_dir <- paste0(project_dir,"/RData")
wqp_sites_file <- paste0(gsub(" ", "_", "Umatilla Basin"), "_wqp_sites_",paste(c(startDate,endDate), collapse = "."), ".Rdata")

 

if(file.exists(file=paste0(Rdata_dir,"/",wqp_sites_file))) {
  
  print("loading wqp sites")
  load(file=paste0(Rdata_dir,"/",wqp_sites_file))
  
}  else {
  myHUCs <- "17070101;17070103"
  sampleMedia <- 'Water'
  siteType <- 'Estuary;Ocean;Stream;Lake, Reservoir, Impoundment'
  characteristics <- "pH;Temperature, water;Temperature;Escherichia coli;Fecal Coliform;Enterococci;Enterococcus;Dissolved oxygen;Dissolved oxygen (DO);Total suspended solids;Phosphorus;Phosphorus as P;Total Phosphorus, mixed forms;Total Nitrogen, mixed forms;Nitrogen"
  
  wqp.sites <- whatWQPsites(huc = myHUCs, 
                            characteristicName = characteristics,
                            startDate = startDate, 
                            endDate = endDate,
                            sampleMedia = sampleMedia,
                            siteType = siteType,
                            providers ='STORET')
  
  
  # import GIs files needed for this
  support_files_dir <-  "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/R_support_files"
  agwqma_shp <- readOGR(dsn = support_files_dir, layer = 'ODA_AgWQMA', integer64="warn.loss", verbose = FALSE)
  tribal_lands_shp <- readOGR(dsn = support_files_dir, layer = 'tl_2017_or_aiannh', integer64="warn.loss", verbose = FALSE)
  
  # remove  sites on tribal nation land or tribal trust land
  wqp.sites.keep <- Stations_in_poly(df.all=wqp.sites,
                                     poly_shp=tribal_lands_shp,
                                     station_id_col = "MonitoringLocationIdentifier",
                                     datum_col = "HorizontalCoordinateReferenceSystemDatumName",
                                     lat_col = "LatitudeMeasure", long_col = "LongitudeMeasure",
                                     outside=TRUE)
  
  wqp.sites <-wqp.sites[wqp.sites$MonitoringLocationIdentifier %in% wqp.sites.keep,]
  
  # remove sites outside the AgWQMA
  wqp.sites.keep <- Stations_in_poly(df.all=wqp.sites,
                                     poly_shp=agwqma_shp[agwqma_shp$PlanName=="Umatilla Basin",],
                                     station_id_col = "MonitoringLocationIdentifier",
                                     datum_col = "HorizontalCoordinateReferenceSystemDatumName",
                                     lat_col = "LatitudeMeasure", long_col = "LongitudeMeasure",
                                     outside=FALSE)
  
  wqp.sites.query <- paste(wqp.sites.keep,collapse=';')
  
  save(wqp.sites.query, file=paste0(Rdata_dir,"/",wqp_sites_file))
  
}
# ----------------------------------------------------------------------------------------
# break the query into date chunks

wqp_query <- function(wqp.sites.query, query_dates) {
  
  project_dir <- "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/Umatilla Basin/2018-Umatilla-Basin"
  Rdata_dir <- paste0(project_dir,"/RData")
  wqp_raw_file <- paste0(gsub(" ", "_", "Umatilla Basin"), "_wqp_raw_",paste(query_dates, collapse = "."), ".Rdata")
  
  myHUCs <- "17070101;17070103"
  sampleMedia <- 'Water'
  siteType <- 'Estuary;Ocean;Stream;Lake, Reservoir, Impoundment'
  characteristics <- "pH;Temperature, water;Temperature;Escherichia coli;Fecal Coliform;Enterococci;Enterococcus;Dissolved oxygen;Dissolved oxygen (DO);Total suspended solids;Phosphorus;Phosphorus as P;Total Phosphorus, mixed forms;Total Nitrogen, mixed forms;Nitrogen"
  
  wqp.data <- readWQPdata(siteNumbers = wqp.sites.query, 
                          characteristicName = characteristics, 
                          startDate = query_dates[1], 
                          endDate = query_dates[2],
                          sampleMedia = sampleMedia,
                          siteType = siteType,
                          providers ="STORET")
  
  
  Wx <- attr(wqp.data, "siteInfo")
  
  # ##REMOVE dissolved P##

  wqp.data[c("ResultSampleFractionText")][is.na(wqp.data[c("ResultSampleFractionText")])] <- 'Total'
  
  wqp.data<- wqp.data[wqp.data$ResultSampleFractionText == 'Total' , ]
  
  if(unique(wqp.data$ResultMeasure.MeasureUnitCode == 'ug/l' & !is.na(wqp.data$ResultMeasure.MeasureUnitCode))){
    wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'ug/l'), 'ResultMeasureValue'] <- 
      (wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'ug/l'), 'ResultMeasureValue'])/1000
  }
  
  wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'ug/l'), 'ResultMeasure.MeasureUnitCode'] <- 'mg/l'
  wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'mg/kg as P'), 'ResultMeasure.MeasureUnitCode'] <- 'mg/l'
  wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'mg/l as P'), 'ResultMeasure.MeasureUnitCode'] <- 'mg/l'
    
  
  attr(wqp.data, "siteInfo") <- Wx
  
  save(wqp.data, file=paste0(Rdata_dir,"/",wqp_raw_file))
  
}


wqp_query(wqp.sites.query=wqp.sites.query, query_dates=query_dates[[1]])
wqp_query(wqp.sites.query=wqp.sites.query, query_dates=query_dates[[2]])
wqp_query(wqp.sites.query=wqp.sites.query, query_dates=query_dates[[3]])
wqp_query(wqp.sites.query=wqp.sites.query, query_dates=query_dates[[4]])


wqp_raw_file <- paste0(gsub(" ", "_", "Umatilla Basin"), "_wqp_raw_",paste(query_dates[[1]], collapse = "."), ".Rdata")
load(file=paste0(Rdata_dir,"/",wqp_raw_file))
Wx1 <- attr(wqp.data, "siteInfo")
wqp.data.f <- wqp.data
rm(wqp.data)

wqp_raw_file <- paste0(gsub(" ", "_", "Umatilla Basin"), "_wqp_raw_",paste(query_dates[[2]], collapse = "."), ".Rdata")
load(file=paste0(Rdata_dir,"/",wqp_raw_file))
Wx2 <- attr(wqp.data, "siteInfo")
wqp.data.f <- rbind(wqp.data.f, wqp.data)
rm(wqp.data)

wqp_raw_file <- paste0(gsub(" ", "_", "Umatilla Basin"), "_wqp_raw_",paste(query_dates[[3]], collapse = "."), ".Rdata")
load(file=paste0(Rdata_dir,"/",wqp_raw_file))
Wx3 <- attr(wqp.data, "siteInfo")
wqp.data.f <- rbind(wqp.data.f, wqp.data)
rm(wqp.data)

wqp_raw_file <- paste0(gsub(" ", "_", "Umatilla Basin"), "_wqp_raw_",paste(query_dates[[4]], collapse = "."), ".Rdata")
load(file=paste0(Rdata_dir,"/",wqp_raw_file))
Wx4 <- attr(wqp.data, "siteInfo")
wqp.data.f <- rbind(wqp.data.f, wqp.data)
rm(wqp.data)

Wx <- rbind(Wx1,Wx2,Wx3,Wx4)

Wx <- Wx[!duplicated(Wx), ]

attr(wqp.data.f, "siteInfo") <- Wx

wqp_raw_file <- paste0(gsub(" ", "_", "Umatilla Basin"), "_wqp_raw_",paste(c(startDate,endDate), collapse = "."), ".Rdata")

save(wqp.data.f, file=paste0(Rdata_dir,"/",wqp_raw_file))
