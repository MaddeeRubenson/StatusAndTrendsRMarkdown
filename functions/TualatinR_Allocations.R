require(xts)

TP_all #All total phosphorus data
tp_stn #stations that fit status and/or trend

##Tualatin River Subbasin TMDL load allocation 
#http://www.oregon.gov/deq/FilterDocs/tualatinCh2Phosphorus.pdf

dta <- TP_all %>% 
  filter(Analyte == 'Total Phosphorus') %>%
  filter(Station_ID %in% tp_stn)

#dta$Result <- as.numeric(paste(dta$Result))

alloc <- read.csv("Lookups/Tualatin_PAllocations_LU.csv")

#Summer_min <- '' #Jun 1
#Summer_max <- '' #Sept 22

alloc

dta$Sampled <- as.POSIXct((strptime(dta$Sampled, format = '%Y-%m-%d')))
dta$Sampled <- as.Date(dta$Sampled)
dta$month <- as.numeric(format(dta$Sampled, format = '%m'))
dta$year <- as.numeric(format(dta$Sampled, format = '%Y'))
dta$day <- as.numeric(format(dta$Sampled, format = '%d'))

#summer median allocation
mths <- c('6', '7', '8', '9')

summer <- dta %>%
  filter(month %in% mths)

print(paste('Monitoring stations that have a summer Allocation are:', unique(summer$Station_ID)))

list <- list()
for(i in 1:length(tp_stn)) {

  sub <- summer[summer$Station_ID == summer$Station_ID[i],]
  #sub$Result <- as.numeric(paste(sub$Result))
  median <- median(sub$Result)
 
  print(paste('median summer value for station', tp_stn[i], 'is', median))
  
  sub$median <- median
  
  list[[i]] <- sub
  
}

summer <- ldply(list, data.frame, .id=NULL)

##daily maximum allocation for tributary locations
trib_stn <- alloc[!is.na(alloc$TP_Concentrations_DailyMaximum), 'Station_ID']
daily_max <-dta %>%
  filter(Station_ID %in% tp_stn) %>%
  filter(Station_ID %in% trib_stn) %>%
  group_by(Station_ID, Sampled = as.Date(Sampled)) %>%
  dplyr::summarise(daily_max = max(Result))

trib <- merge(daily_max, dta, by=c('Station_ID','Sampled'))

##combine summer and trib allocation columns
#temp <- merge(trib, summer[,c('Station_ID', 'median')], by = 'Station_ID')

###evaluate exceedance of allocation

#summer median 


#daily max trib allocation


