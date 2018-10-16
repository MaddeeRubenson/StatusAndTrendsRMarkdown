
s <- function(x) {
  # function to return s if a noun is plural given the count of x
  
  if (x == 1) {
    return("")
  } else {
    return("s") }
  
}

was.were <- function(x) {
  # function to return past singular (was) or past plural (were) given the count of x
  
  if (x == 1) {
    return("was")
  } else {
    return("were") }
  
}

numbers.to.words <- function(x) {
  # function to convert numbers < 10 to words
  # x = numeric number
  
  words <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  
  if (x >= 0 & x < 10) {
    return(words[x +1])
  } else {
    return(x) }
  
}



con.auto <- function(x) {
  
  ex <- "Data collected at nine stations were sufficient to assess status. 
  All nine stations (10729, 10730, 11050, 1119USBR_WQX-OWY301, 1119USBR_WQX-OWY304, 1119USBR_WQX-OWY307, 1119USBR_WQX-OWY310, 1119USBR_WQX-OWY313) 
  had total phosphours concentrations that exceeded the 0.07 mg/L Snake River - Hells-Canyon Sediment TMDL targets. 
  Nine stations had sufficient data to assess trends. Three stations (10729, 1119USBR_WQX-MAL218, 1119USBR_WQX-OWY110)
  had an improving trend. One drain station (1119USBR_WQX-MAL214) had a degrading trend. 
  The remaining five stations (10730, 11050, 1119USBR_WQX-MAL216, 1119USBR_WQX-OWY002, 36783) 
  did not have statistically significant trends."
  
  df <- stns_param_summary
  
  col.status <- "DO_S"
  col.trend <- "DO_T"
  analyte <- "dissolved oxygen"
  
  col.status <- "TP_S"
  col.trend <- "pH_T"
  
  status.result <- c("Meets", "Exceeds")
  trend.result <- c("Improving", "Steady", "Degrading", "No Sig Trend")
  
  n_status <- length(df[df[,col.status] %in% status.result, col.status])
  n_trend <- length(df[df[,col.trend] %in% trend.result, col.trend])
  
  status.unique <- unique(df[df[,col.status] %in% status.result, col.status])
  trend.unique <- unique(df[df[,col.trend] %in% trend.result, col.trend])
  
  if(n_status == 0) {
    
    txt.0 <- "Data were not available in the last two years to assess status."
    
  } else {
    
    txt.0 <- paste0("Data collected at ",numbers.to.words(n_status)," station",s(n_status)," ",was.were(n_status)," sufficient to assess status.")
    
    if(length(status.unique) == 1) {
      txt.1 <- paste0(" All ", numbers.to.words(n_status)," station", s(n_status)," ",was.were(n_status))
    }
    
    n_status.meets <- length(df[df[,col.status] == "Meets", col.status])
    if(n_status.meets > 0) {
      txt.2 <- paste0(numbers.to.words(n_status.meets)," station",s(n_status.meets)," (",
                      paste0(df[df[,col.status] == "Meets", "Station_ID"], collapse=", "),
                      ") had ", analyte ," that met the criteria.")
      
    }
    
    n_status.exceeds <- length(df[df[,col.status] == "Exceeds", col.status])
    if(n_status.exceeds > 0) {
      txt.3 <- paste0(numbers.to.words(n_status.exceeds)," station",s(n_status.exceeds)," (",
                      paste0(df[df[,col.status] == "Exceeds", "Station_ID"], collapse=", "),
                      ") had ", analyte ," that exceeded the criteria.")

    }
    
  
    
  }
  
  
}