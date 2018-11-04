
s <- function(x) {
  # function to return s if a noun is plural given the count of x
  
  if (x == 1) {
    return("")
  } else {
    return("s") }
  
}

a <- function(x) {
  # function to return "a" or "" given the count of x
  
  if (x == 1) {
    return("a")
  } else {
    return("") }
  
}

was.were <- function(x) {
  # function to return past singular (was) or past plural (were) given the count of x
  
  if (x == 1) {
    return("was")
  } else {
    return("were") }
  
}

a.an <- function(x) {
  
}

numbers.to.words <- function(x, cap=TRUE, plural.only=FALSE) {
  # function to convert numbers < 10 to words
  # x = numeric number
  if (plural.only) {
    
    words <- c("", "", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  
    } else {
    
    words <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    
  }
  
  if (x >= 0 & x < 10) {
    if (cap) {
      return(paste0(toupper(substr(words[x +1], 1, 1)), substr(words[x +1], 2, nchar(words[x +1]))))
    } else {
      return(words[x +1])
    }
  } else {
    return(x) }
  
}


con.auto <- function(analyte, stns_param_summary) {
  
  # if (status_station_ID_vector == trend_station_ID_vector)
  # 1. Data collected at [n_status_trend] station(s) were sufficient to assess both [analyte] status and trends.
  
  # Status
  # s1a. Data collected at [n_status] station(s) were sufficient to assess status.
  # or
  # s1a. [analyte ?concentrations] data were not availible in the last two years to assess status.
  
  # s2a. All [n_status] station(s) (station_ID_vector) had [analyte ?concentrations] that [status_result] the [analyte] water quality criteria.
  # or
  # s2a. [n_status_attain] station(s) (station_ID_vector) had [analyte ?concentrations] that [status_result] the [analyte] water quality criteria.
  # s2b. [n_status_exceed] station(s) (Station_ID_vector) [status_result] the [analyte] water quality criteria.
  
  # TRENDS
  # t1a. [n_trends] station(s) had sufficient data to assess trends.
  # or
  # t1a. [analyte ?concentrations] data were not availible in sufficient qunitity to assess trend.
  
  # t2a. All [n_trends] station(s) (station_ID_vector) had (a/an) [trend result].
  # or
  # t2a. [n_trend_improving] station(s) (station_ID_vector) had an improving trend.
  # t2b. [n_trend_steady] station(s) (station_ID_vector) had a steady trend.
  # t2c. [n_trend_degrading] station(s) (station_ID_vector) had a degrading trend.
  
  # t3. The remaining
  
  # t4. [n_trend_insuff] station(s) (station_ID_vector) did not have statistically significant trends.
  
  
  al <- list("DO_S"=c("dissolved oxygen","concentrations"),
             "DO_T"=c("dissolved oxygen","concentrations"),
             "Ecoli_S"=c("_E. coli_","concentrations"),            
             "Ecoli_T"=c("_E. coli_","concentrations"),
             "Entero_S"=c("_Enterococcus_","concentrations"),
             "Entero_T"=c("_Enterococcus_","concentrations"),
             "pH_S"=c("pH",""),
             "pH_T"=c("pH",""),
             "Temp_S"=c("temperature",""),
             "Temp_T"=c("temperature",""),             
             "TP_S"=c("total phosphorus","concentrations"), 
             "TP_T"=c("total phosphorus","concentrations"), 
             "TSS_S"=c("total suspended soilids","concentrations"),
             "TSS_T"=c("total suspended soilids","concentrations"))
  
  a.an <- list("Improving"="an", 
               "Steady"="a",
               "Degrading"="a", 
               "No Sig Trend"="")
  
  
  df <- stns_param_summary
  
  #col.status <- "pH_S"
  #col.trend <- "pH_T"
  
  col.status <- "DO_S"
  col.trend <- "DO_T"
  analyte <- al[[col.status]][1]
  conc <- al[[col.status]][2]
  

  
  status.result <- c("Meets", "Exceeds")
  trend.result <- c("Improving", "Steady", "Degrading", "No Sig Trend")
  
  n_status <- length(df[df[,col.status] %in% status.result, col.status])
  n_trend <- length(df[df[,col.trend] %in% trend.result, col.trend])
  
  status.unique <- unique(df[df[,col.status] %in% status.result, col.status])
  trend.unique <- unique(df[df[,col.trend] %in% trend.result, col.trend])
  
  if(n_status == 0) {
    
    s1a <- "Data were not available in the last two years to assess status."
    s2a <-""
    s2b <-""
    
  } else 
  {
    
    s1a <- paste0("Data collected at ",numbers.to.words(n_status, cap=FALSE)," station",s(n_status)," ",was.were(n_status)," sufficient to assess status. ")
    
    if(length(status.unique) == 1) {
      
      s2a <- paste0(" All ", numbers.to.words(n_status, cap=FALSE)," station", s(n_status),
                    " had ", analyte," ", conc," that ",status.unique, 
                    " the ", analyte, " water quality criteria in the last two years.")
      s2b <-""
    } else { 
      
      n_status.meets <- length(df[df[,col.status] == "Meets", col.status])
      # Meets
      if(n_status.meets > 0) {
        s2a <- paste0(numbers.to.words(n_status.meets)," of the ", numbers.to.words(n_status, cap=FALSE)," station",s(n_status)," (",
                      paste0(df[df[,col.status] == "Meets", "Station_ID"], collapse=", "),
                      ") had ", analyte," ", conc," that met the criteria in the last two years. ")
        
      }
      
      n_status.exceeds <- length(df[df[,col.status] == "Exceeds", col.status])
      # Exceeds
      if(n_status.exceeds > 0) {
        s2b <- paste0("The remaining ", numbers.to.words(n_status.exceeds, cap=FALSE)," station",s(n_status)," (",
                      paste0(df[df[,col.status] == "Exceeds", "Station_ID"], collapse=", "),
                      ") had ", analyte," ", conc," that exceeded the criteria in the last two years.")
        
      }
    }
  }
  
  txt.status <- paste0(s1a,s2a,s2b)
  
  trend.result <- c("Improving", "Steady", "Degrading", "No Sig Trend")
  
  if(n_trend == 0) {
    
    t1a <- "Data were not available in the last two years to assess trend."
    t2a <-""
    t2b <-""
    t2c <-""
    t2d <-""
    t3 <-""
    t4 <-""
    
  } else 
  {
    
    t1a <- paste0("Data collected at ",numbers.to.words(n_trend, cap=FALSE)," station",s(n_trend)," ",was.were(n_trend)," sufficient to assess trends. ")
    
    if(length(trend.unique) == 1) {
      
      if(n_trend == 1) {
        
        t2a <- paste0("Station ",df[df[,col.trend] == trend.unique, "Station_ID"]," had ", a.an[[trend.unique]]," ", trend.unique, 
                      " trend.")
        
      } else
      {
      
      t2a <- paste0(" All ", numbers.to.words(n_trend, cap=FALSE)," station", s(n_trend),
                    " had ", a.an[[trend.unique]]," ", trend.unique, 
                    " trend.")
      t2b <-""
      t2c <-""
      t2d <-""
      t3 <-""
      t4 <-""
      }
    } else {
      
      n_trend_improving <- length(df[df[,col.trend] == "Improving", col.trend])
      # Improving
      if(n_trend_improving > 0) {
        
        t2a <- paste0(numbers.to.words(n_trend_improving)," of the ", numbers.to.words(n_trend, cap=FALSE)," station",s(n_trend)," (",
                      paste0(df[df[,col.trend] == "Improving", "Station_ID"], collapse=", "),
                      ") had an improving trend.")
      } else {
        
        t2a <- ""
      }
      
      n_trend_steady <- length(df[df[,col.trend] == "Steady", col.trend])
      # Steady
      if(n_trend_steady > 0) {
        
        ("Degrading" %in% trend.unique | "No Sig Trend" %in% trend.unique)
        
        t2b <- paste0(numbers.to.words(n_trend_steady)," of the ", numbers.to.words(n_trend, cap=FALSE)," station",s(n_trend)," (",
                      paste0(df[df[,col.trend] == "Steady", "Station_ID"], collapse=", "),
                      ") had a steady trend.")
        
      } else {
        t2b <- ""
      }
      
      n_trend_degrading <- length(df[df[,col.trend] == "Degrading", col.trend])
      # Degrading
      
      if(n_trend_degrading > 0) {
        
        "No Sig Trend" %in% trend.unique
        
        t2c <- paste0(numbers.to.words(n_trend_degrading)," of the ", numbers.to.words(n_trend, cap=FALSE)," station",s(n_trend)," (",
                      paste0(df[df[,col.trend] == "Degrading", "Station_ID"], collapse=", "),
                      ") had a degrading trend.")
      } else {
        t2c <- ""
      }
      
      n_trend_nosig <- length(df[df[,col.trend] == "No Sig Trend", col.trend])
      # No Sig Trend
      if(n_trend_nosig > 0) {
        
        t2d <- paste0("The remaining ", numbers.to.words(n_trend_nosig, cap=FALSE, plural.only = TRUE)," station",s(n_trend_nosig)," (",
                      paste0(df[df[,col.trend] == "No Sig Trend", "Station_ID"], collapse=", "),
                      ") did not have ",a(n_trend_nosig)," statistically significant trend",s(n_trend_nosig),".")
      } else {
        t2d <- ""
      }
      
    }
  }
  txt.trend <- paste0(t1a," ",t2a," ",t2b," ",t2c," ",t2d)
  
  txt.con <- trimws(gsub("\\s+", " ", paste0(txt.status," ",txt.trend)))
  
  
}