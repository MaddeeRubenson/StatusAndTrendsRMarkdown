
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
    return(" a")
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

con.auto <- function(df, status_column, trend_column, station_id_column="Station_ID", target="criteria") {
  #function to auto generate conclusions based on status and trend results in stns_param_summary dataframe.
  
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

  # Testing
  #df <- stns_param_summary
  #station_id_column <- "Station_ID"
  
  #status_column <- "pH_S"
  #trend_column <- "pH_T"
  
  #status_column <- "Entero_S"
  #trend_column <- "Entero_T"
  
  #status_column <- "DO_S"
  #trend_column <- "DO_T"
  
  status.result <- c("Meets", "Exceeds")
  trend.result <- c("Improving", "Steady", "Degrading", "No Sig Trend")
  
  n_status <- length(df[df[,status_column] %in% status.result, status_column])
  n_status.meets <- length(df[df[,status_column] == "Meets", status_column])
  n_status.exceeds <- length(df[df[,status_column] == "Exceeds", status_column])
  
  n_trend <- length(df[df[,trend_column] %in% trend.result, trend_column])
  n_trend_improving <- length(df[df[,trend_column] == "Improving", trend_column])
  n_trend_steady <- length(df[df[,trend_column] == "Steady", trend_column])
  n_trend_degrading <- length(df[df[,trend_column] == "Degrading", trend_column])
  n_trend_nosig <- length(df[df[,trend_column] == "No Sig Trend", trend_column])
  
  status.unique <- unique(df[df[,status_column] %in% status.result, status_column])
  trend.unique <- unique(df[df[,trend_column] %in% trend.result, trend_column])
  
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
  
  result <- list("Meets"="met",
                 "Exceeds"="exceeded",
                 "Improving"="improving trend.",
                 "Steady"="steady trend.",
                 "Degrading"="degrading trend.", 
                 "No Sig Trend"=paste0(" did not have",a(n_trend_nosig)," statistically significant trend",s(n_trend_nosig),"."))
  
  analyte <- al[[status_column]][1]
  conc <- al[[status_column]][2]
  
  s2a <-""
  s2b <-""
  t2a <-""
  t2b <-""
  t2c <-""
  t2d <-""
  t3 <-""
  t4 <-""
  
  if(n_status == 0) {
    # No status results
    s1a <- "Data were not available in sufficient quantity to assess status."
    s2a <-""
    s2b <-""
  } else {
    
    # Status Results
    s1a <- paste0("Data collected at ",numbers.to.words(n_status, cap=FALSE)," station",s(n_status)," ",was.were(n_status)," sufficient to assess status. ")
    
    if(length(status.unique) == 1) {
      # All same status result
      
      if(n_status == 1) {
        # One station
        s2a <- paste0("Station ",df[df[,status_column] == status.unique, station_id_column],
                      " ", result[status.unique], " the ", analyte, " ", target, " in the last two years.")
      } else {
        
        s2a <- paste0(" All ", numbers.to.words(n_status, cap=FALSE)," station", s(n_status)," (",
                      paste0(df[df[,status_column] == status.unique, station_id_column], collapse=", "),
                      ") ", result[status.unique]," the ", analyte, " ", target," in the last two years.")
        s2b <-""
      } 
      
    } else {
      
      # Meets
      s2a <- paste0(numbers.to.words(n_status.meets)," of the ", numbers.to.words(n_status, cap=FALSE)," station",s(n_status)," (",
                    paste0(df[df[,status_column] == "Meets", station_id_column], collapse=", "),
                    ") had ", analyte," ", conc," that met the ", target," in the last two years. ")
      
      # Exceeds
      s2b <- paste0("The remaining ", numbers.to.words(n_status.exceeds, cap=FALSE)," station",s(n_status)," (",
                    paste0(df[df[,status_column] == "Exceeds", station_id_column], collapse=", "),
                    ") had ", analyte," ", conc," that exceeded the ", target," in the last two years.")
    }
  }
  
  txt.status <- paste0(s1a,s2a,s2b)
  
  if(n_trend == 0) {
    t1a <- "Data were not available in the last two years to assess trend."
    t2a <-""
    t2b <-""
    t2c <-""
    t2d <-""
    t3 <-""
    t4 <-""
    
  } else {
    
    t1a <- paste0("Data collected at ",numbers.to.words(n_trend, cap=FALSE)," station",s(n_trend)," ",was.were(n_trend)," sufficient to assess trends. ")
    
    if(length(trend.unique) == 1) {
      
      if(n_trend == 1) {
        
        t2a <- paste0("Station ",df[df[,trend_column] == trend.unique, station_id_column]," had ", a.an[[trend.unique]]," ", result[trend.unique])
      } else {
        t2a <- paste0(" All ", numbers.to.words(n_trend, cap=FALSE)," station", s(n_trend),
                      " had ", a.an[[trend.unique]]," ", result[trend.unique])
        t2b <-""
        t2c <-""
        t2d <-""
        t3 <-""
        t4 <-""
      }
    } else {
      
      # Improving
      if(n_trend_improving > 0) {
        
        t2a <- paste0(numbers.to.words(n_trend_improving)," of the ", numbers.to.words(n_trend, cap=FALSE)," station",s(n_trend)," (",
                      paste0(df[df[,trend_column] == "Improving", station_id_column], collapse=", "),
                      ") had an improving trend.")
      } else {
        t2a <- ""
      }
      
      pre.txt <- ""
      cap.txt <- TRUE
      
      # Steady
      if(n_trend_steady > 0) {
        
        if(n_trend_degrading + n_trend_nosig == 0) {
          pre.txt <- "The remaining "
          cap.txt <- FALSE}
        
        t2b <- paste0(pre.txt, numbers.to.words(n_trend_steady, cap=cap.txt)," of the ", numbers.to.words(n_trend, cap=FALSE)," station",s(n_trend)," (",
                      paste0(df[df[,trend_column] == "Steady", station_id_column], collapse=", "),
                      ") had a steady trend.")
        
      } else {
        t2b <- ""
      }
      
      # Degrading
      if(n_trend_degrading > 0) {
        
        if(n_trend_nosig == 0) {
          pre.txt <- "The remaining "
          cap.txt <- FALSE
        }
        
        t2c <- paste0(pre.txt, numbers.to.words(n_trend_degrading, cap=cap.txt)," of the ", numbers.to.words(n_trend, cap=FALSE)," station",s(n_trend)," (",
                      paste0(df[df[,trend_column] == "Degrading", station_id_column], collapse=", "),
                      ") had a degrading trend.")
      } else {
        t2c <- ""
      }
      
      # No Sig Trend
      if(n_trend_nosig > 0) {
        
        t2d <- paste0("The remaining ", numbers.to.words(n_trend_nosig, cap=FALSE, plural.only = TRUE)," station",s(n_trend_nosig)," (",
                      paste0(df[df[,trend_column] == "No Sig Trend", station_id_column], collapse=", "),
                      ") did not have ",a(n_trend_nosig)," statistically significant trend",s(n_trend_nosig),".")
      } else {
        t2d <- ""
      }
    }
  }
  
    txt.trend <- paste0(t1a," ",t2a," ",t2b," ",t2c," ",t2d)
    
    txt.con <- trimws(gsub("\\s+", " ", paste0(txt.status," ",txt.trend)))
    
  return(txt.con)
}