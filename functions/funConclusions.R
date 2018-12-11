
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

a.an <- function(x){
  if(x == "Improving"){
    return("an")
  } else {
      return("a")
    }
}

singular.status <- function(x){
  if(x == "Meets"){
    return("Met")
  } else if(x == "Exceeds"){
    return("Exceed")
  }
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

first.up <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

to.lower.analyte <- function(x) {
  if(x %in% c("E.Coli", "Enterococcus", "pH")){
    return(x)
  } else {
    return(tolower(x))
  }
}

## For testing purposes
# stns_param_summary <- data.frame("Station_ID"=c("12345", "USGS-12345", "14355", "12995", "19948", "USGS-00000"),
#                                  "DO_S"=rep(c("Meets", "Exceeds"),3),
#                                  "DO_T"=rep(c("Improving", "Degrading"),3),
#                                  "Ecoli_S"=rep(c("Meets", "Exceeds"),3),
#                                  "Ecoli_T"=rep(c("Improving", "Steady"),3),
#                                  "Entero_S"=rep(c("Exceeds", "Exceeds"),3),
#                                  "Entero_T"=rep(c("Improving", "--"),3),
#                                  "pH_S"=rep(c("Meets", "--"),3),
#                                  "pH_T"=rep(c("Improving", "No Sig Trend"),3),
#                                  "Temp_S"=rep(c("Meets", "Meets"),3),
#                                  "Temp_T"=rep(c("--", "Degrading"),3),             
#                                  "TP_S"=rep(c("--", "--"),3), 
#                                  "TP_T"=rep(c("Improving", "Degrading"),3), 
#                                  "TSS_S"=rep(c("Meets", "Exceeds"),3),
#                                  "TSS_T"=rep(c("Improving", "Degrading"),3)
# )

con.auto <- function(df, input_analyte) {
  
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
  # t3a. [n_trends] station(s) had sufficient data to assess trends.
  # or
  # t3a. [analyte ?concentrations] data were not availible in sufficient qunitity to assess trend.
  
  # t4a. All [n_trends] station(s) (station_ID_vector) had (a/an) [trend result].
  # or
  # t4a. [n_trend_improving] station(s) (station_ID_vector) had an improving trend.
  # t4b. [n_trend_steady] station(s) (station_ID_vector) had a steady trend.
  # t4c. [n_trend_degrading] station(s) (station_ID_vector) had a degrading trend.
  
  # t5. The remaining
  
  # t4d. [n_trend_insuff] station(s) (station_ID_vector) did not have statistically significant trends.
  
  analyte_cols <- data.frame(analyte = c("E.Coli", "Enterococcus", "Dissolved Oxygen", "pH", "Temperature", "Total Phosphorus", "TSS"),
                             status = c("Ecoli_S", "Entero_S", "DO_S", "pH_S", "Temp_S", "TP_S", "TSS_S"),
                             trend = c("Ecoli_T", "Entero_T", "DO_T", "pH_T", "Temp_T", "TP_T", "TSS_T"))
  
  col.status <- analyte_cols[analyte_cols$analyte == input_analyte, "status"]
  col.trend <- analyte_cols[analyte_cols$analyte == input_analyte, "trend"]
  analyte <- input_analyte
  
  measurement <- if(analyte %in% c('Dissolved Oxygen', 'E.Coli', 'Enterococcus', 'Total Phosphorus', 'Total Suspended Solids')){
    "concentrations"
  } else {"measurements"}
  
  #col.status <- "TP_S"
  #col.trend <- "pH_T"
  
  status.result <- c("Meets", "Exceeds")
  trend.result <- c("Improving", "Steady", "Degrading", "No Sig Trend")
  
  n_status <- length(df[df[,col.status] %in% status.result, col.status])
  n_trend <- length(df[df[,col.trend] %in% trend.result, col.trend])
  
  status.unique <- unique(df[df[,col.status] %in% status.result, col.status])
  trend.unique <- unique(df[df[,col.trend] %in% trend.result, col.trend])
  s1a <- NULL
  s2a <- NULL
  s2b <- NULL
  t1a <- NULL
  t2a <- NULL
  t2b <- NULL
  t2c <- NULL
  t2d <- NULL
  t2bx <- NULL
  t2cx <- NULL
  t2dx <- NULL
  
  if(analyte == 'Enterococcus' & n_status == 0 & n_trend == 0){
    s1a <- "The *Enterococcus* coastal water contact recreation criteria does not apply in the Lost River so an assessment was not made."
    return(paste(s1a))
  }
  
  if(n_status == 0) { # If no stations had sufficient data to assess status do this
    
    s1a <- "Data were not available in the last two years to assess status. "
    
  } else { # Otherwise, do this
    
    # These stations had sufficient data to asses status
    
    s1a <- paste0("Data collected at ",numbers.to.words(n_status)," station",s(n_status)," ",was.were(n_status)," sufficient to assess status. ")
    
    if(length(status.unique) == 1) { # If all of the stations had the same status do this
      
      s2a <- paste0(" All ", numbers.to.words(n_status)," station", s(n_status)," had ", to.lower.analyte(analyte)," ", measurement, " that ", 
                    tolower(singular.status(status.unique)), " the ", to.lower.analyte(analyte), " water quality criteria. ")
      
    } else { # Otherwise, do this
      
      n_status_attain <- sum(df[,col.status] == "Meets")
      n_status_exceed <- sum(df[,col.status] == "Exceeds")
      s_attain_stations <- unique(df[df[,col.status] == "Meets",]$Station_ID)
      s_exceed_stations <- unique(df[df[,col.status] == "Exceeds",]$Station_ID)
      
      s2a <- paste0("The ", to.lower.analyte(analyte), " ", measurement, " observed at ", numbers.to.words(n_status_attain), " station(s) (", paste0(s_attain_stations, collapse = ", "), 
                    ") had no in-stream values that exceeded the ", to.lower.analyte(analyte), " water quality criteria. ")
      s2b <- paste0("The ", to.lower.analyte(analyte), " ", measurement, " observed at ", numbers.to.words(n_status_exceed), " station(s) (", paste0(s_exceed_stations, collapse = ", "), 
                    ") had at least one in-stream value in the last two years that exceeded the ", to.lower.analyte(analyte), " water quality criteria. ")
      }
    }
  
  if(n_trend == 0){ # If no stations had sufficient data to assess trends do this
    
    t1a <- paste0("Sufficient data was not available to assess trends related to ", to.lower.analyte(analyte), ".")
    
  } else { # Otherwise, do this
    
    # These stations had sufficient data to asses trends
    
    t1a <- paste0("Data collected at ", numbers.to.words(n_trend)," station", s(n_trend)," ", was.were(n_trend)," sufficient to assess trends. ")
    
    if(length(trend.unique) == 1) { # If all of the stations had the same trend do this
      
      t2a <- paste0(" All ", numbers.to.words(n_trend)," station", s(n_trend)," had ", to.lower.analyte(analyte)," concentrations that showed ", a.an(trend.unique), 
                    " ", tolower(trend.unique), " trend in the available ", to.lower.analyte(analyte), " data. ")
      
    } else { # Otherwise, do this
      
      n_trend_improve <- sum(df[,col.trend] == "Improving")
      n_trend_degrade <- sum(df[,col.trend] == "Degrading")
      n_trend_steady <- sum(df[,col.trend] == "Steady")
      n_trend_noSig <- sum(df[,col.trend] == "No Sig Trend")
      t_improve_stations <- unique(df[df[,col.trend] == "Improving",]$Station_ID)
      t_degrade_stations <- unique(df[df[,col.trend] == "Degrading",]$Station_ID)
      t_steady_stations <- unique(df[df[,col.trend] == "Steady",]$Station_ID)
      t_noSig_stations <- unique(df[df[,col.trend] == "No Sig Trend",]$Station_ID)
      
      # This many stations showed an improving trend
      if(n_trend_improve > 0){
        t2a <- paste0(first.up(numbers.to.words(n_trend_improve)), " station(s) (", paste0(t_improve_stations, collapse = ", "),
                      ") showed an improving trend in the available ", to.lower.analyte(analyte), " data. ")
      }
      # This many showed a degrading trend
      if(n_trend_degrade > 0){
        if(sum(n_trend_improve, n_trend_degrade) == n_trend){
          t2bx <- "The remaining "
          num.word <- numbers.to.words(n_trend_degrade)
          } else {num.word <- first.up(numbers.to.words(n_trend_degrade))}
        t2b <- paste0(num.word, " station(s) (", paste0(t_degrade_stations, collapse = ", "),
                      ") showed a degrading trend in the available ", to.lower.analyte(analyte), " data. ")
      }
      # This many showed a steady trend
      if(n_trend_steady > 0){
        if(sum(n_trend_improve, n_trend_degrade, n_trend_steady) == n_trend){
          t2cx <- "The remaining "
          num.word <- numbers.to.words(n_trend_steady)
        } else {num.word <- first.up(numbers.to.words(n_trend_steady))}
        t2c <- paste0(num.word, " station(s) (", paste0(t_steady_stations, collapse = ", "),
                      ") showed a steady trend in the available ", to.lower.analyte(analyte), " data. ")
      }
      # This many did not show a significant trend
      if(n_trend_noSig > 0){
        if(sum(n_trend_improve, n_trend_degrade, n_trend_steady, n_trend_noSig) == n_trend){
          t2dx <- "The remaining "
          num.word <- numbers.to.words(n_trend_noSig)
        } else {num.word <- first.up(numbers.to.words(n_trend_noSig))}
        t2d <- paste0(num.word, " station(s) (", paste0(t_noSig_stations, collapse = ", "),
                      ") did not show significant trending in the available ", to.lower.analyte(analyte), " data. ")
      }
    }
  }
  return(paste0(s1a, s2a, s2b, t1a, t2a, t2bx, t2b, t2cx, t2c, t2dx, t2d))
}


