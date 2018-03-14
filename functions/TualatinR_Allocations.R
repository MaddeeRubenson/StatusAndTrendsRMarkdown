evaluate_tp_alloc <- function(TP_all, tp_stn, Dairy = FALSE) {

#TP_all #All total phosphorus data
#tp_stn #stations that fit status and/or trend

##Tualatin River Subbasin TMDL load allocation 
#http://www.oregon.gov/deq/FilterDocs/tualatinCh2Phosphorus.pdf

dta <- TP_all %>% 
  filter(Analyte == 'Total Phosphorus') %>%
  filter(Station_ID %in% tp_stn)

# t <- df.all  %>%
#   filter(Analyte == "Total Phosphorus") 
# t <- unique(t[,c('Station_ID', 'Station_Description', 'DECIMAL_LAT', 'DECIMAL_LONG')])
# t <- t[t$Station_ID %in% tp_stn,]
# al <- rbind.fill(t, alloc)
# write.csv(al, "Lookups/Tualatin_PAllocations_LU.csv")
if(Dairy == FALSE) {
  alloc <- read.csv("T:/AgWQM/DataAnalysis/StatusAndTrendsRMarkdown/Lookups/Tualatin_PAllocations_LU.csv")
} else { #specific to Dairy/McKay watershed ONLY
  alloc <- data.frame('Station_ID' = unique(tp_stn),
                      'TP_Concentration_SummerMedian' = as.numeric(0.09),
                      'Table_Number' = '2-5')
}
#Summer_min <- '' #Jun 1
#Summer_max <- '' #Sept 22

#alloc

dta$Sampled <- as.POSIXct((strptime(dta$Sampled, format = '%Y-%m-%d')))
dta$Sampled <- as.Date(dta$Sampled)
dta$month <- as.numeric(format(dta$Sampled, format = '%m'))

#summer median allocation
mths <- c('6', '7', '8', '9')
not_mths <- c('1', '2', '3', '4', '5', '10', '11', '12')

#summer_stns <- c('10456', '10458', '10459', '10461')

summer <- dta %>%
  filter(month %in% mths) %>%
  filter(Station_ID %in% tp_stn)
not_summer <- dta %>%
  filter(month %in% not_mths) %>%
  filter(Station_ID %in% tp_stn)

summer$Station_ID <- as.character(summer$Station_ID)

list <- list()
for(i in 1:length(unique(summer$Station_ID))) {

  sub <- summer[summer$Station_ID == tp_stn[i],]
  median <- median(sub$Result)
 
  sub$median <- median
  
  list[[i]] <- sub
  
}

summer <- ldply(list, data.frame, .id=NULL)

# ##daily maximum allocation for tributary locations
# trib_stn <- alloc[!is.na(alloc$TP_Concentrations_DailyMaximum), 'Station_ID']
# daily_max <-dta %>%
#  # filter(Station_ID %in% tp_stn) %>%
#   filter(Station_ID %in% trib_stn) %>%
#   group_by(Station_ID, Sampled = as.Date(Sampled)) %>%
#   dplyr::summarise(daily_max = max(Result))
# 
# trib <- merge(daily_max, dta, by=c('Station_ID','Sampled'))

###evaluate exceedance of allocation

#summer median 
summer <- merge(summer, alloc, by="Station_ID")
summer$exceed <- ifelse(summer$median > summer$TP_Concentration_SummerMedian, 'Exceeds', 'Meets')

# #daily max trib allocation
# trib <- merge(trib, alloc, by="Station_ID")
# trib_summer <- trib %>% filter(month %in% mths)
# trib_summer$exceed <- ifelse(trib_summer$Result > trib_summer$TP_Concentrations_SummerMedian, 'Exceeds', 'Meets')
# trib_year <- trib %>% filter(month %in% not_mths)
# trib_year$exceed <- ifelse(trib_year$daily_max > trib_year$TP_Concentrations_DailyMaximum, 'Exceeds', 'Meets')
# 
# trib <- rbind(trib_summer, trib_year)

#TP_alloc <- rbind.fill(summer, trib)

not_summer$median <- NA
not_summer$TP_Concentration_SummerMedian <- NA
not_summer$Table_Number <- NA
not_summer$exceed <- 'Meets'

TP_alloc <- rbind(not_summer, summer)

return(TP_alloc)
}

tp_alloc_boxplot <- function(TP_alloc) {
  
  new_data <- TP_alloc
  
  alloc <- read.csv("Lookups/Tualatin_PAllocations_LU2.csv")
  
  new_data$year <- as.numeric(format(new_data$Sampled, format = '%Y'))
  new_data$month <- as.numeric(format(new_data$Sampled, format = '%m'))
  
  new_data <- new_data[new_data$month %in% c('6', '7', '8', '9'), ]
  
  stn <- unique(new_data$Station_ID)
  
  alloc <- alloc[alloc$Station_ID == stn, "TP_Concentrations_SummerMedian"]
  
  x.lab <- "Year"
  y.lab <- paste('Total Phosphorus', '(mg/L)')
  title <- paste0(min(new_data[, 'Station_Description']), ", ID = ",
                  min(new_data[, 'Station_ID']))
  sub.text <- paste0('The applicable median summer results compared to the loading capacity: ', alloc, ' mg/l')
  
  med <- ddply(new_data, .(year), summarise, med = median(Result))
  
  new_data$year <- as.factor(new_data$year)
  
  
  g <- ggplot(data = new_data, aes(x = year, y = Result)) +
    geom_boxplot() +
    geom_point(data = med, aes(x = as.factor(year), y = med), color = 'red') +
    geom_text(data = med, aes(x = as.factor(year), y = med, label = med), size = 2.2, vjust = -1.5) +
    geom_hline(yintercept = alloc, color = 'darkgreen', linetype = 'dashed') + 
    theme(plot.title = element_text(vjust=1.5, face="bold", size = 10))+
    ggtitle(bquote(atop(.(title), atop(paste(.(sub.text)))))) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.direction = 'horizontal') +
    xlab(x.lab) +
    ylab(y.lab) 
  g
  
  
}



tp_alloc_plot <- function(TP_alloc, sea_ken_table, plot_trend) {
  
    new_data <- TP_alloc
    x.min <- min(new_data$Sampled) 
    x.max <- max(new_data$Sampled) 
    x.lim <- c(x.min, x.max)
    title <- paste0(min(new_data[, 'Station_Description']), ", ID = ",
                    min(new_data[, 'Station_ID']))
    x.lab <- "Date"
    y.lab <- paste('Total Phosphorus', '(mg/L)')
    
    y.min <- floor(min(new_data[, 'Result'])) 
    
    y.max <- ceiling(max(new_data[, 'Result']))
    
    
    y.lim <- c(y.min, y.max)
    y.median <- median(new_data[, 'Result'])
    slope <- suppressWarnings(
      as.numeric(
        results_seaken[results_seaken$Station_ID == 
                        unique(new_data[, 'Station_ID']) & 
                        results_seaken$analyte == 
                        unique(new_data[, 'Analyte']), 'slope']
      )
    )
    p.value <- suppressWarnings(
      as.numeric(
        results_seaken[results_seaken$Station_ID == 
                        unique(new_data[,'Station_ID']) & 
                        results_seaken$analyte == 
                        unique(new_data[,'Analyte']),'pvalue']
      )
    )
    p.value.label <- results_seaken[results_seaken$Station_ID == 
                                     unique(new_data[,'Station_ID']) & 
                                     results_seaken$analyte == 
                                     unique(new_data[,'Analyte']),'signif']
    x.delta <- as.numeric((x.max-x.min)/2)####average date
    SK.min <- y.median - x.delta*slope/365.25#minimum y value for line
    SK.max <- y.median + x.delta*slope/365.25#maximum y value for line
    sub.text <- paste0("p value = " ,
                       round(p.value, digits=3),
                       ", ",  
                       p.value.label, 
                       ", slope = ", 
                       round(slope, digits=2), 
                       ", n = ", 
                       nrow(new_data))
    
    df_trend_line <- data.frame(x = c(x.min, x.max),
                                y = c(SK.min, SK.max),
                                variable = rep('Trend line', 2))
    
    # summer_stns <- c("10456", "10458", "10459", "10461")
    # trib_stns <- c('10469', '10480')
    
    # if(unique(new_data$Station_ID) %in% trib_stns) {
    #   d <- data.frame(x = c(x.min, x.max), 
    #                   y = rep(unique(new_data$TP_Concentrations_DailyMaximum), 2),
    #                   z = rep(unique(new_data$TP_Concentrations_SummerMedian), 2),
    #                   variable_y = rep('Daily Maximum TP Allocation', 2),
    #                   variable_z = rep('Summer Median TP Allocation'), 2)
    # } else {
      d <- data.frame(x = c(x.min, x.max), 
                      y = c(0.09, 0.09),
                      variable = rep('Summer Median TP Loading Capacity', 2))
    #}
    
    
    # if(unique(new_data$Station_ID) %in% trib_stns) {
    #   g <- ggplot(data = new_data, aes(x = Sampled, y = Result)) +
    #     geom_point(aes(color = exceed)) +
    #     xlim(x.lim) +
    #     ylim(y.lim) +
    #     theme(plot.title = element_text(vjust=1.5, face="bold", size = 10))+
    #     ggtitle(bquote(atop(.(title), atop(paste(.(sub.text)))))) +
    #     theme(legend.position = "top",
    #           legend.title = element_blank(),
    #           legend.direction = 'horizontal') +
    #     xlab(x.lab) +
    #     ylab(y.lab)
    #   g <- g + geom_line(aes(x = x, y = y, color = variable_y), data = d)
    #   g <- g + geom_line(aes(x = x, y = z, color = variable_z), data = d)
    #   
    #   if (plot_trend & !is.na(p.value)) {
    #     g <- g + geom_line(aes(x = x, y = y, color = variable), data = df_trend_line)  
    #   }
    #   
    #   if (plot_trend & !is.na(p.value)) {
    #     if ('Exceeds' %in% unique(new_data$exceed)) { #with exceedances
    #       meet<-new_data %>% filter(exceed == 'Meets') 
    #       if (nrow(meet) < 1) {
    #         g <-g + scale_color_manual("", values = c('darkorange1','blue', 'black'),
    #                                    guide = guide_legend(override.aes = list(
    #                                      linetype = c('solid')))) 
    #       } else {
    #         g <- g + scale_color_manual("", values = c('red4', 'darkorange1', 'black', 'grey', 'blue'),
    #                                     guide = guide_legend(override.aes = list(
    #                                       linetype = c('solid', 'solid', 'solid', 'solid', 'solid'))))
    #       }
    #     } else { #without exceedances
    #       g <- g + scale_color_manual("", values = c('blue', 'black', 'black'),
    #                                   guide = guide_legend(override.aes = list(
    #                                     linetype = c('solid'))))
    #     }
    #   } else {
    #     if ('Exceeds' %in% unique(new_data$exceed)) {
    #       meet<-new_data %>% filter(exceed == 'Meets')
    #       if(nrow(meet) < 1) {
    #         g <-g + scale_color_manual("", values = c('darkorange1', 'black'),
    #                                    guide = guide_legend(override.aes = list(
    #                                      linetype = c('solid'))))
    #       } else {
    #         g <- g + scale_color_manual("", values = c('red4', 'darkorange1', 'black', 'grey'),
    #                                     guide = guide_legend(override.aes = list(
    #                                       linetype = c('solid', 'solid', 'solid', 'solid'))))
    #       }
    #     } else {
    #       g <- g + scale_color_manual("", values = c('black', 'black', 'black', 'black'),
    #                                   guide = guide_legend(override.aes = list(
    #                                     linetype = c('solid', 'solid'))))
    #     }
    #   } 
    #   
    # } else {
      
      g <- ggplot(data = new_data, aes(x = Sampled, y = Result)) +
        geom_point(aes(color = exceed)) +
        xlim(x.lim) +
        ylim(y.lim) +
        theme(plot.title = element_text(vjust=1.5, face="bold", size = 10))+
        ggtitle(bquote(atop(.(title), atop(paste(.(sub.text)))))) +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.direction = 'horizontal') +
        xlab(x.lab) +
        ylab(y.lab)
      g <- g + geom_line(aes(x = x, y = y, color = variable), data = d)
      
      if (plot_trend & !is.na(p.value)) {
        g <- g + geom_line(aes(x = x, y = y, color = variable), data = df_trend_line)  
      }
      
      if (plot_trend & !is.na(p.value)) {
        if ('Exceeds' %in% unique(new_data$exceed)) { #with exceedances
          meet<-new_data %>% filter(exceed == 'Meets') 
          if (nrow(meet) < 1) {
            g <-g + scale_color_manual("", values = c('darkorange1','blue', 'black'),
                                       guide = guide_legend(override.aes = list(
                                         linetype = c('solid')))) 
          } else { #### THIS IS THE CODE THAT IS USED
            g <- g + scale_color_manual("", values = c('darkorange1', 'black', 'grey' ,'blue'),
                                        guide = guide_legend(override.aes = list(
                                          linetype = c('solid', 'solid', 'solid','solid')))) 
          }
        } else { #without exceedances
          g <- g + scale_color_manual("", values = c('blue', 'black', 'black'),
                                      guide = guide_legend(override.aes = list(
                                        linetype = c('solid'))))
        }
      } else {
        if ('Exceeds' %in% unique(new_data$exceed)) {
          meet<-new_data %>% filter(exceed == 'Meets')
          if(nrow(meet) < 1) {
            g <-g + scale_color_manual("", values = c('darkorange1', 'black'),
                                       guide = guide_legend(override.aes = list(
                                         linetype = c('solid'))))
          } else { #### THIS IS THE CODE THAT IS USED
            g <- g + scale_color_manual("", values = c('darkorange1','black', 'grey'),
                                        guide = guide_legend(override.aes = list(
                                          linetype = c('solid', 'solid', 'solid'))))
          }
        } else {
          g <- g + scale_color_manual("", values = c('black', 'black', 'black', 'black'),
                                      guide = guide_legend(override.aes = list(
                                        linetype = c('solid', 'solid'))))
        }
     
    
    g 
    
    }
}
  
  


