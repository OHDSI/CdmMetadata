
.buildAchillesTs <- function(achillesResultSet) {
  
  AchillesData <- achillesResultSet
  
  # Convert YYYYMMDD string into a valid date    
  AchillesData$START_DATE <- as.Date(AchillesData$START_DATE,"%Y%m%d")
  
  # Create a vector of dense dates to capture all dates between the start and end of the time series
  lastRow <- nrow(AchillesData)
  
  denseDates <- seq.Date(
    from = as.Date(AchillesData$START_DATE[1],"%Y%m%d"),
    to   = as.Date(AchillesData$START_DATE[lastRow],"%Y%m%d"), 
    by   = "month" 
  )
  
  # Find gaps in the Achilles data (ie, months where there are no counts) and give that month a 0 count and 0 prevalence
  denseDatesDf <- data.frame(START_DATE=denseDates, CNT=rep(0,length(denseDates)))
  
  joinResults <- dplyr::left_join(denseDatesDf,AchillesData,by=c("START_DATE" = "START_DATE"))
  
  joinResults$COUNT_VALUE[which(is.na(joinResults$COUNT_VALUE))] <- 0
  joinResults$PREVALENCE[which(is.na(joinResults$PREVALENCE))]   <- 0
  
  # Now that we no longer have sparse dates, keep only dates, counts, and prevalences from Achilles and build the time series
  joinResults <- joinResults[,c("START_DATE","COUNT_VALUE","PREVALENCE")]
  
  # Find the end of the dense results
  lastRow <- nrow(joinResults)
  
  # Create the bivariate time series
  tsData <- data.frame(COUNT_VALUE=joinResults$COUNT_VALUE,PREVALENCE=joinResults$PREVALENCE)
  
  ts(
    data      = tsData, 
    start     = c(as.numeric(substring(joinResults$START_DATE[1],1,4)),as.numeric(substring(joinResults$START_DATE[1],6,7))), 
    end       = c(as.numeric(substring(joinResults$START_DATE[lastRow],1,4)),as.numeric(substring(joinResults$START_DATE[lastRow],6,7))),
    frequency = 12
  )
}