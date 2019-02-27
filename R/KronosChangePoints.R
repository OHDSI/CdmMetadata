
.getTrendChangePoints <- function(AchillesTs, changeLength = 12, changeLowerBounds = .75) {
  
  tsObj       <- AchillesTs
  tsObj.stl   <- stats::stl(tsObj,s.window="periodic",robust=T)
  tsObj.trend <- tsObj.stl$time.series[,"trend"]
  
  
  # Find inflection points.
  tsObj.trend.ip <- which(diff(sign(diff(tsObj.trend)))!=0)+1 
  
  # Determine if new direction is continued for changeLength consecutive months.
  # Since the last inflection point found doesn't have a diff, append changeLength so it doesn't get excluded prematurely.
  tsObj.trend.ip <- tsObj.trend.ip[which(c(diff(tsObj.trend.ip),changeLength) >= changeLength)]
  
  # Determine the magnitude of the change (i.e., how large is it compared to other changeLength consecutive month changes?).
  # We'll find change points whose magnitude fall into the following percentiles: 50th, 75th, 90th, 95th, and 99th.
  # These change points correspond to severity levels 1 through 5.
  
  cpSeverity       <- NULL
  cpIndex          <- NULL
  cpDate           <- NULL
  cpVal            <- NULL
  
  hasSev1Cp        <- FALSE
  hasSev2Cp        <- FALSE
  hasSev3Cp        <- FALSE
  hasSev4Cp        <- FALSE
  hasSev5Cp        <- FALSE
  
  cutOff           <- length(tsObj.trend)-changeLength
  diffSizes        <- NULL
  tsObj.trend.sev1 <- NULL
  tsObj.trend.sev2 <- NULL
  tsObj.trend.sev3 <- NULL
  tsObj.trend.sev4 <- NULL
  tsObj.trend.sev5 <- NULL
  
  for (k in 1:cutOff) {
    diffSizes[k] <- sum(abs(diff(tsObj.trend[k:(k+changeLength)])))
  }
  
  percentiles <- quantile(diffSizes, probs=c(.5,.75,.9,.95,.99))
  
  tsObj.trend.sev1 <- intersect(tsObj.trend.ip,which(diffSizes >= percentiles[1]))
  tsObj.trend.sev2 <- intersect(tsObj.trend.ip,which(diffSizes >= percentiles[2]))
  tsObj.trend.sev3 <- intersect(tsObj.trend.ip,which(diffSizes >= percentiles[3]))
  tsObj.trend.sev4 <- intersect(tsObj.trend.ip,which(diffSizes >= percentiles[4]))
  tsObj.trend.sev5 <- intersect(tsObj.trend.ip,which(diffSizes >= percentiles[5]))
  
  tsObj.trend.sev1 <- setdiff(tsObj.trend.sev1,tsObj.trend.sev2)
  tsObj.trend.sev2 <- setdiff(tsObj.trend.sev2,tsObj.trend.sev3)
  tsObj.trend.sev3 <- setdiff(tsObj.trend.sev3,tsObj.trend.sev4)
  tsObj.trend.sev4 <- setdiff(tsObj.trend.sev4,tsObj.trend.sev5)
  tsObj.trend.sev5 <- tsObj.trend.sev5
  
  # Determine which change points types have been found
  if (length(tsObj.trend.sev1) > 0) hasSev1Cp <- TRUE
  if (length(tsObj.trend.sev2) > 0) hasSev2Cp <- TRUE
  if (length(tsObj.trend.sev3) > 0) hasSev3Cp <- TRUE
  if (length(tsObj.trend.sev4) > 0) hasSev4Cp <- TRUE
  if (length(tsObj.trend.sev5) > 0) hasSev5Cp <- TRUE
  
  
  # Determine which change points to return
  # Only return change points that exist at or above the given threshold
  
  if (changeLowerBounds == .5 & any(hasSev1Cp,hasSev2Cp,hasSev3Cp,hasSev4Cp,hasSev5Cp)) {
    
    cpSeverity   <- rep(1:5,c(length(tsObj.trend.sev1),length(tsObj.trend.sev2),length(tsObj.trend.sev3),length(tsObj.trend.sev4),length(tsObj.trend.sev5)))
    year         <- .getYear(tsObj.trend,c(tsObj.trend.sev1,tsObj.trend.sev2,tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5))
    month        <- .getMonth(tsObj.trend,c(tsObj.trend.sev1,tsObj.trend.sev2,tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5))
    cpIndex      <- c(tsObj.trend.sev1,tsObj.trend.sev2,tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5)
    cpDate       <- as.Date(paste0(year,month.name[month],"01"),"%Y%B%d")
    cpVal        <- tsObj.trend[c(tsObj.trend.sev1,tsObj.trend.sev2,tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5)]
    
  } else if (changeLowerBounds == .75 & any(hasSev2Cp,hasSev3Cp,hasSev4Cp,hasSev5Cp)) {
    
    cpSeverity   <- rep(2:5,c(length(tsObj.trend.sev2),length(tsObj.trend.sev3),length(tsObj.trend.sev4),length(tsObj.trend.sev5)))
    year         <- .getYear(tsObj.trend,c(tsObj.trend.sev2,tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5))	
    month        <- .getMonth(tsObj.trend,c(tsObj.trend.sev2,tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5))
    cpIndex      <- c(tsObj.trend.sev2,tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5)
    cpDate       <- as.Date(paste0(year,month.name[month],"01"),"%Y%B%d")
    cpVal        <- tsObj.trend[c(tsObj.trend.sev2,tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5)]
    
  } else if (changeLowerBounds == .9 & any(hasSev3Cp,hasSev4Cp,hasSev5Cp)) {
    
    cpSeverity   <- rep(3:5,c(length(tsObj.trend.sev3),length(tsObj.trend.sev4),length(tsObj.trend.sev5)))
    year         <- .getYear(tsObj.trend,c(tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5))	
    month        <- .getMonth(tsObj.trend,c(tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5))
    cpIndex      <- c(tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5)
    cpDate       <- as.Date(paste0(year,month.name[month],"01"),"%Y%B%d")
    cpVal        <- tsObj.trend[c(tsObj.trend.sev3,tsObj.trend.sev4,tsObj.trend.sev5)]
    
  } else if (changeLowerBounds == .95 & any(hasSev4Cp,hasSev5Cp)) {
    
    cpSeverity   <- rep(4:5,c(length(tsObj.trend.sev4),length(tsObj.trend.sev5)))
    year         <- .getYear(tsObj.trend,c(tsObj.trend.sev4,tsObj.trend.sev5))	
    month        <- .getMonth(tsObj.trend,c(tsObj.trend.sev4,tsObj.trend.sev5))
    cpIndex      <- c(tsObj.trend.sev4,tsObj.trend.sev5)
    cpDate       <- as.Date(paste0(year,month.name[month],"01"),"%Y%B%d")
    cpVal        <- tsObj.trend[c(tsObj.trend.sev4,tsObj.trend.sev5)]
    
  } else if (changeLowerBounds == .99 & hasSev5Cp) {
    
    cpSeverity   <- rep(5,length(tsObj.trend.sev5))
    year         <- .getYear(tsObj.trend,tsObj.trend.sev5)	
    month        <- .getMonth(tsObj.trend,tsObj.trend.sev5)
    cpIndex      <- tsObj.trend.sev5
    cpDate       <- as.Date(paste0(year,month.name[month],"01"),"%Y%B%d")
    cpVal        <- tsObj.trend[tsObj.trend.sev5]
    
  } else if (!(changeLowerBounds %in% c(.5,.75, .9, .95, .99))) {
    
    stop("Invalid Lower Bound.  Valid values .5,.75, .9, .95, .99") 
  }
  
  data.frame(SEVERITY = cpSeverity, INDICES = cpIndex, DATES = cpDate, VALUES = cpVal, stringsAsFactors = F)
}