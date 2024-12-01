ConvertCoordsToICPF <- function(
    LatitudeWGS84Decimal = NA,
    LongitudeWGS84Decimal = NA
  ) {
  
  #Converts from WGS84 decimal representation to ICP Forests coordinate format:
  #Degrees Minutes Seconds (WGS84) E.g. '+505852' = 50 Degree, 58 Minutes, 52 Seconds
  
  lats <- LatitudeWGS84Decimal
  lons <- LongitudeWGS84Decimal
  
  #Drop trailing NAs and empty string-----
  while ( is.na(lats[length(lats)]) | (lats[length(lats)] == "") ) {
    lats <- lats[-length(lats)]
  }
  while ( is.na(lons[length(lons)]) | (lons[length(lons)] == "") ) {
    lons <- lons[-length(lons)]
  }
  
  #Check lengths of vectors-----
  n <- length(lats)
  if ( length(lons) != n ) stop("Number of rows for lats and lons is not identical.")
  
  #Error on NAs in between----------
  if ( any(is.na(lats)) ) {
    stop("lats contain NA")
  }
  if ( any(is.na(lons)) ) {
    stop("lons contain NA")
  }
  
  #Prepare output-----
  Output <- data.frame(
    Lat_Deg = rep(NA, n),
    Lat_Min = rep(NA, n), 
    Lat_Sec = rep(NA, n),
    Lon_Deg = rep(NA, n),
    Lon_Min = rep(NA, n), 
    Lon_Sec = rep(NA, n),
    Lat_WGS84 = rep(NA, n),
    Lon_WGS84 = rep(NA, n)
  )
  
  #Parse----
  for ( i in 1:n ) {
    
    CurrentLat <- lats[i]
    CurrentLon <- lons[i]
    
    #_Degree to numeric-----
    DegLatNum <- suppressWarnings(expr = {
      as.numeric(CurrentLat)
    })
    if ( is.na(DegLatNum) ) {
      stop(paste("Latitude is not numeric:", CurrentLat))
    }
    
    DegLonNum <- suppressWarnings(expr = {
      as.numeric(CurrentLon)
    })
    if ( is.na(DegLonNum) ) {
      stop(paste("Longitude is not numeric:", CurrentLon))
    }
    
    
    #_Degree ICPF region boundary------
    LatMinimum <- 0
    LatMaximum <- 90
    LonMinimum <- -40 #Todo: Check
    LonMaximum <- 90 #Todo: Check
    
    if ( (DegLatNum < LatMinimum) | (DegLatNum > LatMaximum) ) {
      stop(paste(
        "Latitude is out of bounds of ICPF region",
        "(", LatMinimum, "to", LatMaximum, "):",
        CurrentLat
      ))
    }
    
    if ( (DegLonNum < LonMinimum) | (DegLonNum > LonMaximum) ) {
      stop(paste(
        "Longitude is out of bounds of ICPF region",
        "(", LonMinimum, "to", LonMaximum, "):",
        CurrentLon
      ))
    }
    
    #_Accept Degree----
    Output$Lat_WGS84[i] <- DegLatNum
    Output$Lon_WGS84[i] <- DegLonNum
    
  }
  
  #Convert-----
  
  #Take integer part for degrees
  Output$Lat_Deg <- Output$Lat_WGS84 - (Output$Lat_WGS84 %% 1)
  Output$Lon_Deg <- Output$Lon_WGS84 - (Output$Lon_WGS84 %% 1)
        
  #Take integer part of minutes
  Output$Lat_Min <- (Output$Lat_WGS84 - Output$Lat_Deg) * 60
  Output$Lat_Min <- Output$Lat_Min - (Output$Lat_Min %% 1)
  Output$Lon_Min <- (Output$Lon_WGS84 - Output$Lon_Deg) * 60
  Output$Lon_Min <- Output$Lon_Min - (Output$Lon_Min %% 1)
  
  #Calculate seconds
  Output$Lat_Sec = round((Output$Lat_WGS84 - Output$Lat_Deg - Output$Lat_Min/60) * 3600, 0)
  Output$Lon_Sec = round((Output$Lon_WGS84 - Output$Lon_Deg - Output$Lon_Min/60) * 3600, 0)

  #Zero padding
  Output$Lat_Deg <- sprintf(fmt = "%02d", Output$Lat_Deg)
  Output$Lon_Deg <- sprintf(fmt = "%02d", Output$Lon_Deg)
  Output$Lat_Min <- sprintf(fmt = "%02d", Output$Lat_Min)
  Output$Lon_Min <- sprintf(fmt = "%02d", Output$Lon_Min)
  Output$Lat_Sec <- sprintf(fmt = "%02d", Output$Lat_Sec)
  Output$Lon_Sec <- sprintf(fmt = "%02d", Output$Lon_Sec)
  Output$Lat_ICFP <- paste0(Output$Lat_Deg, Output$Lat_Min, Output$Lat_Sec)
  Output$Lon_ICFP <- paste0(Output$Lon_Deg, Output$Lon_Min, Output$Lon_Sec)
  
  #Leading sign
  for ( i in 1:nrow(Output) ) {
    
    if ( Output$Lat_WGS84[i] < 0 ) {
      Output$Lat_ICFP[i] <- paste0("-", Output$Lat_ICFP[i])
    } else {
      Output$Lat_ICFP[i] <- paste0("+", Output$Lat_ICFP[i])
    }
    
    if ( Output$Lon_WGS84[i] < 0 ) {
      Output$Lon_ICFP[i] <- paste0("-", Output$Lon_ICFP[i])
    } else {
      Output$Lon_ICFP[i] <- paste0("+", Output$Lon_ICFP[i])
    }
    
  }
  
  #Return----
  Output <- Output[,c("Lat_ICFP", "Lon_ICFP")]
  return(Output)
  
}


