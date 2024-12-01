ConvertCoordsFromICPF <- function(lats,lons) {
  #Converts from ICP Forests coordinate format
  #(Degrees Seconds Seconds (WGS84) E.g. '+505852' = 50 Degree, 58 Seconds, 52 Seconds)
  #to WGS84 decimal representation
  
  # require("tidyverse")
  require("stringr")
  
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
    
    #_String length----
    if ( nchar(CurrentLat) != 7 ) {
      stop(paste("Latitude must be of 7 chararacters long (e.g. +502027):", CurrentLat))
    }
    if ( nchar(CurrentLon) != 7 ) {
      stop(paste("Longitude must be of 7 chararacters long (e.g. +502027):", CurrentLon))
    }
    
    #_Leading plus minus------
    SignLat <- NA
    SignLon <- NA
    
    if ( substr(x = CurrentLat, start = 1, stop = 1) == "+" ) {
      SignLat <- "plus"
    } else if ( substr(x = CurrentLat, start = 1, stop = 1) == "-" ) {
      SignLat <- "minus"
    } else {
      stop(paste("Latitude does not start with + or -:", CurrentLat))
    }
    
    if ( substr(x = CurrentLon, start = 1, stop = 1) == "+" ) {
      SignLon <- "plus"
    } else if ( substr(x = CurrentLon, start = 1, stop = 1) == "-" ) {
      SignLon <- "minus"
    } else {
      stop(paste("Longitude does not start with + or -:", CurrentLon))
    }
    
    #_Degree to numeric-----
    DegLat <- substr(x = CurrentLat, start = 1, stop = 3)
    DegLatNum <- suppressWarnings(expr = {
      as.numeric(DegLat)
    })
    if ( is.na(DegLatNum) ) {
      stop(paste("Latitude is not numeric:", CurrentLat))
    }
    
    DegLon <- substr(x = CurrentLon, start = 1, stop = 3)
    DegLonNum <- suppressWarnings(expr = {
      as.numeric(DegLon)
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
    Output$Lat_Deg[i] <- DegLatNum
    Output$Lon_Deg[i] <- DegLonNum
    
    #_Minutes numeric------
    MinLat <- substr(x = CurrentLat, start = 4, stop = 5)
    MinLatNum <- suppressWarnings(expr = {
      as.numeric(MinLat)
    })
    if ( is.na(MinLatNum) ) {
      stop(paste("Latitude Seconds are not numeric:", CurrentLat))
    }
    
    MinLon <- substr(x = CurrentLon, start = 4, stop = 5)
    MinLonNum <- suppressWarnings(expr = {
      as.numeric(MinLon)
    })
    if ( is.na(MinLonNum) ) {
      stop(paste("Lonitude Seconds are not numeric:", CurrentLon))
    }
    
    #_Minutres in range 0-59-----
    if ( MinLatNum > 59 ) {
      stop(paste("Latitude Seconds are larger than 59:", CurrentLat))
    }
    if ( MinLonNum > 59 ) {
      stop(paste("Lonitude Seconds are larger than 59:", CurrentLon))
    }
    
    #_Accept Minutes----
    Output$Lat_Min[i] <- MinLatNum
    Output$Lon_Min[i] <- MinLonNum


    #_Seconds numeric------
    SecLat <- substr(x = CurrentLat, start = 6, stop = 7)
    SecLatNum <- suppressWarnings(expr = {
      as.numeric(SecLat)
    })
    if ( is.na(SecLatNum) ) {
      stop(paste("Latitude Seconds are not numeric:", CurrentLat))
    }
    
    SecLon <- substr(x = CurrentLon, start = 6, stop = 7)
    SecLonNum <- suppressWarnings(expr = {
      as.numeric(SecLon)
    })
    if ( is.na(SecLonNum) ) {
      stop(paste("Lonitude Seconds are not numeric:", CurrentLon))
    }
    
    #_Seconds in range 0-59-----
    if ( SecLatNum > 59 ) {
      stop(paste("Latitude Seconds are larger than 59:", CurrentLat))
    }
    if ( SecLonNum > 59 ) {
      stop(paste("Lonitude Seconds are larger than 59:", CurrentLon))
    }
    
    #_Accept Seconds----
    Output$Lat_Sec[i] <- SecLatNum
    Output$Lon_Sec[i] <- SecLonNum
        
    
  } #end of loop over each element in lats/lons
  
  
  #Convert-----
  nDigits <- 6
  Output$Lat_WGS84 <- Output$Lat_Deg + Output$Lat_Min/60 + Output$Lat_Sec/3600
  Output$Lon_WGS84 <- Output$Lon_Deg + Output$Lon_Min/60 + Output$Lon_Sec/3600
  Output$Lat_WGS84 <- round(Output$Lat_WGS84, nDigits)
  Output$Lon_WGS84 <- round(Output$Lon_WGS84, nDigits)  
  Output$Lat_WGS84 <- sprintf(fmt = paste0("%.", nDigits ,"f"), Output$Lat_WGS84)
  Output$Lon_WGS84 <- sprintf(fmt = paste0("%.", nDigits ,"f"), Output$Lon_WGS84)
  
  #Return-----
  Output <- Output[, c("Lat_WGS84", "Lon_WGS84")]
  return(Output)

}

