#2019-08-01
#Jessie Bolin
#Honours - extracting shark net temps

#This is step 1 (jplMURSST41)
#Extract net temp 2002-06 to 2017-11.
source("file:///C:/Users/jessi/Dropbox/For windows laptop/Edge/Packages + Coordinates.R")

library(raster)
sharknettempjpl <- function(site) {
  
  for (m in 1:length(monthdat$date)) {
    
    sst <- sststack[[m]]
    monthssss <- monthdat$date
    monthnames <- format(as.Date(monthssss),"%B")
    monthdat$month <- monthnames[m]
    yearsssss <- monthdat$date
    yearsnames <- format(as.Date(yearsssss), "%Y")
    monthdat$year <- yearsnames[m]
    monthdat$day <- c(1:30)
    xy <- data.frame(latlons_nets[latlons_nets$site == site, ])[,2:3]
    xy <- c(xy$lon, xy$lat)
    xy <- data.frame(ID = 1:2, X = xy[1], Y = xy[2])
    coordinates(xy) <- c("X", "Y")
    proj4string(xy) <- CRS("+proj=longlat +datum=WGS84") 
    nettemp <- extract(sst,xy)[1]
    #if (nettemp = NA) {}
    #nettemp <- sst@data@values[which.min(replace(distanceFromPoints(sst, xy), is.na(sst), NA))]
    monthdat$sharknettemp[m] <<- nettemp 
    print(monthdat$date)
    
  }   
  
  monthdat$area <<- site
  
}    
#2003 to 2017

biglist <- list()
massivelist <- list()

YEARS = 3:17 #change this to 2
MONTHS = 5:11 # change this to 6:11 and rerun for 2002.

for (i in min(YEARS):max(YEARS)) {
  
  for (j in min(MONTHS):max(MONTHS)) {
    
    setwd("D:/jplmursst41_QLD")
    
    files <- paste0("20", 
                    if (i < 10) { paste("0", i, sep = "") } else (paste0(i)),
                    "-", 
                    if (j < 10) { paste("0", j, sep = "") } else (paste0(j)), 
                    "-")
    
    month <- grep(paste(files, collapse = "|"), list.files(), value = TRUE)
    
    
    if (length(month) == 31) {
      delete <- grep("-31", month) #remove 31's from entanglements
      deletethem <- month[delete]
      month <- month[month != deletethem]
    }
    
    sststack <- stack(month)
    area <- extent(c(xmin=152.5,xmax=156,ymin=-28.8,ymax=-24.2))
    sststack <- crop(sststack, area)
    sststack <- aggregate(sststack, 10, mean)
    monthdat <- data.frame(matrix(NA, nrow = length(month), ncol = 2))
    names(monthdat) <- c('date', 'sharknettemp')
   
    
    #Uses the jpl file names as inputs for the date column in my dataframe.
    for (p in 1:length(month)) { 
      monthdat$date[p] <- gsub(".nc", "", month[p]) #strip the .nc part (gsub(annoying, replace, fromwhat))
    }
    outputs <- list()
    
    sharknettempjpl("Gold Coast")
    outputs[[1]] <- monthdat
    sharknettempjpl("Rainbow Beach")
    outputs[[2]] <- monthdat
    sharknettempjpl("Noosa")
    outputs[[3]] <- monthdat
    sharknettempjpl("Sunshine Coast")
    outputs[[4]] <- monthdat
    
    outputs <- do.call("rbind", outputs)
    biglist[[j]] <- outputs
    
  }
  
  massivelist[[i]] <- do.call("rbind", biglist)
  
}


bigger <- do.call("rbind", massivelist) #reran for 2003-2017 and rbinded here.
write.csv(bigger, "file:///C:/Users/jessi/Dropbox/For windows laptop/Net/jplMURSST_2003_to_2017.csv", row.names = F)


