#Extract SST of edge using jplMURSST41 data. Has data from 2002-06 to 2017-11. 

source("/Packages + Coordinates.R")
dat <- read.csv("/alldates_allsites_pc1_distance.csv") #csv file created from mapping the inner edge

extractedge <- function(area) {
  
  setwd("D:/jplmursst41_QLD") 
  files <- list.files()
  
  for (i in 1:length(files)) {
    
    tryCatch({
      sst <- raster(files[i]) #rasterise each SST file
      area1 <- extent(c(xmin=152.7,xmax=155,ymin=-28.25,ymax=-25.75)) #set extent to Southeast QLD
      sst <- crop(sst, area1)
      sst <- aggregate(sst, fact=10, fun=mean) #aggregate resolution of jplmursst (1km) to the resolution of BlueLink OFAM3 (10km) using the mean
      date <- gsub(".nc", "",files[i]) #file names are dates
      dat1 <- dat[dat$date == date,] #extract date
      xy <- data.frame(c(as.numeric(dat1[dat1$site == area,]["max.lon"]),  #get lon of max env gradient
                         dat1[dat1$site == area,]["net.lat"])) #get latitude of shark net
      
      names(xy) <- c("max.lon", "net.lat")
      xy <- c(xy$max.lon, xy$net.lat)
      xy <- data.frame(ID = 1:2, X = xy[1], Y = xy[2]) 
      coordinates(xy) <- c("X", "Y")
      crs(xy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
      sc <- extract(sst, xy)[1] #extract the SST of the max lon
      sstedge[nrow(sstedge)+1,] <<- c(date, sc, site = area) #similar to rbind(), add info to sstedge dataframe
      print(nrow(sstedge))
    }, error=function(e){})
    
  }
}

sstedge <- data.frame(date = numeric(), sstedge = numeric(), site = numeric())
extractedge("Noosa")
noosaedgesst <- sstedge

sstedge <- data.frame(date = numeric(), sstedge = numeric(), site = numeric())
extractedge("Sunshine Coast") 
sunshinecoastedgesst <- sstedge

sstedge <- data.frame(date = numeric(), sstedge = numeric(), site = numeric())
extractedge("Rainbow Beach")
rainbowbeachedgesst <- sstedge

sstedge <- data.frame(date = numeric(), sstedge = numeric(), site = numeric())
extractedge("Gold Coast")
goldcoastedgesst <- sstedge

sstedge2002to2017 <- rbind(noosaedgesst, sunshinecoastedgesst, rainbowbeachedgesst, 
                          goldcoastedgesst)

write.csv(sstedge2002to2017, "/mursst_2002_to_2017.csv", row.names = F)

