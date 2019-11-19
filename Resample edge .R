#Resample edge sst
#2018-08-01
#Jessie BOLIN
source("/Users/jessicabolin/Desktop/Whale analysis/Source code/Packages + Coordinates.R")

setwd("/Users/jessicabolin/Desktop/Whale analysis/8. Resampling/Edge")
dat <- read.csv("/Users/jessicabolin/Desktop/Whale analysis/7 - Merge data/master_edge.csv")
dat <- dat[,-1] #get rid of first column (row numbers)
names(dat)[2] <- "area" #overwrite 'site' to 'area'
dat$date <- as.Date(dat$date)

#Subset each area/PA combo
nogoldcoast <- subset(dat, area == "Gold Coast" & PA == 0)
nosunshinecoast <- subset(dat, area == "Sunshine Coast" & PA == 0)
nonoosa <- subset(dat, area == "Noosa" & PA == 0)
norainbowbeach <- subset(dat, area == "Rainbow Beach" & PA == 0)

entangled <- dat[dat$PA == 1,]
yesgoldcoast <- subset(entangled, area == "Gold Coast")
yessunshinecoast <- subset(entangled, area == "Sunshine Coast")
yesnoosa <- subset(entangled, area == "Noosa")
yesrainbowbeach <- subset(entangled, area == "Rainbow Beach")

modelinputs <- list() #model inputs go into this object

the_supreme_bootstrap_function <- function(area, area_abbreviated) {
  
  months <- month(get(paste0("yes", area))$date)
  years <- year(get(paste0("yes", area))$date)
  lengthy <- length(get(paste0("yes", area))$PA)
  smonths <- sample(months, size = lengthy * 4, replace = TRUE)
  syears <- sample(years, size = lengthy * 4, replace = TRUE)
  days <- c(1:30)
  sdays <- sample(days, size = lengthy * 4, replace = TRUE)
  sdf <- data.frame(day = sdays, month = smonths, year = syears)#put date vectors into a dataframe
  sdf <- sdf[!duplicated(sdf),] #exclude duplicate dates
  #force the vectors in each row together into a single vector, and coerce that vector to class: date
  randomdates <- as.Date(with(sdf, paste(year, month, day,sep="-")), "%Y-%m-%d")
  assign(paste("dates_model_input", area_abbreviated, i, sep = "_"), randomdates)
  #above is my random sample dates drawn from my actual entanglement positive days...
  thedata <- data.frame(matrix(NA, nrow = length(get(paste0("dates_model_input_", 
                                                            area_abbreviated, "_",i))), ncol = 1))
  thedata[,1] <- get(paste0("dates_model_input_", area_abbreviated, "_", i))
  names(thedata) <- c("date")
  zeros <- get(paste0("no", area))[get(paste0("no", area))$date %in% thedata$date,]
  assign(paste0("the", area), rbind(zeros, get(paste0("yes", area))), envir = globalenv())
  
}


for (i in 1:1000) { #Why 1000, I hear you ask? KS said so
  
  the_supreme_bootstrap_function("goldcoast", "gc")
  the_supreme_bootstrap_function("rainbowbeach", "rb")
  the_supreme_bootstrap_function("sunshinecoast", "sc")
  the_supreme_bootstrap_function("noosa", "no")
  
  assign(paste0("modelinput", i), rbind(thegoldcoast, thenoosa,
                                        therainbowbeach, thesunshinecoast))
  Store(paste0("modelinput", i)) #store in SOAR cache in sub-wd()
  modelinputs[[i]] <- get(paste0("modelinput", i))
  print(i)
}

length(modelinputs)
#Test, to make sure it worked
test <- modelinput866 
subset(test, area =="Noosa")
subset(test, area == "Rainbow Beach") #should be 6 entanglements, and there are
subset(test, area=="Sunshine Coast") #3 entanglements now (removed twin waters)
which(is.na(test))
55 + (4*55) #this is no. observations I expect to have , but will have less as I excluded duplicates
nrow(test) #good enough!

modelinput1
