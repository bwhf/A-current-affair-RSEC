#Packages and dataframe of coordinates used in analysis

##### (Central) Net Locations at each site ######

# Will aggregate entanglements by site for analysis. Chose the central net
# at each 'site' to represent all entanglements for that site. Write the below coordinates into
# a reference dataframe.

#Gold Coast (lat/lon Burleigh Beach-ish) - 153.454, -28.0763
#Rainbow Beach - 153.097, -25.894
#Noosa Beach - 153.089, -26.381
#Sunshine Coast (lat/lon Twin Waters-ish) - 153.105, -26.6295

latlons_nets <- data.frame(site=c("Gold Coast", "Rainbow Beach",
                                  "Noosa", "Sunshine Coast"), 
                           lon=c(153.454, 153.1156, 153.0956, 153.1355),
                           lat=c(-28.0763, -25.87189, -26.35045, -26.62961))

#### Packages
for (pkg in c("raster", "ncdf4", "colorRamps", "rgeos", "maptools", "geosphere", "maps", "mapdata",
              "lubridate", "SOAR", "plyr", "measurements")) {
  library(pkg, character.only = TRUE)
}


# 200m contour used for isobath in Southeast Queensland
#shape <- shapefile("C:/Users/jessi/Dropbox/Whale analysis/Raw data/ausbath_09_v4_contour_simp300m.gs.shp/ausbath_09_v4_contour_200m_simp300m.gs.shp/ausbath_09_v4_contour_200m_simp300m.shp")
shape <- shapefile("/Users/jessicabolin/Dropbox/Whale analysis/Raw data/ausbath_09_v4_contour_simp300m.gs.shp/ausbath_09_v4_contour_200m_simp300m.gs.shp/ausbath_09_v4_contour_200m_simp300m.shp")

#Extent object for SEQ
ex <- c(152.5, 155, -28.5, -25.5)
cropped <- crop(shape, ex)
