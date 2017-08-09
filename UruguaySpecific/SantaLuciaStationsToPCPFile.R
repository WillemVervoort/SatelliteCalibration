# Transform the observed Santa Lucia precipitation data
# into SWAT format in the pcp.pcp file
# also need to adjust the .sub files to indicate the right gage
# and need to finally adjust file.cio to indicate no of gages

# Identify which gauge is closest to the subbasin centroid
library(rgeos)
library(sp)
library(rgdal)
setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/Resources/UruguaySWAT/SantaLuciaSWAT")

# read in the station locations and the subbasin points
Stations <- read.csv("Precipitacion_SantaLucia_coordinates.csv")
Subbasins <- read.csv("../Uruguaycourse/data/Subbasins_SantaLucia.csv")

sub_b_sp <- SpatialPoints(cbind(Subbasins$Long_, Subbasins$Lat),
                          proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))

#assuming UTM = EPSG:29181 (21S) for stations
Stat_UTM <- SpatialPoints(cbind(Stations$X_UTM,Stations$Y_UTM), 
                    proj4string=CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"))
longlatcoor<-spTransform(Stat_UTM,CRS("+proj=longlat +datum=WGS84 +no_defs"))

# check on plot
plot(longlatcoor)
points(sub_b_sp,col="blue")

Stations_nearSub <- apply(gDistance(longlatcoor, 
                                    sub_b_sp, byid=TRUE), 
                          1, which.min)




Rivers <- data.frame(Rivers,  Stationssp[Rivers$nearest_in_Stations],station_quality[Rivers$nearest_in_Stations,][c(2,4)])
colnames(Rivers)[11:14] <- c("Nearest Lon", "Nearest Lat", "Station_No", "Station_Name")

