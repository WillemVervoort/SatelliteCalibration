# Transform the observed Santa Lucia precipitation data
# into SWAT format in the pcp.pcp file
# also need to adjust the .sub files to indicate the right gage
# and need to finally adjust file.cio to indicate no of gages
# Willem Vervoort 2017-08-08

# Identify which gauge is closest to the subbasin centroid
library(rgeos)
library(sp)
library(rgdal)
setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/Resources/UruguaySWAT/SantaLuciaSWAT")

# read in the station locations and the subbasin points
Stations <- read.csv("Precipitacion_SantaLucia_coordinates.csv")
Subbasins <- read.csv("../Uruguaycourse/data/Subbasins_SantaLucia.csv")
# read in the precipitation data
Pdata <- read.csv("Precipitacion_SantaLucia.csv", 
                  na.strings="NaN")
head(Pdata)     

Dates <- as.Date(paste(Pdata[,1],Pdata[,2],
                       Pdata[,3],sep="-"))

# find the stations that have 90% of data after 2001
Pdata_2000 <- Pdata[Dates >= "2000-01-01",]
result <- rep(0,(ncol(Pdata_2000)-3))
for (i in 4:ncol(Pdata_2000)) {
  result[i-3] <- sum(ifelse(is.na(Pdata_2000[,i]),1,0))/nrow(Pdata_2000)
}

# result indicates the fraction of NA data for the stations 
# throw out all the columns and rows where result >0.1
Pdata_new <- Pdata_2000[,-(which(result>0.1)+3)]
Stations <- Stations[-which(result>0.1),]

sub_b_sp <- SpatialPoints(cbind(Subbasins$Long_, Subbasins$Lat),
                          proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))

#assuming UTM = EPSG:29181 (21S) for stations
Stat_UTM <- SpatialPoints(cbind(Stations$X_UTM,Stations$Y_UTM), 
                    proj4string=CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"))
longlatcoor<-spTransform(Stat_UTM,CRS("+proj=longlat +datum=WGS84 +no_defs"))

# check on plot
plot(longlatcoor)
points(sub_b_sp,col="blue")


# Find stations closest to subbasins centroids.
Stations_nearSub <- apply(gDistance(longlatcoor, 
                                    sub_b_sp, byid=TRUE), 
                          1, which.min)

# Read in the subfiles and put in the numbers of the pcp stations
# first order the pcp stations and renumber
order_sta <- Stations_nearSub[order(Stations_nearSub)]
head(order_sta)
ranking <- unique(order_sta)
# new data frame
numbers_to_sub <- data.frame(sub =as.numeric(names(order_sta)),
                             station = order_sta)
# match rank with station
for (i in 1:nrow(numbers_to_sub)) {
  no <- 1:length(ranking)
  numbers_to_sub$rank[i] <- no[ranking %in% numbers_to_sub$station[i]]
}
# find all the *.sub files in the txtinout directory
subfiles <- list.files("SantaLuciaSWAT/scenarios/default/txtinout",
                       pattern = ".sub")

# rewrite all the sub files
for (i in 1:nrow(numbers_to_sub)) {
  # find the specific subbasins for each weather station
  foo <- subfiles[grep(as.character(numbers_to_sub$sub[i]),
                       subfiles)[1]]
  foo_b <- paste("SantaLuciaSWAT/scenarios/default/txtinout/",
                 foo,sep="")
  # open the file to read the text lines
  bar <- file(foo_b,"r+")
  data_bar <- readLines(bar)
  # close the file after reading the lines
  close(bar)
  # rewrite line 7, which is the line that indicates the weather station to use
  data_bar[7] <- paste("        ",
                       numbers_to_sub$rank[i],
                       "      | IRGAGE: precip gage data used in subbasin")
  # rewrite the file to disk
    write(data_bar,foo_b)
}

# Now write the SWAT pcp file with only the gauges 
# that are close to subbasins, but ordered
Dates <- as.Date(paste(Pdata_new[,1],Pdata_new[,2],
                       Pdata_new[,3],sep="-"))
head(Dates)
Pdata_select <- data.frame(Dates = Dates)
# select only the stations close to subbasin centroids
for (i in 1:length(ranking)) {
  Pdata_select[,i+1] <- Pdata_new[,ranking[i]+3]
}

Pdata_select[is.na(Pdata_select)] <- -99
colnames(Pdata_select) <- c("Dates",ranking)

# format Dates to be like SWAT
Pdata_select$Dates <- format(Pdata_select$Dates, "%Y%j")

# write to a file
write(c("Sta", ranking),"testPCP.txt", ncol=13)
write(c("Lati", round(coordinates(longlatcoor)[ranking,1],1)),"testPCP.txt", append = T, ncol=13)
write(c("Long", round(coordinates(longlatcoor)[ranking,2],1)),"testPCP.txt", append = T ,ncol=13)
write(format(c("Elev", rep(25,12)),width=5,format="d"),"testPCP.txt", append = T ,ncol=13)
write.table(cbind(format(Pdata_select[,1],width=7, format="d"),
        format(Pdata_select[,2:ncol(Pdata_select)],
               width=5, format="f",digits=1)),
             "testPCP.txt", append = T ,col.names=F,row.names=F,
        quote=F,sep="")

