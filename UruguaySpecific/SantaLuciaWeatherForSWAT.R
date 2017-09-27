# Convert original Las Brujas data to SWAT weather data
# version 2, add new rainfall data
library(tidyverse)
library(zoo)
library(rgeos)
library(sp)
library(rgdal)


#set working directory
setwd("R:/PRJ-HPWC/SWAT_ETCalibration/SantaLuciaData")

# read in the weather data:
WD <- read.csv("INIALasBrujas_1983-2016.csv")

# There are two different dates: 31-12-2016 and 1/1/1983
# Need to convert to standard dates
Dates <- ifelse(grepl("-",WD$Fecha)==TRUE,
                as.character(as.Date(WD$Fecha,"%d-%m-%Y")),
                as.character(as.Date(WD$Fecha,"%d/%m/%Y")))

# Check for missing data
sum(ifelse(is.na(WD[,2:ncol(WD)]),1,0))

# Add additional rainfall data
# read in the station locations 
Stations <- read.csv("Precipitacion_SantaLucia_coordinates.csv")
# read in the precipitation data
Pdata <- read.csv("Precipitacion_SantaLucia.csv", 
                  na.strings="NaN")
head(Pdata)     

Dates2 <- as.Date(paste(Pdata[,1],Pdata[,2],
                       Pdata[,3],sep="-"))

# find the stations that have 90% of data after 2001
Pdata_2000 <- Pdata[Dates2 >= "2000-01-01",]
result <- rep(0,(ncol(Pdata_2000)-3))
for (i in 4:ncol(Pdata_2000)) {
  result[i-3] <- sum(ifelse(is.na(Pdata_2000[,i]),1,0))/nrow(Pdata_2000)
}

# result indicates the fraction of NA data for the stations 
# throw out all the columns and rows where result >0.1
Pdata_new <- Pdata_2000[,-(which(result>0.1)+3)]
Stations <- Stations[-which(result>0.1),]
Dates2000 <- Dates2[Dates2 >= "2000-01-01"]

# zoo the data and replace missing data with -99.0
# Zoo the Pdata (rainfall)
Pdata_z <- zoo(Pdata_new[,4:ncol(Pdata_new)], order.by = Dates2000, frequency=1)
Pdata_z[is.na(Pdata_z)] <- -99.0


# wind data needs to be in m/s
wind_z <- zoo(WD[,5]*1000/86400, order.by =Dates)
wind_z[is.na(wind_z)] <- -99.0

# humidity
hum_z <- zoo(WD[,6], order.by =Dates)
hum_z[is.na(hum_z)] <- -99.0

# maxT
maxT_z <- zoo(WD[,3], order.by =Dates)
maxT_z[is.na(maxT_z)] <- -99.0

# minT
minT_z <- zoo(WD[,4], order.by =Dates)
minT_z[is.na(minT_z)] <- -99.0

# solar radiation needs to be in MJ/m2
# 1 cal cm-2 day-1 = 4.1868 10-2 MJ m-2 day-1
# http://www.fao.org/docrep/X0490E/x0490e0i.htm
slr_z <- zoo(WD[,2]*0.041868, order.by =Dates)
slr_z[is.na(slr_z)] <- -99.0


# Write files to SWAT input format
# the station file has the following headings
cnames <- c("ID","NAME","LAT","LONG","ELEVATION")

# convert UTM to latlong
Stat_UTM <- SpatialPoints(cbind(Stations$X_UTM,Stations$Y_UTM), 
                          proj4string=CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"))
longlatcoor<-spTransform(Stat_UTM,CRS("+proj=longlat +datum=WGS84 +no_defs"))
elev <- rep(50.00,nrow(Stations))

# create the dataframe for the stations file
foo <- data.frame(1:nrow(Stations),paste("p", 1:nrow(Stations), sep = ""), 
                  round(coordinates(longlatcoor)[,2], 2),
                  round(coordinates(longlatcoor)[,1], 2),
                  round(elev,2))
colnames(foo) <- cnames

write.csv(foo,"SWAT_weather/SL_stations.txt",row.names=F,quote=F)

# now write actual files
# rainfall
for (i in 1:ncol(Pdata_z)) {
  write(c("20000101",round(Pdata_z[,i],1)),
        file=paste("SWAT_weather/p",i,".txt",sep=""),
        ncolumns=1)
}

# wind
write_delim(as.data.frame(c("19830101",round(wind_z,2))),
            path="SWAT_weather/w1.txt", col_names=F)
# hum
write_delim(as.data.frame(c("19830101",round(hum_z,2))),
            path="SWAT_weather/h1.txt", col_names=F)

# slr
write_delim(as.data.frame(c("19830101",round(slr_z,2))),
            path="SWAT_weather/s1.txt", col_names=F)

# temperature
write("19830101",
      file="SWAT_weather/t1.txt",
      ncolumns=1)
write_delim(as.data.frame(cbind(round(maxT_z,2),round(minT_z,2))),
            path="SWAT_weather/t1.txt",
            col_names = F, append = T, delim=",")
