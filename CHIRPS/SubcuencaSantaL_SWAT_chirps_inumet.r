# Convert original Las Brujas data to SWAT weather data
# version 2, add new rainfall data
library(tidyverse)
library(zoo)
library(rgeos)
library(sp)
library(rgdal)


#set working directory
#setwd("R:/PRJ-HPWC/SWAT_ETCalibration/SantaLuciaData")

setwd("C:/Users/User/Documents/Uruguay/UruguayCourse")
# read in the weather data:
#WD <- read.csv("INIALasBrujas_1983-2016.csv")

# There are two different dates: 31-12-2016 and 1/1/1983
# Need to convert to standard dates
#Dates <- ifelse(grepl("-",WD$Fecha)==TRUE,
                as.character(as.Date(WD$Fecha,"%d-%m-%Y")),
                as.character(as.Date(WD$Fecha,"%d/%m/%Y")))

# Check for missing data
#sum(ifelse(is.na(WD[,2:ncol(WD)]),1,0))

# Add additional rainfall data
# read in the station locations 
Stations <- read.csv("Precipitacion_SantaLucia_inumet_coordinates.csv")
# read in the precipitation data
Pdata <- read.csv("Precipitacion SantaLucia_inumet_stations_prcp.csv", 
                  na.strings="NaN",header=T,nrows=11322)
head(Pdata)     
tail(Pdata)
Dates2 <- as.Date(paste(Pdata[,1],
                        Pdata[,2],Pdata[,3],sep="-"))

# find the stations that have 90% of data after 2001
Pdata_2000 <- Pdata[Dates2 >= "2000-01-01",]
Pdata_2000 <- Pdata_2000[-(1:5),]
result <- rep(0,(ncol(Pdata_2000)-3))
for (i in 4:ncol(Pdata_2000)) {
  result[i-3] <- sum(ifelse(is.na(Pdata_2000[,i]),1,0))/nrow(Pdata_2000)
}

# result indicates the fraction of NA data for the stations 
# throw out all the columns and rows where result >0.1
Pdata_new <- Pdata_2000[,-(which(result>0.1)+3)]
Stations <- Stations[-which(result>0.1),]
Dates2000 <- Dates2[Dates2 >= as.Date("2000-01-01")]

# zoo the data and replace missing data with -99.0
# Zoo the Pdata (rainfall)
Pdata_z <- zoo(Pdata_new[,4:ncol(Pdata_new)], order.by = Dates2000, frequency=1)
#Pdata_z[is.na(Pdata_z)] <- -99.0
head(Pdata_z)


# convert UTM to latlong

require(rgdal)
Stat_UTM <- SpatialPoints(cbind(Stations$X_UTM,Stations$Y_UTM), 
                          proj4string=CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"))
longlatcoor<-spTransform(Stat_UTM,CRS("+proj=longlat +datum=WGS84 +no_defs"))
elev <- rep(50.00,nrow(Stations))


# create the dataframe for the stations file
foo <- data.frame(colnames(Pdata_z), 
                  round(coordinates(longlatcoor)[,2], 2),
                  round(coordinates(longlatcoor)[,1], 2),
                  round(elev,2))
colnames(foo) <- c("Stations", "Lat","Long","Elev")
head(foo)


head(Pdata_z)

outlist <- list()

for(i in 1:ncol(Pdata_z)) {
  outlist[[i]] <- list(x = foo$Long[i],
                       y = foo$Lat[i],
                       prcp = Pdata_z[,i])
  
  }

names(outlist) <- colnames(Pdata_z)
str(outlist[[1]])

#test
plot(outlist[[1]]$prcp)



# Load the list of chirps data 
# see R script: ReadNetcdfFile.r for the steps to get to output.rds

setwd("C:/Users/User/Documents/Uruguay/IRI-Inia/SWAT-GIS/Clima CHIRPS SubSantaLucia")

chirpsSL<- readRDS("chirps_data_subcuencaSL/output.rds", refhook = NULL)

# insert the dates for the CHIRPS data and make zoo

C_dates <- seq.Date(as.Date("2000-01-01"), as.Date("2017-09-30"), by=1)

# You need to define the series of days (the pentads)
# So we want to write a function that generates the pentads for each year
# then we want to aggregate on the pentads
require(lubridate)
require(epiR)

# bunch of functions
pentad_fun <- function(Date) {
  d <- seq.Date(as.Date(Date), as.Date(Date) + days_in_month(as.Date(Date)),by=5)
  d1 <- d[1:(length(d)-1)] # need to not have the last day
}

long_pentad <- function(startDate, endDate) {
  months <- seq.Date(as.Date(startDate), as.Date(endDate), by="month")
  pentads <- sapply(months,pentad_fun)
  out <- c(do.call(c,pentads),as.Date(endDate))
  return(out)
}

# test <- long_pentad("2000-01-01","2005-12-31")
# head(test,20)
# replication of pentads function
rep_fun <- function(x) {
  out <- sort(rep(x,times=c(diff(x),1)))
  return(out)
}
# rep_try <- rep_fun(test)
# head(rep_try,20)


# loop to calculate correlation between each inumet point and each chirps point
#first neet to create the output table 
cor1<- matrix(0,nrow=length(chirpsSL),ncol=nrow(foo))

result <- data.frame(Long= numeric(length = length(chirpsSL)),Lat= numeric(length = length(chirpsSL)) , cor1) 

#fill the table output with loop

for(j in 1:nrow(foo)) {
  for (i in 1:length(chirpsSL)){
    c_precip <- zoo(chirpsSL[[i]]$prcp, order.by = C_dates) 
    

    test_merge <- merge.zoo(c_precip,outlist[[j]]$prcp,all=F)

    
    # Now apply to the inumet data
    chirps_pentad <- rep_fun(long_pentad(time(test_merge)[1],
                                         time(test_merge)[nrow(test_merge)]))


    # now aggregate the data # problem because argument not have same lenght
    chirps_rain_pentad <- aggregate(coredata(test_merge),
                                    list(pentad=chirps_pentad),
                                    sum)
    print(i)
    print(j)

    # correlation 
    result[i,j+2] <- cor(chirps_rain_pentad[,2],chirps_rain_pentad[,3],use="complete.obs")
    result$Long[i] <- chirpsSL[[i]]$x
    result$Lat[i] <- chirpsSL[[i]]$y
  }
}
  
#ggplot
par(mfrow=c(2,2))
ggplot(result, aes(x=Long, y=Lat))+ geom_point(aes(colour=X1)) + scale_color_gradientn(colours=rainbow(5))
ggplot(result, aes(x=Long, y=Lat))+ geom_point(aes(colour=X2))+ scale_color_gradientn(colours=rainbow(5))




# outlist is inumet stations outer loop
# chirpsSL is chirps stations is inner loop

