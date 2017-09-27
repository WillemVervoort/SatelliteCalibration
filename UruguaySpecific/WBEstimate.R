# Basic Water balance check

# for this we want to read precipation and weather 
# and we want to read in the water outflow at the outlet
library(tidyverse)
library(zoo)
#set working directory
setwd("../resources/uruguaySWAT/uruguaycourse/data")

# Example for Santa Lucia
# Read in the flow data in cumecs
Flowdata <- readRDS("SantaLucia.RDS")
Flow <- zoo(Flowdata$flow/(5171*10^6)*86400*1000,
            order.by = as.Date(Flowdata$Date))

# read in the weather data:
WD <- read.csv("weather/INIALasBrujas_1983-2016.csv")

# There are two different dates: 31-12-2016 and 1/1/1983
# Need to convert to standard dates
Dates <- ifelse(grepl("-",WD$Fecha)==TRUE,
                as.character(as.Date(WD$Fecha,"%d-%m-%Y")),
                as.character(as.Date(WD$Fecha,"%d/%m/%Y")))

# Add additional rainfall data
# read in the precipitation data
Pdata <- read.csv("weather/Precipitacion_SantaLucia.csv", 
                  na.strings="NaN")
head(Pdata)     

Dates2 <- as.Date(paste(Pdata[,1],Pdata[,2],
                        Pdata[,3],sep="-"))

# find the stations that have 90% of data after 1995
Pdata_1995 <- Pdata[Dates2 >= "1995-01-01",]
result <- rep(0,(ncol(Pdata_1995)-3))
for (i in 4:ncol(Pdata_1995)) {
  result[i-3] <- sum(ifelse(is.na(Pdata_1995[,i]),1,0))/nrow(Pdata_1995)
}

# result indicates the fraction of NA data for the stations 
# throw out all the columns and rows where result >0.1
Pdata_new <- Pdata_1995[,-(which(result>0.1)+3)]
Stations <- Stations[-which(result>0.1),]
Dates1995 <- Dates2[Dates2 >= "1995-01-01"]

# zoo data
Pdata_z <- zoo(Pdata_new[,4:ncol(Pdata_new)], 
               order.by = Dates1995, frequency=1)
names(Pdata_z)
Rainfall_avg <- apply(Pdata_z,1,mean,na.rm=T)
Rainfall_z <- zoo(Rainfall_avg, order.by = Dates1995, frequency=1)

# wind data needs to be in m/s
wind_z <- zoo(WD[,5]*1000/86400, order.by =Dates)

# humidity
hum_z <- zoo(WD[,6], order.by =Dates)

# maxT
maxT_z <- zoo(WD[,3], order.by =Dates)

# minT
minT_z <- zoo(WD[,4], order.by =Dates)

# solar radiation needs to be in MJ/m2
# 1 cal cm-2 day-1 = 4.1868 10-2 MJ m-2 day-1
# http://www.fao.org/docrep/X0490E/x0490e0i.htm
slr_z <- zoo(WD[,2]*0.041868, order.by =Dates)


# Create a data.frame to feed into ET.fun for potET
# only one relative humidity, no max and min
ETinput <- merge(maxT_z, minT_z, hum_z, hum_z, slr_z, wind_z)
source("C:/Users/rver4657/ownCloud/SouthSouthNetwork/SatelliteCalibration/UruguaySpecific/ETfun.R")

# Calculate all the ET data
ETout <- ET.fun(ETinput,elev=30, lat=34)
names(ETout)
ETout_z <- zoo(ETout[,7:9], order.by=as.Date(time(ETout)))

# -----------------------------------------
# WATER BALANCE
# ---------------------------
# first combine data into one dataframe (start with Penman M ET)
WBdata <- merge(Flow, ETout_z$PM.Ep,Rainfall_z, all=F)
names(WBdata) <- c("Flow", "PMEp", "Rain")

WBdata_annual <- aggregate(WBdata,
                           list(year=format(time(WBdata),"%Y")),
                           sum,na.rm=T)
head(WBdata_annual)

plot(time(WBdata_annual),
     with(WBdata_annual,Rain - PMEp - Flow), type="b",
     xlab = "Date", ylab = "Water balance difference")
# seems negative many years, so possibly underestimating the rainfall data 