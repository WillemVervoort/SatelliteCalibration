# Convert original Las Brujas data to SWAT weather data
library(tidyverse)
library(zoo)

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

names(WD)

# zoo the data and replace missing data with -99.0
# rainfall
rain_z <- zoo(WD[,7], order.by = Dates)
rain_z[is.na(rain_z)] <- -99.0

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

# rainfall
write_delim(as.data.frame(c("19830101",round(rain_z,2))),
      path="p1.txt", col_names=F)

# wind
write_delim(as.data.frame(c("19830101",round(wind_z,2))),
            path="w1.txt", col_names=F)
# hum
write_delim(as.data.frame(c("19830101",round(hum_z,2))),
            path="h1.txt", col_names=F)

# slr
write_delim(as.data.frame(c("19830101",round(slr_z,2))),
            path="s1.txt", col_names=F)

# temperature
write("19830101",
      file="t1.txt",
      ncolumns=1)
write_delim(as.data.frame(cbind(round(maxT_z,2),round(minT_z,2))),
      path="t1.txt",
      col_names = F, append = T)

