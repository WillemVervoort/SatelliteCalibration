# comparison of INUMET stations in the Santa Lucia catchment
# - mean daily and annual rainfall, by season
# - var daily rainfall and annual, by season
# - number of raindays and length between rainfall events, calculate by season
# - double mass curve daily rainfall

setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/SatelliteCalibration")

# Some useful packages to install
library(tidyverse)
library(zoo)
library(rgeos)
library(sp)
library(rgdal)
library(lubridate)


# Read in the INUMET station locations 
Stations <- read.csv("data/rainfall stations-INUMET-subcuencasantalucia.csv")

# Read in the precipitation data for each INUMET stations
Pdata <- read.csv("data/Precipitacion SantaLucia_inumet_stations_prcp.csv", 
                  na.strings="NaN")



Dates2 <- as.Date(paste(Pdata[,1],
                        Pdata[,2],Pdata[,3],sep="-"))

# Find the INUMET stations that have 90% of data after 2001
Pdata_2000 <- Pdata[Dates2 >= "2000-01-01",]
Pdata_2000 <- Pdata_2000[-(1:5),]
result <- rep(0,(ncol(Pdata_2000)-3))
for (i in 4:ncol(Pdata_2000)) {
  result[i-3] <- sum(ifelse(is.na(Pdata_2000[,i]),1,0))/nrow(Pdata_2000)
}

# result indicates the fraction of NA data for the stations 
# Throw out all the columns and rows where result >0.1
Pdata_new <- Pdata_2000[,-(which(result>0.1)+3)]
Stations <- Stations[-which(result>0.1),]
Dates2000 <- Dates2[Dates2 >= as.Date("2000-01-01")]


Pdata_z <- zoo(Pdata_new[,4:ncol(Pdata_new)], order.by = Dates2000, frequency=1)
head(Pdata_z)

# read in the shapefile
SL <- readOGR("data/sl_shape/subcuencaSantaLuciahastariostaluciachico.shp")

map <- ggplot() + geom_polygon(data = SL, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
map

# station Df
Stat_df <- data.frame(Stations=colnames(Pdata_z), 
                  Long = Stations$POINT_X,
                  Lat = Stations$POINT_Y)
windows()
pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
                              coord_equal() +
  geom_point(data = Stat_df,aes(Long,Lat,colour=Stations), size=4) +
  geom_text(data = Stat_df, aes(Long,Lat,label=Stations, vjust=1))
pl

# --------------------
# Calculate basic statistics
# ----------------------

# log mean daily rainfall
mean_d_P <- apply(Pdata_z,2,function(x) exp(mean(log(x+1),na.rm=T)))
mean_df <- Stat_df
mean_df$daily <- mean_d_P

pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = mean_df,aes(Long,Lat,colour=daily), size=4) +
  geom_text(data = mean_df, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl

# CV daily rainfall
CV_d_P <- apply(Pdata_z,2,function(x) sd(x,na.rm=T)/mean(x,na.rm=T))
CV_df <- Stat_df
CV_df$daily <- CV_d_P

pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = CV_df,aes(Long,Lat,colour=daily), size=4) +
  geom_text(data = CV_df, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl


# mean annual rainfall
annual <- aggregate(Pdata_z, list(year=format(time(Pdata_z),"%Y")), 
                    sum, na.rm=T)
mean_a_P <- apply(annual,2,mean,na.rm=T)
mean_df$annual <- mean_a_P

pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = mean_df,aes(Long,Lat,colour=annual), size=4) +
  geom_text(data = mean_df, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl

# CV annual rainfall
CV_a_P <- apply(annual,2,function(x) sd(x)/mean(x))
CV_df$annual <- CV_a_P

pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = CV_df,aes(Long,Lat,colour=annual), size=4) +
  geom_text(data = CV_df, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl

# Seasonal analysis
seasons <- sapply(time(Pdata_z),
                 function(x) ifelse(month(x) > 11 || month(x) < 3 , "Summer", 
                                    ifelse(month(x) < 5 , "Autumn",
                                           ifelse(month(x) < 8, 
                                                  "Winter","Spring"))))
# based on daily data
Pseason <- data.frame(coredata(Pdata_z),seasons)
Pseason_pl <- gather(Pseason,key=Station, value=Rainfall,X2673:X2632)
Sp <- ggplot(Pseason_pl,aes(seasons,Rainfall)) +
  geom_boxplot(aes(colour=Station)) 
Sp
# lots of variability, can't see the boxplot

# summarise by season
season_P <- aggregate(coredata(Pdata_z), list(year=format(time(Pdata_z),"%Y"),season=seasons), 
                    sum, na.rm=T)
Pseason_pl <- gather(season_P,key=Station, value=Rainfall,X2673:X2632)
# boxplot
Sp <- ggplot(Pseason_pl,aes(season,Rainfall)) +
  geom_boxplot(aes(colour=Station)) 
Sp

# calculate mean by season
mean_s_P <- aggregate(season_P[,3:16],list(season=season_P$season),mean)
for (i in 1:nrow(mean_s_P)) {
   mean_df[,i+5] <- t(mean_s_P[i,2:15])
   colnames(mean_df)[i+5] <- mean_s_P$season[i]
}
# spatial plot
# Summer and Spring
pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = mean_df,aes(Long,Lat,colour=Summer,size=Spring)) +
  geom_text(data = mean_df, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl
# Autumn & Winter
pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = mean_df,aes(Long,Lat,colour=Autumn,size=Winter)) +
  geom_text(data = mean_df, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl

# double mass curves
# set all missing data to 0
Pdata_z_cor <- Pdata_z
Pdata_NA <- Pdata_z
Pdata_NA[] <- 0
for (i in 1:14) {
  Pdata_NA[is.na(Pdata_z_cor[,i]),i] <- 1
  Pdata_z_cor[is.na(Pdata_z_cor[,i]),i] <- 0
}

NAcount <- apply(Pdata_NA,2,sum)



# step 1 calculate cumulative mass curves
Cum_mass <- apply(Pdata_z_cor,2,cumsum)

plot_df <- gather(as.data.frame(Cum_mass),key = Station, value = CumulativeP, X2673:X2632)
plot_df$Dates <- rep(time(Pdata_z),14)
maxP <-Cum_mass[nrow(Cum_mass),1:14]
plot_df$maxP <- rep(maxP,each=nrow(Pdata_z))

windows()
pl <- ggplot(plot_df,aes(Dates,CumulativeP)) +
  geom_line(aes(colour=Station, linetype=Station)) +
  geom_text(aes(as.Date("2004-01-01"),maxP,
                label=Station, 
                hjust=rep(rep(c(-3,-2,-1,0,1,2,3),2),each=nrow(Pdata_z))))

pl


# frequency curves for rainfall > 0
FDC_gen <- function(DATA) {
  FDC <- data.frame(probs = seq(0,1,length=1000)*100,
                    rain = quantile(DATA[DATA>0],probs=seq(0,1,length=1000),
                                    na.rm=T))
  return(FDC)
}

Fcurves <- apply(Pdata_z, 2, FDC_gen)
# this is a list with a data.frame for each station
# str(Fcurves)
F_df <- do.call(cbind,Fcurves)
F_df <- F_df[,-seq(3,27,by=2)]

F_df_pl <- gather(F_df,value=Rainfall, key=Station,X2673.rain:X2632.rain)
colnames(F_df_pl)[1] <- "Prob"
 F_df_pl$Station <- sapply(F_df_pl$Station,
                           function(x) gsub(".rain","",x))
f_plot <- ggplot(F_df_pl,aes(Prob,Rainfall)) +
  geom_line(aes(colour=Station)) +
  ggtitle("Daily Rainfall Volume Probability plot for rainfall > 0")#+
  #scale_y_continuous(trans='log')
f_plot