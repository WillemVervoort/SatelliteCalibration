# comparison of CHIRPS stations in the Santa Lucia subcatchment
# - mean daily and annual rainfall, by season
# - var daily rainfall and annual, by season
# - number of raindays and length between rainfall events, calculate by season
# - double mass curve daily rainfall

setwd("C:/Users/User/Documents/Uruguay/IRI-Inia/SWAT-GIS/Clima CHIRPS SubSantaLucia")

# Some useful packages to install
library(tidyverse)
library(zoo)
library(rgeos)
library(sp)
library(rgdal)
library(lubridate)

chirpsSL<- readRDS("output.rds", refhook = NULL)

# read in the shapefile
SL <- readOGR("sl_shape/subcuencaSantaLuciahastariostaluciachico.shp")

map <- ggplot() + geom_polygon(data = SL, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
map


# convert prcp from list into a dataframe
# first extract the prcp elements into a list

SLprcp <- chirpsSL %>% map("prcp")

is.list(SLprcp)

SLprcp_df <- as.data.frame(do.call(cbind,SLprcp))
head(SLprcp_df)

Dates <- seq.Date(as.Date("2000-01-01"),as.Date("2017-09-30"),by=1)
Dates_df<-data.frame(Dates)
Dates_df_<-separate(Dates_df, "Dates", c("Year","Month","Day"), sep="-")
SLprcp_dates<- cbind(Dates_df_,SLprcp_df)
#erase the "V" caracter of the column name
names(SLprcp_dates) <- gsub("V", "", names(SLprcp_dates), fixed = TRUE)
head(SLprcp_dates)

# second extract the lat and long elements into a list

SLlon<- chirpsSL%>% map("x")
SLlon_df <- as.data.frame(do.call(c,SLlon))
colnames(SLlon_df)[1] <- "lon"
head(SLlon_df)

SLlat<- chirpsSL %>% map("y")
SLlat_df <- as.data.frame(do.call(c,SLlat))
colnames(SLlat_df)[1] <- "lat"
str(SLlat_df)

#add dataframe with number of chirps stations
chirps_stations <- c(colnames(SLprcp_df))
chirps_stations<- c(1:nrow(SLlat_df))
#chirps_stations<-data.frame(chirps_stations) # using this line the spatial plot is not nice

# Dataframe with stations number, lat, long

#Dates <- seq.Date(as.Date("2000-01-01"),as.Date("2017-09-30"),by=1)

chirpslatlong_df <- cbind(chirps_stations,SLlon_df, SLlat_df)
head(chirpslatlong_df)

# read in the shapefile
SL <- readOGR("sl_shape/subcuencaSantaLuciahastariostaluciachico.shp")

map <- ggplot() + geom_polygon(data = SL, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
map

# station chirps Df - dataframe chirps station with lat, lont for spatial plot
Statchirps_df <- data.frame(Stations=chirps_stations, 
                      Long = chirpslatlong_df$lon,
                      Lat = chirpslatlong_df$lat)

pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = Statchirps_df,aes(Long,Lat,colour=chirps_stations), size=4) +
  geom_text(data = Statchirps_df, aes(Long,Lat,label=chirps_stations, vjust=1))
pl



# --------------------
# Calculate basic statistics
# ----------------------

# log mean daily rainfall

SLprcp_dates_new <- zoo(SLprcp_dates[,4:ncol(SLprcp_dates)], order.by = Dates, frequency=1)

mean_d_P_chirps <- apply(SLprcp_dates_new,2,function(x) exp(mean(log(x+1),na.rm=T)))
meanchirps_df <- Statchirps_df
meanchirps_df$daily <- mean_d_P_chirps

pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = meanchirps_df,aes(Long,Lat,colour=daily), size=2) +
  geom_text(data = meanchirps_df, aes(Long,Lat,label=Stations, vjust=1),size=2) +
  scale_colour_gradientn(colors=rainbow(10))
pl

# CV daily rainfall
CV_d_P_chirps <- apply(SLprcp_dates_new,2,function(x) sd(x,na.rm=T)/mean(x,na.rm=T))
CV_df_chirps <- Statchirps_df
CV_df_chirps$daily <- CV_d_P_chirps

pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = CV_df_chirps,aes(Long,Lat,colour=daily), size=2) +
  geom_text(data = CV_df_chirps, aes(Long,Lat,label=Stations, vjust=1),size=2) +
  scale_colour_gradientn(colors=rainbow(10))
pl

# mean annual rainfall

SLprcp_dates_new <- zoo(SLprcp_dates[,4:ncol(SLprcp_dates)], order.by = Dates, frequency=1)
annualchirps <- aggregate(SLprcp_dates_new, list(year=format(time(SLprcp_dates_new),"%Y")), 
                    sum, na.rm=T)

mean_a_P_chirps <- apply(annualchirps,2,mean,na.rm=T)
meanchirps_df <- Statchirps_df
meanchirps_df$annual <- mean_a_P_chirps

## spatial plot of the mean annual rainfall of CHIRPS stations 
pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = meanchirps_df,aes(Long,Lat,colour=annual), size=4) +
  geom_text(data = meanchirps_df, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl

# CV annual rainfall
CV_a_P_chirps <- apply(annualchirps,2,function(x) sd(x)/mean(x))
CV_df_chirps <- Statchirps_df
CV_df_chirps$annual <- CV_a_P_chirps

## spatial plot of CV rainfall of CHIRPS stations 
pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = CV_df_chirps,aes(Long,Lat,colour=annual), size=4) +
  geom_text(data = CV_df_chirps, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl

# Seasonal analysis
##season function
seasonschirps <- sapply(time(SLprcp_dates_new),
                 function(x) ifelse(month(x) > 11 || month(x) < 3 , "Summer", 
                                    ifelse(month(x) < 5 , "Autumn",
                                           ifelse(month(x) < 8, 
                                                  "Winter","Spring"))))

# boxplot not done as to many chirps station to have clear results
# based on daily data
#Pseasonchirps <- data.frame(coredata(SLprcp_dates_new),seasonschirps)
#Pseason_plchirps <- gather(Pseasonchirps,key=Station, value=Rainfall,X2673:X2632)
# too many station and variability 

# summarise by season
season_P_chirps <- aggregate(coredata(SLprcp_dates_new), list(year=format(time(SLprcp_dates_new),"%Y"),season=seasonschirps), 
                    sum, na.rm=T)
head(season_P_chirps)
#Pseason_pl_chirps <- gather(season_P_chirps,key=Station, value=Rainfall,X2673:X2632)
# boxplot
#Sp <- ggplot(Pseason_pl,aes(season,Rainfall)) +
  geom_boxplot(aes(colour=Station)) 
#Sp

# calculate mean by season
mean_s_P_chirps <- aggregate(season_P_chirps[,3:340],list(season=season_P_chirps$season),mean)
for (i in 1:nrow(mean_s_P_chirps)) {
  meanchirps_df[,i+4] <- t(mean_s_P_chirps[i,2:339]) #error
   colnames(meanchirps_df)[i+4] <- mean_s_P_chirps$season[i]
}
# spatial plot
# Summer and Spring
pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = meanchirps_df,aes(Long,Lat,colour=Summer,size=Spring)) +
  geom_text(data = meanchirps_df, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl
# Autumn & Winter
pl <- ggplot() + geom_polygon(data = SL, 
                              aes(x = long, y = lat, group = group), 
                              colour = "black", fill = NA) +
  coord_equal() +
  geom_point(data = meanchirps_df,aes(Long,Lat,colour=Autumn,size=Winter)) +
  geom_text(data = meanchirps_df, aes(Long,Lat,label=Stations, vjust=1)) +
  scale_colour_gradientn(colors=rainbow(10))
pl


# double mass curves - plot are not really easy to examine as a lot of CHIRPS stations
# set all missing data to 0
SLprcp_dates_cor <- SLprcp_dates_new
SLprcp_dates_NA <- SLprcp_dates_new
SLprcp_dates_NA[] <- 0
for (i in 1:338) {
  SLprcp_dates_NA[is.na(SLprcp_dates_cor[,i]),i] <- 1
  SLprcp_dates_cor[is.na(SLprcp_dates_cor[,i]),i] <- 0
}

NAcountchirps <- apply(SLprcp_dates_NA,2,sum)


# step 1 calculate cumulative mass curves
Cum_mass_chirps <- apply(SLprcp_dates_cor,2,cumsum)

plot_df_chirps <- gather(as.data.frame(Cum_mass_chirps),key = Station, value = CumulativeP, 1:338)
plot_df_chirps$Dates <- rep(time(SLprcp_dates_new),338)
maxP <-Cum_mass_chirps[nrow(Cum_mass_chirps),1:338]
plot_df_chirps$maxP <- rep(maxP,each=nrow(SLprcp_dates_new))

#windows()
pl <- ggplot(plot_df_chirps,aes(Dates,CumulativeP)) +
  geom_line(aes(colour=Station, linetype=Station)) +
  theme(legend.position = "none")
#+
 # geom_text(aes(as.Date("2004-01-01"),maxP,
                label=Station, 
                hjust=rep(rep(c(-3,-2,-1,0,1,2,3),2),each=nrow(SLprcp_dates_new))))

pl


# frequency curves for rainfall > 0
# not given the right plot 
FDC_gen <- function(DATA) {
  FDC <- data.frame(probs = seq(0,1,length=1000)*100,
                    rain = quantile(DATA[DATA>0],probs=seq(0,1,length=1000),
                                    na.rm=T))
  return(FDC)
}

Fcurves <- apply(SLprcp_dates_new, 2, FDC_gen)
# this is a list with a data.frame for each station
# str(Fcurves)
F_df_chirps <- do.call(cbind,Fcurves)
F_df_chirps <- F_df_chirps[,-seq(3,27,by=2)]

F_df_pl_chirps <- gather(F_df_chirps,value=Rainfall, key=Station,1:338)
colnames(F_df_pl_chirps)[1] <- "Prob"
F_df_pl_chirps$Station <- sapply(F_df_pl_chirps$Station,
                           function(x) gsub(".rain","",x))
f_plot_chirps <- ggplot(F_df_pl_chirps,aes(Prob,Rainfall)) +
  geom_line(aes(colour=Station)) +
  ggtitle("Daily Rainfall Volume Probability plot for rainfall > 0")+
  theme(legend.position = "none")

f_plot_chirps
# there is one data which have another pattern from the others # why??? 
