
#install.packages("tidyverse")
require("tidyverse")
require("zoo")

#setwd("C:/Users/User/Documents/Uruguay/IRI-Inia/SWAT-GIS/Clima CHIRPS SubSantaLucia")
root <- "c:/users/rver4657/owncloud/southsouthnetwork/resources/UruguaySWAT/UruguayCourse/data"
setwd(paste(root,"weather",sep="/"))


#reading daily precipitation data for 1 specific point from CHRIPS and INUMET

chirps_inumet_pt2 <- read.csv("chirps_inumet_dailyPrec_point2_2000-2011.csv")
head(chirps_inumet_pt2)


#correlation prcp inumet chirps all data together ==> not the same as calculated in excel
cor(chirps_inumet_pt2[,c(5,6)], use = "pairwise.complete.obs")
cor(chirps_inumet_pt2[,c(5,6)], use="pairwise.complete.obs", method="spearman")

#plot chirps vs inumet - all year together 2000-2011
plot(chirps_inumet_pt2$prcp_chirps,chirps_inumet_pt2$prcp_inumet, xlab="inumet prcp (mm)", ylab = "chirps prcp(mm)")
reslm20002011 <- lm(chirps_inumet_pt2$prcp_inumet~chirps_inumet_pt2$prcp_chirps)
abline(reslm20002011,col=2)

require(zoo)
# Create a zoo data frame for easier plotting in time
chirps_inumet_pt2_z <- zoo(chirps_inumet_pt2[,5:6],
                           order.by = as.Date(chirps_inumet_pt2$Date,"%d-%b-%y"))

plot(chirps_inumet_pt2_z, xlab="Date")

old.par <- par # put plot together
# I think this is what you wanted
# 2000 = subset for a year inumet and chirps and correlate inumet & chirps
prcp2000 <- chirps_inumet_pt2[chirps_inumet_pt2$year==2000,]
reslm <- lm(prcp2000$prcp_inumet~prcp2000$prcp_chirps)
# inserted this line
par(mfrow=c(2,1))
plot(prcp2000$prcp_chirps,prcp2000$prcp_inume,xlab="inumet prcp (mm)", ylab = "chirps prcp(mm)", xlim=c(0,100),ylim = c(0,100))
abline(reslm,col=2)
title(sprintf("year 2000 - R^2 = %.3f",summary(reslm)$r.squared))
cor(prcp2000$prcp_chirps,prcp2000$prcp_inumet, use ="complete.obs") 

# 2001 
prcp2001 <- chirps_inumet_pt2[chirps_inumet_pt2$year==2001,]
reslm1 <- lm(prcp2001$prcp_inumet~prcp2001$prcp_chirps)
plot(prcp2001$prcp_chirps,prcp2001$prcp_inume,xlab="inumet prcp (mm)", ylab = "chirps prcp(mm)", xlim=c(0,100),ylim = c(0,100))
abline(reslm,col=2)
title(sprintf("year 2001 - R^2 = %.3f",summary(reslm1)$r.squared))
cor(prcp2001$prcp_chirps,prcp2001$prcp_inumet, use ="complete.obs") 

par <- old.par # end of the plot together

#annual prcp chirps - inumet 
(annual_prcp<- aggregate(chirps_inumet_pt2_z,list(Year=chirps_inumet_pt2$year),sum))
# colnames(annual_prcp_chirps)[2]<- "prcp_chirps"
# (annual_prcp_inumet <- aggregate(chirps_inumet_pt2$prcp_inumet,list(Year=chirps_inumet_pt2$year),sum))
# colnames(annual_prcp_inumet)[2]<- "prcp_inumet"
# 
# annual_prcp_chirps_inumet<- merge(annual_prcp_chirps, annual_prcp_inumet, by ="Year")
cor(annual_prcp$prcp_chirps,annual_prcp$prcp_inumet, use ="complete.obs") 

plot(annual_prcp$prcp_chirps, annual_prcp$prcp_inumet)
reslmannual <- lm(annual_prcp$prcp_inumet~annual_prcp$prcp_chirps)
abline(reslmannual,col=2)
title(sprintf("annual prcp- R^2 = %.3f",summary(reslmannual)$r.squared))


# # Merge prcp by month and for same year - chirps & inumet
# 
# #head(chirps_inumet_pt2)
# #mensual_prcp_chirps <- aggregate(chirps_inumet_pt2$prcp_chirps,list(month=chirps_inumet_pt2$mes),sum)
# #head(mensual_prcp_chirps)
# #other way but not separating the year
# #year<- chirps_inumet_pt2$year
# #for(year in 2000:2011){
#  # mensual_prcp_chirps <- aggregate(chirps_inumet_pt2$prcp_chirps,list(month=chirps_inumet_pt2$mes),sum)
# #}
# setwd("C:/Users/User/Documents/Uruguay/IRI-Inia/SWAT-GIS/Clima CHIRPS SubSantaLucia")
# install.packages('data.table')
# library(data.table)
# require(data.table)
# chirps_inumet_pt2 <- read.csv("chirps_inumet_dailyPrec_point2_2000-2011.csv")
# 
# mensual_prcp_chirps<-setkey(setDT(chirps_inumet_pt2),year,mes)[, 
#                                      list(prcp_chirps=sum(prcp_chirps)), by=list(year, mes)]
# head(mensual_prcp_chirps)
# 
# 
# mensual_prcp_inumet<-setkey(setDT(chirps_inumet_pt2),year,mes)[, 
#                                                                list(prcp_inumet=sum(prcp_inumet)), by=list(year, mes)]
# head(mensual_prcp_inumet)
# 
# mensual_prcp_chirps_inumet<- cbind(mensual_prcp_chirps, prcp_inumet=mensual_prcp_inumet)
# 
# head(mensual_prcp_chirps_inumet)
# test<-mensual_prcp_chirps_inumet[-1,]
# head(test)
# mensual_prcp_chirps_inumet<-test[-1,]
# head(mensual_prcp_chirps_inumet)
# mensual_prcp_chirps_inumet <- mensual_prcp_chirps_inumet[,-4][,-4]
# colnames (mensual_prcp_chirps_inumet)[4]<- "prcp_inumet"
# head(mensual_prcp_chirps_inumet)

# use zoo data frame
mensual_prcp <- aggregate(chirps_inumet_pt2_z, as.yearmon, sum)

#plot correlation between year and month
reslm <- lm(mensual_prcp$prcp_inumet~mensual_prcp$prcp_chirps)
plot(mensual_prcp$prcp_chirps,mensual_prcp$prcp_inume,xlab="inumet prcp (mm)", ylab = "chirps prcp(mm)", xlim=c(0,350),ylim = c(0,350))
abline(reslm,col=2)
title(sprintf("mensual - R^2 = %.3f",summary(reslm)$r.squared))
cor(as.numeric(mensual_prcp$prcp_chirps),as.numeric(mensual_prcp$prcp_inumet), use ="complete.obs") 

#normal probability plot
# par(mfrow=c(1,2))
# qqnorm(mensual_prcp_chirps_inumet$prcp_chirps)
# qqline(mensual_prcp_chirps_inumet$prcp_chirps,col='red')
# 
# qqnorm(mensual_prcp_chirps_inumet$prcp_inumet)
# qqline(mensual_prcp_chirps_inumet$prcp_inumet,col='red')

# qq plot is easier if you just plot the distributions against each other
#qqplot
FDC_gen <- function(DATA) {
  FDC <- data.frame(probs = seq(0,1,length=1000)*100,
                    rainfall = quantile(DATA,probs=seq(0,1,length=1000),
                                    na.rm=T))
  return(FDC)
}

# qq probability plot
plot(FDC_gen(mensual_prcp$prcp_chirps)$rainfall,
     FDC_gen(mensual_prcp$prcp_inumet)$rainfall)
lines(c(0,100),c(0,100), lty=2)
# also works for the daily data
plot(FDC_gen(chirps_inumet_pt2$prcp_chirps)$rainfall,
     FDC_gen(chirps_inumet_pt2$prcp_inumet)$rainfall)
lines(c(0,100),c(0,100), lty=2)
# shows that there was some level of distribution matching of the data
# at least at monthly scale
write.csv(mensual_prcp,"mensual_prcp_chirps_inumet.csv")

# Merge prcp by 10-day for the same month and for same year - chirps & inumet
# setwd("C:/Users/User/Documents/Uruguay/IRI-Inia/SWAT-GIS/Clima CHIRPS SubSantaLucia")
# install.packages('data.table')
# library(data.table)
# require(data.table)
# chirps_inumet_pt2 <- read.csv("chirps_inumet_dailyPrec_point2_2000-2011.csv")
# head(chirps_inumet_pt2)


# You need to define the series of days (the pentads)
# So we want to write a function that generates the pentads for each year
# then we want to aggregate on the pentads
require(lubridate)
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

test <- long_pentad("2000-01-01","2005-12-31")
head(test,20)
# replication of pentads function
rep_fun <- function(x) {
  out <- sort(rep(x,times=c(diff(x),1)))
  return(out)
}
rep_try <- rep_fun(test)
head(rep_try,20)

# Now apply to the inumet data
chirps_pentad <- rep_fun(long_pentad(time(chirps_inumet_pt2_z)[1],
                    time(chirps_inumet_pt2_z)[nrow(chirps_inumet_pt2_z)]))

# now aggregate the data
# there is a little problem, which I haven't yet figured out
# for some reason there are 4 too many pentads, not sure how
chirps_rain_pentad <- aggregate(chirps_inumet_pt2[,5:6],
                list(pentad=chirps_pentad),
                sum)

plot(chirps_rain_pentad[,2],chirps_rain_pentad[,3])