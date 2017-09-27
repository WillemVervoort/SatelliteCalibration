# Analysis of output.std similar to SWAT_check
library(tidyverse)
library(zoo)
#set working directory
setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/resources/uruguaySWAT")

# use the function "Extract_subbasin.R in the functions folder
source("uruguaycourse/functions/Extract_subbasin.R")

# read in output.std for subbasin 28 (Paso Pache outlet)
sb_out <- extract.sub("SantaLuciaSWAT/scenarios/default/txtinout",sb=28)
head(sb_out)
# This is a run starting in 2000, with NYSKIP = 1
Dates <- seq.Date(as.Date("2001-01-01"),
                  as.Date("2011-12-31"),
                  by=1)
sb_out$Dates <- Dates

sb_annual <- aggregate(sb_out[,1:9], list(Year = format(Dates,"%Y")),sum)
# this gives the annual water balance that can be analysed. 
head(sb_annual)
# In particular the columns PcP, ET and water yield are interesting
Waterbalance <- sb_annual$Pcp - sb_annual$ET - sb_annual$Water.yield

plot(sb_annual$Year, Waterbalance, xlab="Date",
     ylab = "Water balance", col="red", cex=1.3, lwd=3)
