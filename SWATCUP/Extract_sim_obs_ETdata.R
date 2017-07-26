# Script to check the performance of the calibration on ET
# plot the simulated against the observed ET for each subbasin
# SWAT-CUP only plots the flow comparison
# Calibration 1: combined objective function Flow = 0.5, each ET is 0.02

# packages
require(zoo)
require(tidyverse)

# setwd()
setwd("R:/PRJ-HPWC/SWAT_ETCalibration/Cotter2017.PSO.SwatCup")

# Option 1, if there are no ET in the objective function
# In this case: move output.sub to the iteration folder
# load the extract_subbasin function
source("Iterations/Extract_subbasin.R")

# read in the observed ET data
ET_obs <- readRDS("../Uruguaycourse/data/ETData.RDS")

# # read in the output.sub file for sub basin 1
# ET_sub1 <- extract.sub(getwd(),sb=1)
# 
# ET_sub1_z <- zoo(ET_sub1$ET, order.by=seq.Date(as.Date("2006-01-01"),
#                                                as.Date("2011-12-31"),1))
# 
# # subset a point, zoo data and select window
# ET_obs1 <- ET_obs[ET_obs["Point"]==1,]
# ET_obs1_z <- zoo(ET_obs1$ET,order.by=ET_obs1$Date)
# ET_obs1_c <- window(ET_obs1_z,start=as.Date("2006-01-01"),end=as.Date("2011-12-31"))
# 
# 
# # make some plots
# plot(ET_sub1_z,ET_obs1_c)
# plot(ET_sub1_z, col="red")
# points(ET_obs1_c,col="blue")

summary_ETstats <- data.frame(sub = 1:25, KGE = rep(0,25),
                               NSE = rep(0,25))
require(hydroGOF)


for (i in 1:25 ){
  ET_sub1 <- extract.sub(paste(getwd(),"iterations/flowcalibration0912",sep="/"),
                         sb=i)

  ET_sub1_z <- zoo(ET_sub1$ET, order.by=seq.Date(as.Date("2006-01-01"),
                                                 as.Date("2011-12-31"),1))

  # subset a point, zoo data and select window
  ET_obs1 <- ET_obs[ET_obs["Point"]==i,]
  ET_obs1_z <- zoo(ET_obs1$ET,order.by=ET_obs1$Date)
  ET_obs1_c <- window(ET_obs1_z,start=as.Date("2006-01-01"),end=as.Date("2011-12-31"))


  # make some plots
  plot(ET_sub1_z, col="red", main = paste("subbasin",i))
  points(ET_obs1_c/8,col="blue")
  
  ET_all <- merge(ET_sub1_z,ET_obs1_c/8,all=F)
  
  summary_ETstats$KGE[i] <- KGE(ET_all[,1],ET_all[,2])  
  summary_ETstats$NSE[i] <- NSE(ET_all[,1],ET_all[,2])
}

write.csv(summary_ETstats,"Iterations/flowcalibration0912/summary_ETstats.csv",
          row.names=F)


# alternatively:
# sequence of dates
Dates <- c(seq.Date(as.Date("2006-01-01"),
                    as.Date("2006-12-31"),8),
           seq.Date(as.Date("2007-01-01"),
                    as.Date("2007-12-31"),8),
           seq.Date(as.Date("2008-01-01"),
                    as.Date("2008-12-31"),8),
           seq.Date(as.Date("2009-01-01"),
                    as.Date("2009-12-31"),8),
           seq.Date(as.Date("2010-01-01"),
                    as.Date("2010-12-31"),8),
           seq.Date(as.Date("2011-01-01"),
                    as.Date("2011-12-31"),8))


# read in the "best_sim.txt" file from PSO.out
foo <- file("iterations/FirstETCalibration/pso.out/best_sim.txt", "r+")
test <- readLines(foo)


for (i in 1:25) {
  lineno <- grep(paste("ET_",i, sep=""),test)[1]
  ETdata <- fread("iterations/FirstETCalibration/pso.out/best_sim.txt", 
                data.table=T, skip = lineno, nrows=276, header=T)
  
  
  ETdata_z <- zoo(ETdata, order.by=Dates)
  # make some plots
  plot(ETdata_z$simulated, col="red", main = paste("subbasin",i))
  points(ETdata_z$observed,col="blue")
}

# Eight ET calibration (shift in weight  )
foo <- file("iterations/8thETCalibration/pso.out/best_sim.txt", "r+")
test <- readLines(foo)


for (i in 1:25) {
  lineno <- grep(paste("ET_",i, sep=""),test)[1]
  ETdata <- fread("iterations/8thETCalibration/pso.out/best_sim.txt", 
                  data.table=T, skip = lineno, nrows=276, header=T)
  
  
  ETdata_z <- zoo(ETdata, order.by=Dates)
  # make some plots
  plot(ETdata_z$simulated, col="red", main = paste("subbasin",i))
  points(ETdata_z$observed,col="blue")
}
