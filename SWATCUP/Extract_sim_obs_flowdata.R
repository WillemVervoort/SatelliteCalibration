# Plot flow calibration SWAT-CUP
require(zoo)
require(tidyverse)

setwd("R:/PRJ-HPWC/SWAT_ETCalibration/Cotter2017.PSO.SwatCup")

flow_pred <- read.table("iterations/flowcalibration0912/pso.out/95ppu.txt",skip=1,
                        header = T, nrows=2191)

Dates <- seq.Date(as.Date("2009-01-01"), as.Date("2012-12-31"),by=1)

flow_pred_z <- zoo(flow_pred,order.by=Dates)

fp <- ggplot(flow_pred_z,aes(x=time(flow_pred_z), y=observed))
fp <- fp + geom_line(colour = "darkblue", size=1.05)
fp <- fp + geom_line(aes(x=time(flow_pred_z), y=Best_Sim),colour = "red")
fp <- fp + geom_line(aes(x=time(flow_pred_z), y=L95PPU),colour = "red", linetype=2)
fp <- fp + geom_line(aes(x=time(flow_pred_z), y=U95PPU),colour = "red", linetype=2)
fp <- fp + xlab("Date")


print(fp)

flow_pred <- read.table("iterations/9thETCalibration0912/pso.out/95ppu.txt",skip=1,
                        header = T, nrows=2192)

flow_pred_z <- zoo(flow_pred,order.by=Dates)

fp <- ggplot(flow_pred_z,aes(x=time(flow_pred_z), y=observed))
fp <- fp + geom_line(colour = "darkblue", size=1.05)
fp <- fp + geom_line(aes(x=time(flow_pred_z), y=Best_Sim),colour = "red")
fp <- fp + geom_line(aes(x=time(flow_pred_z), y=L95PPU),colour = "red", linetype=2)
fp <- fp + geom_line(aes(x=time(flow_pred_z), y=U95PPU),colour = "red", linetype=2)
fp <- fp + xlab("Date")


print(fp)

mean(aggregate(flow_pred_z$observed,list(year=format(time(flow_pred_z),"%Y")),sum))
