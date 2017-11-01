# testing writing observed.txt for multiple flow data
# Uruguay project
# Willem Vervoort 2017

root <- "c:/users/rver4657/owncloud/southsouthnetwork"
setwd(paste(root,"satelliteCalibration",sep="/"))
library(tidyverse)
library(ggplot2)
library(zoo)

source("swatcup/SWATCUPfunctions.R")

# First organise the flow data
# Read in all the flow data
datadir <- "../resources/UruguaySWAT/UruguayCourse/data/SantaLuciaFlow"
PasoPache <- read.csv(paste(datadir,"DIARIOS HQ 59_1 Paso Pache_sep.csv",sep="/"))
SanRamon <- read.csv(paste(datadir,"DIARIOS HQ 23_0 San Ramón_sep.csv",sep="/"))
FrayMarcos <- read.csv(paste(datadir,"DIARIOS HQ 44_0 Fray Marcos_sep.csv",sep="/"))
PasoRoldan <- read.csv(paste(datadir,"DIARIOS HQ 117_0 Paso Roldán_sep.csv",sep="/"))
PasoTroncos <- read.csv(paste(datadir,"DIARIOS HQ 119_0 Paso de los Troncos_sep.csv",sep="/"))

# convert to zoo
PasoPacheflow <- zoo(PasoPache[,4], 
                     order.by=as.Date(PasoPache$fecha,"%d/%m/%Y"))
SanRamonflow <- zoo(SanRamon[,4],
                    order.by=as.Date(SanRamon$fecha,"%d/%m/%Y"))
FrayMarcosflow <- zoo(FrayMarcos[,4],
                      order.by=as.Date(FrayMarcos$fecha,"%d/%m/%Y"))
PasoRoldanflow <- zoo(PasoRoldan[,4],
                      order.by=as.Date(PasoRoldan$fecha,"%d/%m/%Y"))
PasoTroncosflow <- zoo(PasoTroncos[,4],
                      order.by=as.Date(PasoTroncos$fecha,"%d/%m/%Y"))


plot_df <- rbind(data.frame(Date=time(PasoPacheflow), 
                            Flow = coredata(PasoPacheflow),
                            station = "PasoPache"),
                 data.frame(Date=time(SanRamonflow), 
                            Flow = coredata(SanRamonflow),
                            station = "SanRamon"),
                 data.frame(Date=time(FrayMarcosflow), 
                            Flow = coredata(FrayMarcosflow),
                            station = "FrayMarcos"),
                 data.frame(Date=time(PasoTroncosflow), 
                            Flow = coredata(PasoTroncosflow),
                            station = "PasodelosTroncos"),
                 data.frame(Date=time(PasoRoldanflow), 
                            Flow = coredata(PasoRoldanflow),
                            station = "PasoRoldan"))
names(plot_df)
pl <- ggplot(plot_df,aes(Date,Flow)) + geom_line(aes(colour=station))
tiff(filename=paste(datadir,"PlotAllstations.tiff",sep="/"),
     width=720)
print(pl)
dev.off()

flow_df <- list("PasoPache" = data.frame(Date=time(PasoPacheflow), 
                            Flow = coredata(PasoPacheflow)),
                "SanRamon" = data.frame(Date=time(SanRamonflow), 
                            Flow = coredata(SanRamonflow)),
                "FrayMarcos" = data.frame(Date=time(FrayMarcosflow), 
                            Flow = coredata(FrayMarcosflow)),
                "PasodelosTroncos" = data.frame(Date=time(PasoTroncosflow), 
                            Flow = coredata(PasoTroncosflow)),
                "PasoRoldan" = data.frame(Date=time(PasoRoldanflow), 
                            Flow = coredata(PasoRoldanflow)))

# write this list away as an RDS file for later use
saveRDS(flow_df,file=paste(datadir,"AllSantaLuciaFlowdata.RDS",sep="/"))

# check whether "observed.txt" is in the correct folder
setwd("../resources/UruguaySWAT/UruguayCourse/data")
swatcup_MFformat(df_flow = flow_df[c(1:2,4:5)],
                 date.format = "%Y-%m-%d",
                 "2008-01-01", "2011-12-31",
                 "observed.txt" ,
                 "observed.txt", nlines = 16, 
                 weight = 0.25)

swatcup_MFformat(df_flow = flow_df[c(1:2,4:5)],
                 date.format = "%Y-%m-%d",
                 "2008-01-01", "2011-12-31",
                 "observed_rch.txt" ,
                 "observed_rch.txt", nlines = 6,
                 weight = 0.25)
