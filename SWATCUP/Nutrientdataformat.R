# Converting tje nutrient data SantaLucia
# Also to test writing the observed.txt file for nutrient data 

root <- "c:/users/rver4657/owncloud/southsouthnetwork"
setwd(paste(root,"satelliteCalibration",sep="/"))
library(tidyverse)
library(ggplot2)
library(zoo)


# First organise the flow data
# Read in all the nutrient data
datadir <- "../resources/UruguaySWAT/UruguayCourse/data/SantaLuciaFlow"
SL_NutrientData <- read.csv(paste(datadir,
                            "Monitoreo_Dinama_Subcuenca_Santa_Lucia.csv",
                            sep = "/"))

# However, this data is in ppm or mg/L
# and fosforo total is even in ug/L
# need to multiply by daily flow values
# read in the flow data, this is a list
flow_df <- readRDS(paste(datadir,"AllSantaLuciaFlowdata.RDS",sep="/"))
# we will have to use zoo to do the date matching with flow, 
# otherwise it is really to difficult.


# Create the required list of the nutrient data
# first split the original nutrient data frame by location
PasoPacheNutr <- SL_NutrientData[SL_NutrientData$Station_name=="PasoPache",]
SanRamonNutr <- SL_NutrientData[SL_NutrientData$Station_name=="SanRamon",]
PasoRoldanNutr <- SL_NutrientData[SL_NutrientData$Station_name=="PasoRoldan",]
PasoTroncosNutr <- SL_NutrientData[SL_NutrientData$Station_name=="PasodelosTroncos",]

# now select only certain columns (the nutrients relevant for SWAT)
PasoPacheNutr <- select(PasoPacheNutr,Station_name,Fecha,Nitratos,
                        Nitritos, Amonio, Nitrog..tot.,
                        Fosforo.tot.)
# change the dates to different format
PasoPacheNutr$Fecha <- as.Date(PasoPacheNutr$Fecha,"%d/%m/%Y")
# zoo both data sets, but only the numeric values
PasoPacheNutr_z <- zoo(PasoPacheNutr[,3:7], order.by = as.Date(PasoPacheNutr$Fecha))
PasoPacheflow_z <- zoo(flow_df[[1]][,2], order.by=flow_df[[1]][,1])
# merge on common dates
PasoPache_m <- merge(PasoPacheNutr_z,PasoPacheflow_z,all=F)
# multiply nutrients by flow, but convert flow to L/day an to kg
# m3/sec -> L/day: 86400*cumecs/1000
# mg -> kg -> weight/1E06
PasoPache_new <- apply(PasoPache_m[,1:5],2,
                       function(x) x*PasoPache_m[,6]*86400/1000/1000000)
# correct the ug/L for fosforo.tot.
PasoPache_new[,5] <- PasoPache_new[,5]/1000 

# San Ramon, we can't do, as we don't have flow data past 2010
# SanRamonNutr <- select(SanRamonNutr,Station_name,Fecha,Nitratos,
#                         Nitritos, Amonio, Nitrog..tot.,
#                         Fosforo.tot.)
# # change the dates to different format
# SanRamonNutr$Fecha <- as.Date(SanRamonNutr$Fecha,"%d/%m/%Y")
# # zoo both data sets, but only the numeric values
# SanRamonNutr_z <- zoo(SanRamonNutr[,3:7], order.by = as.Date(SanRamonNutr$Fecha))
# SanRamonflow_z <- zoo(flow_df[[2]][,2], order.by=flow_df[[2]][,1])
# # merge on common dates
# SanRamon_m <- merge(SanRamonNutr_z,SanRamonflow_z,all=F)
# # multiply nutrients by flow, but convert flow to L/day an to kg
# # m3/sec -> L/day: 86400*cumecs/1000
# # mg -> kg -> weight/1E06
# SanRamon_new <- apply(SanRamon_m[,1:5],2,
#                        function(x) x*SanRamon_m[,6]*86400/1000/1000000)
# # correct the ug/L for fosforo.tot.
# SanRamon_new[,5] <- SanRamon_new[,5]/1000 

PasoRoldanNutr <- select(PasoRoldanNutr,Station_name,Fecha,Nitratos,
                        Nitritos, Amonio, Nitrog..tot.,
                        Fosforo.tot.)
# change the dates to different format
PasoRoldanNutr$Fecha <- as.Date(PasoRoldanNutr$Fecha,"%d/%m/%Y")
# zoo both data sets, but only the numeric values
PasoRoldanNutr_z <- zoo(PasoRoldanNutr[,3:7], order.by = as.Date(PasoRoldanNutr$Fecha))
PasoRoldanflow_z <- zoo(flow_df[[3]][,2], order.by=flow_df[[3]][,1])
# merge on common dates
PasoRoldan_m <- merge(PasoRoldanNutr_z,PasoRoldanflow_z,all=F)
# multiply nutrients by flow, but convert flow to L/day an to kg
# m3/sec -> L/day: 86400*cumecs/1000
# mg -> kg -> weight/1E06
PasoRoldan_new <- apply(PasoRoldan_m[,1:5],2,
                       function(x) x*PasoRoldan_m[,6]*86400/1000/1000000)
# correct the ug/L for fosforo.tot.
PasoRoldan_new[,5] <- PasoRoldan_new[,5]/1000 

PasoTroncosNutr <- select(PasoTroncosNutr,Station_name,Fecha,Nitratos,
                         Nitritos, Amonio, Nitrog..tot.,
                         Fosforo.tot.)
# change the dates to different format
PasoTroncosNutr$Fecha <- as.Date(PasoTroncosNutr$Fecha,"%d/%m/%Y")
# zoo both data sets, but only the numeric values
PasoTroncosNutr_z <- zoo(PasoTroncosNutr[,3:7], order.by = as.Date(PasoTroncosNutr$Fecha))
PasoTroncosflow_z <- zoo(flow_df[[4]][,2], order.by=flow_df[[4]][,1])
# merge on common dates
PasoTroncos_m <- merge(PasoTroncosNutr_z,PasoTroncosflow_z,all=F)
# multiply nutrients by flow, but convert flow to L/day an to kg
# m3/sec -> L/day: 86400*cumecs/1000
# mg -> kg -> weight/1E06
PasoTroncos_new <- apply(PasoTroncos_m[,1:5],2,
                       function(x) x*PasoTroncos_m[,6]*86400/1000/1000000)
# correct the ug/L for fosforo.tot.
PasoTroncos_new[,5] <- PasoTroncos_new[,5]/1000 


nutrient_df <- list("PasoPacheNitratos" = data.frame(Date=time(PasoPache_m), 
                                         Nutrient = coredata(PasoPache_new[,"Nitratos"])),
                    "PasoPacheAmonio" = data.frame(Date=time(PasoPache_m), 
                                                   Nutrient = coredata(PasoPache_new[,"Amonio"])),
                    "PasoPacheNitritos" = data.frame(Date=time(PasoPache_m), 
                                                     Nutrient = coredata(PasoPache_new[,"Nitritos"])),
                    "PasoPacheN_tot" = data.frame(Date=time(PasoPache_m), 
                                                  Nutrient = coredata(PasoPache_new[,"Nitrog..tot."])),
                    "PasoPacheFos_tot" = data.frame(Date=time(PasoPache_m), 
                                                    Nutrient = coredata(PasoPache_new[,"Fosforo.tot."])),
                    # "SanRamonNitratos" = data.frame(Date=SanRamonNutr$Fecha, 
                    #                                  Nutrient = SanRamonNutr$Nitratos),
                    # "SanRamonNitritos" = data.frame(Date=SanRamonNutr$Fecha, 
                    #                                  Nutrient = SanRamonNutr$Nitritos),
                    # "SanRamonAmonio" = data.frame(Date=SanRamonNutr$Fecha, 
                    #                                Nutrient = SanRamonNutr$Amonio),
                    # "SanRamonN_tot" = data.frame(Date=SanRamonNutr$Fecha, 
                    #                               Nutrient = SanRamonNutr$Nitrog.tot.),
                    # "SanRamonFosfatos" = data.frame(Date=SanRamonNutr$Fecha, 
                    #                                  Nutrient = SanRamonNutr$Fosfatos),
                    # "SanRamonFos_tot" = data.frame(Date=SanRamonNutr$Fecha, 
                    #                                 Nutrient = SanRamonNutr$Fosforo.tot.),
                    "PasoRoldanNitratos" = data.frame(Date=time(PasoRoldan_m), 
                                              Nutrient = coredata(PasoRoldan_new[,"Nitratos"])),
                    "PasoRoldanAmonio" = data.frame(Date=time(PasoRoldan_m), 
                                                    Nutrient = coredata(PasoRoldan_new[,"Amonio"])),
                    "PasoRoldanNitritos" = data.frame(Date=time(PasoRoldan_m), 
                                                      Nutrient = coredata(PasoRoldan_new[,"Nitritos"])),
                    "PasoRoldanN_tot" = data.frame(Date=time(PasoRoldan_m), 
                                                   Nutrient = coredata(PasoRoldan_new[,"Nitrog..tot."])),
                    "PasoRoldanFos_tot" = data.frame(Date=time(PasoRoldan_m), 
                                                     Nutrient = coredata(PasoRoldan_new[,"Fosforo.tot."])),
                    "PasoTroncosNitratos" = data.frame(Date=time(PasoTroncos_m), 
                                                     Nutrient = coredata(PasoTroncos_new[,"Nitratos"])),
                    "PasoTroncosAmonio" = data.frame(Date=time(PasoTroncos_m), 
                                                    Nutrient = coredata(PasoTroncos_new[,"Amonio"])),
                    "PasoTroncosNitritos" = data.frame(Date=time(PasoTroncos_m), 
                                                       Nutrient = coredata(PasoTroncos_new[,"Nitritos"])),
                    "PasoTroncosN_tot" = data.frame(Date=time(PasoTroncos_m), 
                                                    Nutrient = coredata(PasoTroncos_new[,"Nitrog..tot."])),
                    "PasoTroncosFos_tot" = data.frame(Date=time(PasoTroncos_m), 
                                                      Nutrient = coredata(PasoTroncos_new[,"Fosforo.tot."])))
# write this list away as an RDS file for later use
saveRDS(nutrient_df,file=paste(datadir,"SantaLuciaNutrdata.RDS",sep="/"))

# check whether "observed.txt" is in the correct folder
setwd("../resources/UruguaySWAT/UruguayCourse/data")

swatcup_MFformat(df_flow = flow_df[c(1,4:5)], 
                 df_nutrient = nutrient_df,
                 date.format = "%Y-%m-%d",
                 st.date ="2010-01-01", end.date ="2015-12-31",
                 infile = "observed.txt" ,
                 outfile = "observed_test.txt", nlines = 16, 
                 weight = c(rep(0.5/3,3),rep(0.5/15,15)))

swatcup_MFformat(df_flow = flow_df[c(1,4:5)],
                 df_nutrient = nutrient_df,
                 date.format = "%Y-%m-%d",
                 st.date = "2008-01-01", end.date = "2015-12-31",
                 infile = "observed_rch.txt" ,
                 outfile = "observed_rch_test.txt", nlines = 6,
                 weight = c(rep(0.5/3,3),rep(0.5/15,15)))

