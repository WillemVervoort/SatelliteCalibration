# testing nutrient data SantaLucia
# Also to write observed.txt file for nutrient data 
# Uruguay project
# Willem Vervoort 2017

root <- "c:/users/rver4657/owncloud/southsouthnetwork"
setwd(paste(root,"satelliteCalibration",sep="/"))
library(tidyverse)
library(ggplot2)
library(zoo)


# First organise the flow data
# Read in all the nutrient data
datadir <- "../resources/UruguaySWAT/UruguayCourse/data/SantaLuciaFlow"
SL_NutrientData <- read.csv(paste(datadir,
                            "Monitoreo_DINAMA_SubcuencaSL.csv",
                            sep = "/"))

# plot all data
plot_nutrient <- gather(SL_NutrientData, key="Nutrient", 
                        value = "Measurement", Temperatura:Clorofila.a)
plot_nutrient$Measurement <- as.numeric(plot_nutrient$Measurement)


pl <- ggplot(plot_nutrient,aes(as.Date(Fecha,"%d/%m/%Y"),Measurement)) + 
  geom_point(aes(colour=Station_name), size = 3) + 
  facet_wrap(~Nutrient)
tiff(filename=paste(datadir,"PlotAllNutrients.tiff",sep="/"),
     width=720)
print(pl)
dev.off()

# redo this with only the key nutrients: Amonio, Fosforo.tot, Nitratos, Nitritos
plot_nutrient_set <- gather(SL_NutrientData, key="Nutrient", 
                        value = "Measurement", 
                        Amonio, Fosforo.tot., Nitratos, Nitritos, Nitrog.tot.)
plot_nutrient_set$Measurement <- 
      as.numeric(plot_nutrient_set$Measurement)


pl <- ggplot(plot_nutrient_set,
             aes(as.Date(Fecha,"%d/%m/%Y"),Measurement)) + 
  geom_point(aes(colour=Station_name), size = 3) + 
  facet_wrap(~Nutrient,scales="free") + xlab("Date")
tiff(filename=paste(datadir,"PlotSelectNutrients.tiff",sep="/"),
     width=720)
print(pl)
dev.off()

# Create the required list of the nutrient data
# first split the original data frame by location

PasoPacheNutr <- SL_NutrientData[SL_NutrientData$Station_name=="PasoPache",]
SanRamonNutr <- SL_NutrientData[SL_NutrientData$Station_name=="SanRamon",]
PasoRoldanNutr <- SL_NutrientData[SL_NutrientData$Station_name=="PasoRoldan",]
PasoTroncosNutr <- SL_NutrientData[SL_NutrientData$Station_name=="PasodelosTroncos",]

# now select only certain columns (the nutrients)
PasoPacheNutr <- select(PasoPacheNutr,Station_name,Fecha,Nitratos,
                        Nitritos, Amonio, Nitrog.tot.,Fosfatos,
                        Fosforo.tot.)
# change the dates to different format
PasoPacheNutr$Fecha <- as.Date(PasoPacheNutr$Fecha,"%d/%m/%Y")

SanRamonNutr <- select(SanRamonNutr,Station_name,Fecha,Nitratos,
                        Nitritos, Amonio, Nitrog.tot.,Fosfatos,
                        Fosforo.tot.)
# change the dates to different format
SanRamonNutr$Fecha <- as.Date(SanRamonNutr$Fecha,"%d/%m/%Y")

PasoRoldanNutr <- select(PasoRoldanNutr,Station_name,Fecha,Nitratos,
                        Nitritos, Amonio, Nitrog.tot.,Fosfatos,
                        Fosforo.tot.)
# change the dates to different format
PasoRoldanNutr$Fecha <- as.Date(PasoRoldanNutr$Fecha,"%d/%m/%Y")

PasoTroncosNutr <- select(PasoTroncosNutr,Station_name,Fecha,Nitratos,
                         Nitritos, Amonio, Nitrog.tot.,Fosfatos,
                         Fosforo.tot.)
# change the dates to different format
PasoTroncosNutr$Fecha <- as.Date(PasoTroncosNutr$Fecha,"%d/%m/%Y")


nutrient_df <- list("PasoPacheNitratos" = data.frame(Date=PasoPacheNutr$Fecha, 
                                         Nutrient = PasoPacheNutr$Nitratos),
                    "PasoPacheNitritos" = data.frame(Date=PasoPacheNutr$Fecha, 
                                                     Nutrient = PasoPacheNutr$Nitritos),
                    "PasoPacheAmonio" = data.frame(Date=PasoPacheNutr$Fecha, 
                                                     Nutrient = PasoPacheNutr$Amonio),
                    "PasoPacheN_tot" = data.frame(Date=PasoPacheNutr$Fecha, 
                                                     Nutrient = PasoPacheNutr$Nitrog.tot.),
                    "PasoPacheFosfatos" = data.frame(Date=PasoPacheNutr$Fecha, 
                                                     Nutrient = PasoPacheNutr$Fosfatos),
                    "PasoPacheFos_tot" = data.frame(Date=PasoPacheNutr$Fecha, 
                                                     Nutrient = PasoPacheNutr$Fosforo.tot.),
                    "SanRamonNitratos" = data.frame(Date=SanRamonNutr$Fecha, 
                                                     Nutrient = SanRamonNutr$Nitratos),
                    "SanRamonNitritos" = data.frame(Date=SanRamonNutr$Fecha, 
                                                     Nutrient = SanRamonNutr$Nitritos),
                    "SanRamonAmonio" = data.frame(Date=SanRamonNutr$Fecha, 
                                                   Nutrient = SanRamonNutr$Amonio),
                    "SanRamonN_tot" = data.frame(Date=SanRamonNutr$Fecha, 
                                                  Nutrient = SanRamonNutr$Nitrog.tot.),
                    "SanRamonFosfatos" = data.frame(Date=SanRamonNutr$Fecha, 
                                                     Nutrient = SanRamonNutr$Fosfatos),
                    "SanRamonFos_tot" = data.frame(Date=SanRamonNutr$Fecha, 
                                                    Nutrient = SanRamonNutr$Fosforo.tot.),
                    "PasoRoldanNitratos" = data.frame(Date=PasoRoldanNutr$Fecha, 
                                              Nutrient = PasoRoldanNutr$Nitratos),
                    "PasoRoldanNitritos" = data.frame(Date=PasoRoldanNutr$Fecha, 
                                                     Nutrient = PasoRoldanNutr$Nitritos),
                    "PasoRoldanAmonio" = data.frame(Date=PasoRoldanNutr$Fecha, 
                                                   Nutrient = PasoRoldanNutr$Amonio),
                    "PasoRoldanN_tot" = data.frame(Date=PasoRoldanNutr$Fecha, 
                                                  Nutrient = PasoRoldanNutr$Nitrog.tot.),
                    "PasoRoldanFosfatos" = data.frame(Date=PasoRoldanNutr$Fecha, 
                                                     Nutrient = PasoRoldanNutr$Fosfatos),
                    "PasoRoldanFos_tot" = data.frame(Date=PasoRoldanNutr$Fecha, 
                                                    Nutrient = PasoRoldanNutr$Fosforo.tot.),
                    "PasoTroncosNitratos" = data.frame(Date=PasoTroncosNutr$Fecha, 
                                                     Nutrient = PasoTroncosNutr$Nitratos),
                    "PasoTroncosNitritos" = data.frame(Date=PasoTroncosNutr$Fecha, 
                                                     Nutrient = PasoTroncosNutr$Nitritos),
                    "PasoTroncosAmonio" = data.frame(Date=PasoTroncosNutr$Fecha, 
                                                   Nutrient = PasoTroncosNutr$Amonio),
                    "PasoTroncosN_tot" = data.frame(Date=PasoTroncosNutr$Fecha, 
                                                  Nutrient = PasoTroncosNutr$Nitrog.tot.),
                    "PasoTroncosFosfatos" = data.frame(Date=PasoTroncosNutr$Fecha, 
                                                     Nutrient = PasoTroncosNutr$Fosfatos),
                    "PasoTroncosFos_tot" = data.frame(Date=PasoTroncosNutr$Fecha, 
                                                    Nutrient = PasoTroncosNutr$Fosforo.tot.))
# write this list away as an RDS file for later use
saveRDS(nutrient_df,file=paste(datadir,"SantaLuciaNutrdata.RDS",sep="/"))

# check whether "observed.txt" is in the correct folder
setwd("../resources/UruguaySWAT/UruguayCourse/data")
flow_df <- 
  readRDS(file = "SantaLuciaFlow/AllSantaLuciaFlowdata.RDS")

swatcup_MFformat(df_flow = flow_df[c(1:2,4:5)], 
                 df_nutrient = nutrient_df,
                 date.format = "%Y-%m-%d",
                 st.date ="2008-01-01", end.date ="2015-12-31",
                 infile = "observed.txt" ,
                 outfile = "observed_test.txt", nlines = 16, 
                 weight = c(rep(0.125,4),rep(0.5/24,24)))

swatcup_MFformat(df_flow = flow_df[c(1:2,4:5)],
                 df_nutrient = nutrient_df,
                 date.format = "%Y-%m-%d",
                 st.date = "2008-01-01", end.date = "2015-12-31",
                 infile = "observed_rch.txt" ,
                 outfile = "observed_rch_test.txt", nlines = 6,
                 weight = c(rep(0.125,4),rep(0.5/24,24)))

