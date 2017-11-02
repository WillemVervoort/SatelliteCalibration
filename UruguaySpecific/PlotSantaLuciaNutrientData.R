# Plotting the SantaLucia Nutrient data
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
                                  "Monitoreo_Dinama_Subcuenca_Santa_Lucia.csv",
                                  sep = "/"))

# plot all data
plot_nutrient <- gather(SL_NutrientData, key="Nutrient", 
                        value = "Measurement", Temperatura:Turbiedad)
plot_nutrient$Measurement <- as.numeric(plot_nutrient$Measurement)


pl <- ggplot(plot_nutrient,aes(as.Date(Fecha,"%d/%m/%Y"),Measurement)) + 
  geom_point(aes(colour=Station_name), size = 3) + 
  facet_wrap(~Nutrient, scales="free")
tiff(filename=paste(datadir,"PlotAllNutrients.tiff",sep="/"),
     width=720)
print(pl)
dev.off()


# redo this with only the key nutrients: Amonio, Fosforo.tot, Nitratos, Nitritos
plot_nutrient_set <- gather(SL_NutrientData, key="Nutrient", 
                            value = "Measurement", 
                            Amonio, Fosforo.tot., Nitratos, Nitritos, Nitrog..tot.)
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
