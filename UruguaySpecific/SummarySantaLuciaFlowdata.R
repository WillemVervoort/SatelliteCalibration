# check additional flow data Santa Lucia
require(tidyverse)
require(zoo)

setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/resources/UruguaySWAT/UruguayCourse")

# read in the files and plot
SanRamon <- read.csv("data/Santaluciaflow/DIARIOS HQ 23_0 San Ramón_sep.csv")
SanRamonflow <- zoo(SanRamon[,4],
                    order.by=as.Date(SanRamon$fecha,"%d/%m/%Y"))
plot(SanRamonflow, xlab="Date", ylab="Flow in cumecs",
     type="b", main="San Ramon")


FrayMarcos <- read.csv("data/Santaluciaflow/DIARIOS HQ 44_0 Fray Marcos_sep.csv")
FrayMarcosflow <- zoo(FrayMarcos[,4],
                    order.by=as.Date(FrayMarcos$fecha,"%d/%m/%Y"))
plot(FrayMarcosflow, xlab="Date", ylab="Flow in cumecs", type="b",
     main = "Fray Marcos")

PasoRoldan <- read.csv("data/Santaluciaflow/DIARIOS HQ 117_0 Paso Roldán_sep.csv")
PasoRoldanflow <- zoo(PasoRoldan[,4],
                      order.by=as.Date(PasoRoldan$fecha,"%d/%m/%Y"))
plot(PasoRoldanflow, xlab="Date", ylab="Flow in cumecs", type="b",
     main = "Paso Roldan")

PasodelosTroncos <- read.csv("data/Santaluciaflow/DIARIOS HQ 119_0 Paso de los Troncos_sep.csv")
PasodelosTroncosflow <- zoo(PasodelosTroncos[,4],
                      order.by=as.Date(PasodelosTroncos$fecha,"%d/%m/%Y"))
plot(PasodelosTroncosflow, xlab="Date", ylab="Flow in cumecs", type="b",
     main = "Paso de los Troncos")

#stack all stations to create a multiplot in ggplot
plot_df <- rbind(data.frame(Date=time(SanRamonflow), 
                            Flow = coredata(SanRamonflow),
                            station = "SanRamon"),
                 data.frame(Date=time(FrayMarcosflow), 
                            Flow = coredata(FrayMarcosflow),
                            station = "FrayMarcos"),
                 data.frame(Date=time(PasodelosTroncosflow), 
                            Flow = coredata(PasodelosTroncosflow),
                            station = "PasodelosTroncos"),
                 data.frame(Date=time(PasoRoldanflow), 
                            Flow = coredata(PasoRoldanflow),
                            station = "PasoRoldan"))
names(plot_df)
pl <- ggplot(plot_df,aes(Date,Flow)) + geom_line(aes(colour=station))
tiff(filename="data/SantaLuciaflow/PlotAllstations.tiff",
     width=720)
print(pl)
dev.off()