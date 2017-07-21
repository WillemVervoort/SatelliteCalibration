# Create hydromad input file for Santa Lucia
require(zoo)
require(tidyverse)

setwd("R:/PRJ-HPWC/SWAT_ETCalibration")

# read in the flow data and the weather data
Flow <- read_csv("SantaLuciadata/FlowPasoPache.csv")

WD <- read.csv("SantaLuciadata/INIALasBrujas_1983-2016.csv")

# There are two different dates: 31-12-2016 and 1/1/1983
# Need to convert to standard dates
Dates <- ifelse(grepl("-",WD$Fecha)==TRUE,
                as.character(as.Date(WD$Fecha,"%d-%m-%Y")),
                as.character(as.Date(WD$Fecha,"%d/%m/%Y")))

# Zoo the data and save again
WD_z <- zoo(WD[,2:ncol(WD)],order.by=as.Date(Dates))
write_csv(as.data.frame(WD_z), "SantaLuciadata/WeatherLasBrujas_1983-2016.csv")

# also zoo flow
Flow_z <- zoo(Flow$Flow, order.by=as.Date(Flow$Date,"%d/%m/%Y"))

SantaLucia <- merge(WD_z[,6],Flow_z,WD_z[,2])
names(SantaLucia) <- c("P","Q","E")
# Cut to length of flow data
SantaLucia <- window(SantaLucia,start = time(Flow_z)[1], 
                    end =time(Flow_z)[length(Flow_z)])

save(SantaLucia, file = "UruguayCourse/data/SantaLucia.Rdata")

plot(SantaLucia)
