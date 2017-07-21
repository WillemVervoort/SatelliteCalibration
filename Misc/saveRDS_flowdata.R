require(zoo)

# write flow to RDS
flowdata <- read.csv("../Inputdata/Discharge_data_2000_2017.csv")
#replace the Gingera data
# read in original data
Gingera <- read.csv("../Inputdata/csv.w00002.20170625234920.410730.csv", skip=11,
                    header =F)
Dates <- substr(Gingera[,1],1,10)
Flow <- Gingera[,2]
Gingera_n <- data.frame(Dates=Dates,Flow=Flow)
# zoo and check
Gingera_z <- zoo(Gingera_n$Flow,order.by=as.Date(Gingera_n$Dates))
plot(Gingera_z)

Gingera_sub <- window(Gingera_z,start="2000-01-01",end="2017-06-05")

flowdata[,3] <- as.numeric(Gingera_sub)

saveRDS(flowdata, "../Inputdata/Discharge_data_2000_2017.RDS")
