# write flow to RDS
flowdata <- read.csv("../Inputdata/Discharge_data_2000_2017.csv")

saveRDS(flowdata, "../Inputdata/Discharge_data_2000_2017.RDS")