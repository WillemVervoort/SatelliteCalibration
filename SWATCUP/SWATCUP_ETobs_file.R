# rewriting SWATCUP input files
setwd("X:/PRJ-HPWC/SWAT_ETCalibration/SatelliteCalibration")
source("SWATCUP/SWATCUPfunctions.R")

#testing and application
# read in flow data
flowdata <- readRDS(file="../inputdata/Discharge_data_2000_2017.RDS")
head(flowdata)
colnames(flowdata)[1] <- "Date"

# Create a single file with all the MODIS ET data for all points
ET_Data <- MODIS_ts("../MODIS/Cotter")
# show the data
head(ET_Data)

# save file for later
#saveRDS(ET_Data,"c:/users/rver4657/documents/test/ETData.RDS")

# for testing:
setwd("c:/users/rver4657/documents/test")

# write observed_sub.txt
swatcup_ETformat(ET_Data, df_flow = NULL, date.format = "%Y-%m-%d",
                             "2009-01-01", "2012-12-31",
                 "observed_sub.txt" ,"observed_sub.txt", 6, weight= 0.1)



# write observed.txt
swatcup_ETformat(ET_Data, df_flow = flowdata[,c(1,3)],
                 date.format = "%Y-%m-%d",
                 "2007-01-01", "2012-12-31",
                 "observed.txt" ,"observed.txt", 14, Flow = TRUE, weight = 0.1)

# Now test putting in weights relative to the size of the subcatchment
subbasin_data <- read.csv("R:/PRJ-HPWC/SWAT_ETCalibration/inputdata/subbasins_Cotter_alldata.csv")

# calculate weights from relative areas
f_w <- 0.1 # flow weight
ET_w <- subbasin_data$Area/sum(subbasin_data$Area)*(1-f_w)
 w_in <- c(f_w, ET_w)

 # now try to write the file
swatcup_ETformat(ET_Data, df_flow = flowdata[,c(1,3)],
                  date.format = "%Y-%m-%d",
                  "2009-01-01", "2012-12-31",
                  "observed.txt" ,"observed.txt", 14, Flow = TRUE, weight = w_in)
 