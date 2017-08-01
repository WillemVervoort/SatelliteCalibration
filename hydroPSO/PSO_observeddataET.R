# Creating the observed data for calibration with flow and other data
# in hydroPSO
# assumes input data are either zoo or a wide data.frame 
# (if multiple points) with the first column the Dates
if(require(zoo)==F) library(zoo)

create_obs_data <- function(data1, data2=NULL, d.ini, d.end) {
# browser()
    if (all(grepl("Date",colnames(data1))==F) | all(grepl("Date",colnames(data2))==F)) {
        warning("assuming first column is in Date format")
        colnames(data1)[1] <- colnames(data2)[1] <- "Date"
      }
  if (regexpr("-",data1$Date[1])!=5 | 
      regexpr("-",data2$Date[1])!=5) {
    message("Trying %d/%m/%Y format")
    if (regexpr("/", data1$Date[1]==3)){
      data1[,1] <- as.Date(data1$Date,"%d/%m/%Y") 
      if (regexpr("/", data2$Date[1]==3)){
        data1[,1] <- as.Date(data2$Date,"%d/%m/%Y") 
      }
    } else message("Cannot determine date format, please change")
  } else {
    # zoo both data sets
    #browser()
    data1_z <- zoo(data1[,-1],order.by=as.Date(data1$Date))
    if (length(data2)>0) {
      data2_z <- zoo(data2[,-1],order.by=as.Date(data2$Date))
      # merge
      data3 <- merge(data1_z,data2_z, all = T)
      rm(list=c("data1_z","data2_z"))
      out <- window(data3, start=as.Date(d.ini), end=as.Date(d.end))
    } else {
      out <- window(data1_z, start=as.Date(d.ini), end=as.Date(d.end))
      }
  }
  return(out)
}