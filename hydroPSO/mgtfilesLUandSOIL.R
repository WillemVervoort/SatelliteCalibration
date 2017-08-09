# File to sort management files by landuse and by soil
setwd("R:/prj-hpwc/swat_etcalibration/pso_goodra")

filelist <- list.files(pattern="mgt")

store <- data.frame(files = as.character(filelist))


for (i in 1:length(filelist)) {
 fline <- read.table(filelist[i], header=F, nrow=1)
 store$LU[i] <-  as.character(substr(fline[,7],6,9))
 store$Soil[i] <- as.character(fline[,9])
}

head(store)

write.table(store,"PSO.in/LUSoilmgtfiles.txt",row.names=F)
