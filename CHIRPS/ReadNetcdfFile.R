# extract CHIRPS data from netcdf file
#

setwd("c:/users/rver4657/owncloud/southsouthnetwork/satellitecalibration")

# load the netcdf4 library
require(ncdf4)

# understand the structure of the netcdf file

# first open the netcdf file
chirps_nc <- nc_open( "data/chirps_SL_data.nc" )
print(chirps_nc)
str(chirps_nc)

# dimensions of the data file are hidden in a list
dims <- chirps_nc$var$prcp$size
lats <- chirps_nc$dim$Y$vals
longs <- chirps_nc$dim$X$vals

# read  in data for 1 point from the file
test <- ncvar_get(chirps_nc, count=c(1,1,dims[3]) )
plot(seq.Date(as.Date("2000-01-01"),as.Date("2017-09-30"),by=1),test,
     type="h",col="blue")

output <- list()

for (i in 1:dims[1]) {
  for (j in 1:dims[2]) {
    output[[(i-1)*dims[2]+j]] <- list(x = longs[i],
                                     y = lats[j],
                                     prcp =  ncvar_get(chirps_nc, 
                                                       start = c(i,j,1),
                                                       count=c(1,1,dims[3])))
  
    # temp_list <- list(x = longs[i],
    #                                   y = lats[j],
    #                                   prcp =  ncvar_get(chirps_nc, count=c(i,j,dims[3])))
    # saveRDS(temp_list, paste("Data/CHIRPS/point_",i,"_",j,".RDS",sep=""))
    
  }
}
nc_close(chirps_nc)

saveRDS(output,"data/CHIRPS/output.RDS")
