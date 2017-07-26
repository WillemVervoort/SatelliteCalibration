# plot the performance of the ET data calibration in space

# read in a shape file for the catchment

# Following packages are required
# make sure all packages are updated
require(raster)
require(maptools)
require(rgdal)
require(tidyverse)

# Reading the shape file of the catchment: move this to the Inputdata dir 
cotter <- readShapePoly("R:/GRP-HGIS/Public/SWAT_DB/temp/CotterLatLong.shp")

# setting up the projection of the shapefile
proj <- "+proj=longlat +ellps=WGS84"
crs(cotter) <- proj

subbasins <- read.csv("R:/PRJ-HPWC/SWAT_ETCalibration/InputData/subbasins_cotter.csv")
vegsites <- read.csv("R:/PRJ-HPWC/SWAT_ETCalibration/InputData/VegSitesCoords.csv")[1:6,]

# setwd()
setwd("R:/PRJ-HPWC/SWAT_ETCalibration/Cotter2017.PSO.SwatCup")

# now read in the results
sim_res <- read_table("iterations/FirstETCalibration/PSO.OUT/summary_stat.txt", skip=3)

# now link results to lat and longs of subbasins

KGE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      KGE = sim_res$KGE[2:26])

NSE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      NSE = sim_res$NS[2:26])

# plotting
gp <- ggplot(cotter, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = NSE_sub, aes(x = long, y = lat, col=NSE, 
                                          size = NSE))
gp <- gp + geom_point(data = vegsites, aes(x = Long, y = Lat), col="green") +
  geom_text(data = vegsites, aes(x = Long, y = Lat,label=Site), vjust=-1)
print(gp)

# calibration only on flow
# read in the stats
sim_res <- read.csv("Iterations/flowcalibration0912/summary_ETstats.csv")

# now link results to lat and longs of subbasins
KGE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      KGE = sim_res$KGE)

NSE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      NSE = sim_res$NSE)


# plotting
gp <- ggplot(cotter, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = KGE_sub, aes(x = long, y = lat, col = KGE, 
                                          size = KGE))
gp <- gp + geom_point(data = vegsites, aes(x = Long, y = Lat), col="green") +
  geom_text(data = vegsites, aes(x = Long, y = Lat,label=Site), vjust=-1)
print(gp)

# now read in the results for 2nd ET calibration
sim_res <- read_table("iterations/SecondETCalibration/PSO.OUT/summary_stat.txt", skip=3)

# now link results to lat and longs of subbasins
KGE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      KGE = sim_res$KGE[2:26])

NSE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      NSE = sim_res$NS[2:26])


# plotting
gp <- ggplot(cotter, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = NSE_sub, aes(x = long, y = lat, col=NSE, 
                                          size = NSE))
gp <- gp + geom_point(data = vegsites, aes(x = Long, y = Lat), col="green") +
  geom_text(data = vegsites, aes(x = Long, y = Lat,label=Site), vjust=-1)
print(gp)


# now read in the results for 3rd ET calibration
sim_res <- read_table("iterations/ThirdETCalibration01Flow/PSO.OUT/summary_stat.txt", skip=3)

# now link results to lat and longs of subbasins
KGE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      KGE = sim_res$KGE[2:26])

NSE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      NSE = sim_res$NS[2:26])


# plotting
gp <- ggplot(cotter, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = KGE_sub, aes(x = long, y = lat, col=KGE, 
                                          size = 2*KGE))
gp <- gp + geom_point(data = vegsites, aes(x = Long, y = Lat), col="green") +
  geom_text(data = vegsites, aes(x = Long, y = Lat,label=Site), vjust=-1)
print(gp)

# now read in the results for 4th ET calibration
sim_res <- read_table("iterations/FourthETCalibration/PSO.OUT/summary_stat.txt", skip=3)

# now link results to lat and longs of subbasins
KGE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      KGE = sim_res$KGE[2:26])

NSE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      NSE = sim_res$NS[2:26])


# plotting
gp <- ggplot(cotter, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = KGE_sub, aes(x = long, y = lat, col=KGE, 
                                          size = 2*KGE))
gp <- gp + geom_point(data = vegsites, aes(x = Long, y = Lat), col="green") +
  geom_text(data = vegsites, aes(x = Long, y = Lat,label=Site), vjust=-1)
print(gp)

# now read in the results for 5th ET calibration
sim_res <- read_table("iterations/FifthETCalibration/PSO.OUT/summary_stat.txt", skip=3)

# now link results to lat and longs of subbasins
KGE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      KGE = sim_res$KGE[2:26])

NSE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      NSE = sim_res$NS[2:26])


# plotting
gp <- ggplot(cotter, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = KGE_sub, aes(x = long, y = lat, col=KGE, 
                                          size = 2*KGE))
gp <- gp + geom_point(data = vegsites, aes(x = Long, y = Lat), col="green") +
  geom_text(data = vegsites, aes(x = Long, y = Lat,label=Site), vjust=-1)
print(gp)

# now read in the results for sixth ET calibration (changed pars and 0.1 flow)
sim_res <- read_table("iterations/SixthETCalibration01Flow/PSO.OUT/summary_stat.txt", skip=3)

# now link results to lat and longs of subbasins
KGE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      KGE = sim_res$KGE[2:26])

NSE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      NSE = sim_res$NS[2:26])


# plotting
gp <- ggplot(cotter, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = KGE_sub, aes(x = long, y = lat, col=KGE, 
                                          size = 2*KGE))
gp <- gp + geom_point(data = vegsites, aes(x = Long, y = Lat), col="green") +
  geom_text(data = vegsites, aes(x = Long, y = Lat,label=Site), vjust=-1)
print(gp)

# now read in the results for sixth ET calibration (changed pars and 0.1 flow)
sim_res <- read_table("iterations/7thETCalibration/PSO.OUT/summary_stat.txt", skip=3)

# now link results to lat and longs of subbasins
KGE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      KGE = sim_res$KGE[2:26])

NSE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      NSE = sim_res$NS[2:26])


# plotting
gp <- ggplot(cotter, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = KGE_sub, aes(x = long, y = lat, col=KGE, 
                                          size = 2*KGE))
gp <- gp + geom_point(data = vegsites, aes(x = Long, y = Lat), col="green") +
  geom_text(data = vegsites, aes(x = Long, y = Lat,label=Site), vjust=-1)
print(gp)

# now read in the results for eigth ET calibration (changed pars and 0.1 flow)
sim_res <- read_table("iterations/9thETCalibration0912/PSO.OUT/summary_stat.txt", skip=3)

# now link results to lat and longs of subbasins
KGE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      KGE = sim_res$KGE[2:26])

NSE_sub <- data.frame(long = subbasins[,3],
                      lat = subbasins[,2],
                      NSE = sim_res$NS[2:26])


# plotting
gp <- ggplot(cotter, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = KGE_sub, aes(x = long, y = lat, col=KGE, 
                                          size = KGE))
gp <- gp + geom_point(data = vegsites, aes(x = Long, y = Lat), col="green") +
  geom_text(data = vegsites, aes(x = Long, y = Lat,label=Site), vjust=-1)
print(gp)
