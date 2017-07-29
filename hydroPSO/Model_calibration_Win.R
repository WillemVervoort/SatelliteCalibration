## Loading the required packages

require(tidyverse)
require(zoo)
require(data.table)
require(hydroGOF)
require(hydroPSO)
require(doParallel)


### Defining the model input and output -------------------------------------###
## PSO.in folder should be into the model folder
## PSO.out folder should be into the model folder
## Scrips should be in the scripts folder
### ------------------------------------------------------------------------ ###
## Specifying the model dir
model.drty <- "X:/PRJ-HPWC/SWAT_ETCalibration/PSO_Cotter"
setwd(model.drty)




### Loading the scripts to extract modelled values---------------------------###

source(paste(model.drty,"/Scripts/sub_extract.R", sep = ""))
source(paste(model.drty,"/Scripts/KGE_w.R", sep = ""))
source(paste(model.drty,"/Scripts/PSO_observeddataET.R", sep = ""))


### Reading the observed values
# Flow data
Station <- "Flow_cumec_Gingira_410730"
flow <- readRDS("PSO.in/Discharge_data_2000_2017.RDS")
flow_obs <- data.frame(Date=flow$Date,flow=flow[,match(Station,colnames(flow))])

# ET data
ET_obs <- readRDS("PSO.in/ET_obs_Cotter.RDS")
ET_obs_wide <- spread(ET_obs,key=Point,value=ET,drop=FALSE)

# combine
obs <- create_obs_data(flow_obs, ET_obs_wide,"2009-01-01", "2012-12-31")

# Now test putting in weights relative to the size of the subcatchment
subbasin_data <- read.csv("PSO.in/subbasins_Cotter_alldata.csv")

# calculate weights from relative areas
f_w <- 0.1 # flow weight
ET_w <- subbasin_data$Area/sum(subbasin_data$Area)*(1-f_w)
w_in <- c(f_w, ET_w)


############### Testing whether hydromod works ########################


### model FUN.args.txt--------------------------####

model.FUN.args=list(
  model.drty=model.drty,
  param.files=paste(model.drty,"/PSO.in/ParamFiles.txt",sep=""),
  exe.fname= "swat.exe",
  stdout= "",
  out.FUN = "Out_extract",
  verbose= TRUE,
  obs= obs,
  ###Function for reading the simulated equivalents                  
  out.FUN.args = list(file_sub = "output.sub",
    file_rch = "output.rch",
    sb = 1,
    d.ini = "2009-01-01",
    d.end = "2012-12-31",
    FLOW = TRUE,
    what = "ET",
    ALL = TRUE
  ), ###END out.FUN.args                         
  ###Function assessing the simulated equivalents against the observations                  
  gof.FUN= "KGE_w",
  gof.FUN.args = list
  (
    weights = w_in,
    method = "2012",
    out.type = "single",
    combine=TRUE
  )
  )

### Running the final algorithm ---------------------------------------------###

# Number of cores/nodes desired
par.nnodes <- 2
# packages that have to be loaded in each core or node of the network cluster
par.pkgs   <- c("hydroGOF", "zoo", "data.table", "tidyverse") 


### MAIN PSO ALGORITHM
### For hydroPSO (>=0.3-0) fine-tuning parameters, see Zambrano-Bigiarini and Rojas, 2013

set.seed(100)
hydroPSO(
  fn="hydromod",
  method = "spso2007",
  #model.FUN="hydromod",
  #default method set to "spso2011"
  model.FUN.args=model.FUN.args,
  model.FUN = "hydromod",
  control=list(
    drty.in = paste(model.drty,"/PSO.in/", sep = ""),
    drty.out = paste(model.drty,"/PSO.out/", sep = ""),
    param.ranges = "ParamRanges.txt",
    normalise=TRUE,
    MinMax="max",
    npart=2, # Just for testing if the optimisation runs correctly
    maxit=5, # Just for testing if the optimisation runs correctly
    reltol=1E-30, # Just to ensure 'maxit' is achieved
    Xini.type="lhs",
    Vini.type="lhs2011",
    lambda=1,
    c1=2.05,
    c2=2.05,
    use.IW=FALSE,
    use.CF=TRUE,   
    use.TVlambda=TRUE,TVlambda.type="linear",TVlambda.rng=c(1.0,0.5),TVlambda.exp=1,
    topology="random",K=1,
    boundary.wall="absorbing2011",
    write2disk=TRUE,
    REPORT=1,
    verbose=TRUE,
                parallel = "parallelWin",
                par.nnodes = par.nnodes,
                par.pkgs = par.pkgs
  ) ###END control options
)




