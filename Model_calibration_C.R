## Loading the required packages

require(dplyr)
require(data.table)
require(hydroGOF)
require(hydroPSO)
require(doMC)
require(parallel)
require(doParallel)
require(zoo)


### Defining the model input and output -------------------------------------###
## PSO.in folder should be into the model folder
## PSO.out folder should be into the model folder
## Scrips should be in the scripts folder
### ------------------------------------------------------------------------ ###
## Specifying the model dir
model.drty <- "/project/RDS-FAE-HPWC-RW/PSO_Cotter"
#model.drty <- "R:/PRJ-HPWC/SWAT_ETCalibration/PSO_Goodra"
setwd(model.drty)




### Loading the scripts to extract modelled values---------------------------###

source(paste(model.drty,"/Scripts/Hydromod_PSO.R", sep = ""))
source(paste(model.drty,"/Scripts/RCH_extract.R", sep = ""))


### Reading the observed values
Station <- "Flow_cumec_Gingira_410024"
t_obs <- readRDS("PSO.in/Discharge_data_2000_2017.RDS")
t_obs <- zoo(t_obs[,2:3],order.by=as.Date(t_obs$Date))
obs <- t_obs[,match(Station,colnames(t_obs))]
obs  <- window(obs,start=as.Date("2002-01-01"), end=as.Date("2006-12-31"))


# Setting the model argument function

model.FUN.args=list(
  model.drty=model.drty,
  param.files=paste(model.drty,"/PSO.in/ParamFiles.txt",sep=""),
  exe.fname= "./swat2012",
  out.FUN = "RCH_extract",
  stderr = FALSE,
  stdout = "",
  verbose= FALSE,
  obs= obs,
  ###Function for reading the simulated equivalents                  
  out.FUN.args = list(
    wd = model.drty,
    rch = 1,
    d.ini = "2002-01-01",
    d.end = "2006-12-31"
  ), ###END out.FUN.args                         
  ###Function assessing the simulated equivalents against the observations                  
  gof.FUN= "KGE",
  gof.FUN.args = list(
    method="2012",
    out.type="single"
  ),
  gof.Ini = "2002-01-01",
  gof.Fin = "2006-12-31"
  )

### Running the final algorithm ---------------------------------------------###

# Number of cores/nodes desired
needed_nodes <- 24
# packages that have to be loaded in each core or node of the network cluster
needed_pkgs   <- c("hydroGOF", "data.table", "dplyr", "zoo", "parallel") 


### MAIN PSO ALGORITHM
### For hydroPSO (>=0.3-0) fine-tuning parameters, see Zambrano-Bigiarini and Rojas, 2013
set.seed(100)
hydroPSO(
  par = data.frame(lower = par.ini[,1]),
  fn="hydromod",
  method = "spso2011",
  model.FUN.args=model.FUN.args,
  model.FUN = "hydromod",
  control=list(
    param.ranges = paste(model.drty,"/PSO.in/ParamRanges.txt",sep=""),
    normalise=TRUE,
    MinMax="max",
    npart=40, 
    maxit=500, 
    reltol=1E-30, # Just to ensure 'maxit' is achieved
    Xini.type="lhs",
    Vini.type="lhs2011",
    lambda=1,
    c1=2.05,
    c2=2.05,
    use.IW=FALSE,
    use.CF=TRUE,   
    use.TVlambda=TRUE,TVlambda.type="linear",TVlambda.rng=c(1.0,0.5),TVlambda.exp=1,
    topology="random",K= 19,
    boundary.wall="absorbing2011",
    use.RG = TRUE,
    write2disk=TRUE,
    REPORT=5,
    verbose= FALSE,
    parallel = "parallel",
    par.nnodes = needed_nodes,
    par.pkgs = needed_pkgs
  ) ###END control options
)

print(warnings())




