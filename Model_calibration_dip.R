## Loading the required packages

require(dplyr)
require(data.table)
require(hydroGOF)
require(hydroPSO)
require(hydroTSM)
require(doParallel)


### Defining the model input and output -------------------------------------###
## PSO.in folder should be into the model folder
## PSO.out folder should be into the model folder
## Scrips should be in the scripts folder
### ------------------------------------------------------------------------ ###
## Specifying the model dir
model.drty <- "R:/PRJ-HPWC/SWAT_ETCalibration/PSO_Goodra"
setwd(model.drty)




### Loading the scripts to extract modelled values---------------------------###

source(paste(model.drty,"/Scripts/RCH_extract.R", sep = ""))
source(paste(model.drty,"/Scripts/read_paramfile.R", sep = ""))
source(paste(model.drty,"/Scripts/ParameterValues2InputFiles.R", sep = ""))
source(paste(model.drty,"/Scripts/ModifyInputFile.R",sep = ""))
source(paste(model.drty,"/Scripts/hydromodel.R", sep = ""))

### Reading the observed values
Station <- "Flow_cumec_Weejasper_410024"
t_obs <- readRDS("PSO.in/Discharge_data_2000_2017.RDS")
t_obs <- zoo(t_obs[,2:3],order.by=as.Date(t_obs$Date))
obs <- t_obs[,match(Station,colnames(t_obs))]
obs  <- window(obs,start=as.Date("2002-01-01"), end=as.Date("2006-12-31"))


############### Testing whether hydromod works ########################

###--------- Alternative settings--------------###---------------------

# out.FUN.args=list(
#   file="output.rch",
#   col.names="FLOW_OUTcms",
#   out.type="Q",
#   rchID=89,
#   Date.Ini="2003-01-01",
#   Date.Fin="2012-12-31",
#   tstep="daily",
#   verbose=FALSE
# )
# 
# ### Function for assessing the simulated equivalents against the observations
# param.values <- rep(0.38,30)
# 
# out <- hydromod(
#   param.values = param.values,
#   model.drty = model.drty,
#   param.files = paste(model.drty,"/PSO.in/ParamFiles.txt",sep=""),
#   exe.fname = "swat.exe",
#   out.FUN="read_rch",
#   out.FUN.args = out.FUN.args,
#   gof.FUN = "KGE",
#   gof.FUN.args = gof.FUN.args,
#   stdout = "", 
#   stderr = "" ,
#   obs = obs,
#   verbose = TRUE
# )

###-------- End of testing------------------------------------------------####

# model.FUN.args=list(
#   # Listing the arguments all together
#   model.drty = model.drty,
#   param.files = paste(model.drty,"/PSO.in/ParamFiles.txt",sep=""),
#   exe.fname = "swat.exe",
#   verbose = FALSE,
#   #stdout = "",
#   out.FUN = "read_rch",
#   obs = obs,
#   
#   out.FUN.args = list(
#     file="output.rch",
#     col.names="FLOW_OUTcms",
#     out.type="Q",                     
#     rchID=89,
#     Date.Ini= "2003-01-01",
#     Date.Fin= "2012-12-31",
#     tstep="daily"),
#   
#   gof.FUN = "KGE",
#   gof.FUN.args = list(
#     method = "2012",
#     out.type = "single"
#   )
#   # End of listing arguments
# )

### model FUN.args.txt[Dipangkar]--------------------------####

model.FUN.args=list(
  model.drty=model.drty,
  param.files=paste(model.drty,"/PSO.in/ParamFiles.txt",sep=""),
  exe.fname= "swat.exe",
  stdout= "",
  out.FUN = "RCH_extract",
  verbose= TRUE,
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
  gof.FUN.args = list
  (
    method = "2012",
    out.type = "single"
  )
)

### Running the final algorithm ---------------------------------------------###

# Number of cores/nodes desired
par.nnodes <- 2
# packages that have to be loaded in each core or node of the network cluster
par.pkgs   <- c("hydroGOF", "hydroTSM", "data.table", "dplyr", "doParallel") 


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
    npart=10, # Just for testing if the optimisation runs correctly
    maxit=15, # Just for testing if the optimisation runs correctly
    reltol=1E-30, # Just to ensure 'maxit' is achieved
    Xini.type="lhs",
    Vini.type="lhs2011",
    lambda=1,
    c1=2.05,
    c2=2.05,
    use.IW=FALSE,
    use.CF=TRUE,   
    use.TVlambda=TRUE,TVlambda.type="linear",TVlambda.rng=c(1.0,0.5),TVlambda.exp=1,
    topology="random",K=10,
    boundary.wall="absorbing2011",
    write2disk=TRUE,
    REPORT=1,
    verbose=TRUE,
    parallel = "parallelWin",
    par.nnodes = par.nnodes,
    par.pkgs = par.pkgs
  ) ###END control options
)




