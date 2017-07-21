## Loading the required packages

require(dplyr)
require(data.table)
require(hydroGOF)
require(hydroPSO)
require(hydroTSM)
#require(doMC)
#require(parallel)
require(doParallel)
require(zoo)
require(lhs)


### Defining the model input and output -------------------------------------###
## PSO.in folder should be into the model folder
## PSO.out folder should be into the model folder
## Scrips should be in the scripts folder
### ------------------------------------------------------------------------ ###
## Specifying the model dir
#model.drty <- "/project/RDS-FAE-HPWC-RW/PSO_Cotter"
#model.drty <- "R:/PRJ-HPWC/SWAT_ETCalibration/PSO_Goodra"
model.drty <- "C:/Users/rver4657/Documents/PSO_Cotter"
setwd(model.drty)




### Loading the scripts to extract modelled values---------------------------###

#source(paste(model.drty,"/Scripts/Hydromod_PSO.R", sep = ""))
source(paste(model.drty,"/Scripts/RCH_extract.R", sep = ""))
#source(paste(model.drty,"/Scripts/read_paramfile.R", sep = ""))
#source(paste(model.drty,"/Scripts/ParameterValues2InputFiles.R", sep = ""))
#source(paste(model.drty,"/Scripts/ModifyInputFile.R",sep = ""))
#source(paste(model.drty,"/Scripts/hydromodel.R", sep = ""))


### Reading the observed values
Station <- "Flow_cumec_Gingira_410730"
t_obs <- readRDS("PSO.in/Discharge_data_2000_2017.RDS")
t_obs <- zoo(t_obs[,2:3],order.by=as.Date(t_obs$Date))
obs <- t_obs[,match(Station,colnames(t_obs))]
obs  <- window(obs,start=as.Date("2003-01-01"), end=as.Date("2007-12-31"))


# Setting the model argument function

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
    d.ini = "2003-01-01",
    d.end = "2007-12-31"
  ), ###END out.FUN.args                         
  ###Function assessing the simulated equivalents against the observations                  
  gof.FUN= "KGE",
  gof.FUN.args = list(
    method="2012",
    out.type="single"
  )
  )

### Running the final algorithm ---------------------------------------------###

# Number of cores/nodes desired
needed_nodes <- 1
# packages that have to be loaded in each core or node of the network cluster
needed_pkgs   <- c("hydroGOF", "hydroTSM", "data.table", "dplyr") 


### MAIN PSO ALGORITHM
### For hydroPSO (>=0.3-0) fine-tuning parameters, see Zambrano-Bigiarini and Rojas, 2013
#set.seed(100)
hydroPSO(
  fn="hydromod",
  method = "spso2011",
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
    npart=3, # Just for testing if the optimisation runs correctly
    maxit=15, # Just for testing if the optimisation runs correctly
    reltol=1E-30, # Just to ensure 'maxit' is achieved
    Xini.type="lhs",
    Vini.type="lhs2011",
   # lambda=1,
    c1=2.05,
    c2=2.05,
    use.IW=TRUE,
    use.CF=FALSE,   
    use.TVlambda=TRUE,TVlambda.type="linear",TVlambda.rng=c(1.0,0.5),TVlambda.exp=1,
    topology="random",K=3,
    boundary.wall="absorbing2011",
    #use.RG = TRUE,
    REPORT=1,
    parallel = "none"#,
    #par.nnodes = needed_nodes,
    #par.pkgs = needed_pkgs
  ) ###END control options
)

print(warnings())




