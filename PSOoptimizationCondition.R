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
    npart=100,  # try npart = 40
    maxit=50,  # try maxit = 1000
    reltol=1E-30, 
    Xini.type="lhs",
    Vini.type="lhs2011",
    lambda=1, # This is used to control maximum particle velocity
    c1=2.05, # use the default value 0.5+ log(2)
    c2=2.05, # use the default value 0.5 + log(2)
    use.IW=FALSE,
    use.CF=TRUE,   # use as FALSE
    use.TVlambda=TRUE,TVlambda.type="linear",TVlambda.rng=c(1.0,0.5),TVlambda.exp=1,
    topology="random",K= 19,  # use the topology as "gbest" or "vonNeuman" before trying random
    # use K as default K = 3
    # use TVlambda.type="non-linear"
    boundary.wall="absorbing2011",
    use.RG = TRUE,
    write2disk=TRUE,
    REPORT=5,
    verbose= TRUE,
    parallel = "parallel",
    par.nnodes = par.nnodes,
    par.pkgs = par.pkgs
  ) ###END control options
)