### Function to run model,collect the simulated values and calculate the GOF
### based on the given GOF
### This is modified from the hydroPSO-hydromod function
###--------------------------------------------------------------------------###


hydromod <- function(param.values,param.files, 
                       exe.fname, 
                       model.drty = getwd(),
                       gof.FUN,
                       gof.FUN.args = list(),
                       stdout = FALSE, stderr = "" ,verbose = FALSE,
                       obs,
                       out.FUN,
                       out.FUN.args
                     )
{
      setwd(model.drty)
      gof.name <- gof.FUN
      if (verbose) 
            message("[ 1) Writing new parameter values ...     ]")
      if (verbose) 
            message("===========================================")
      ParameterValues2InputFiles(NewValues = param.values, ParamFiles.fname = param.files, 
                                 verbose = verbose)
      if (verbose) 
            message("===========================================")
      if (verbose) 
            message("[ 2) Running the model ...                ]")
      system2(exe.fname, stdout = stdout, stderr = stderr)
      if (verbose) 
            message("===========================================")
      if (verbose) 
            message("[ 3) Extracting simulated values ...      ]")
      out.FUN.argsDefaults <- formals(out.FUN)
      out.FUN.args <- modifyList(out.FUN.argsDefaults, out.FUN.args) 
      sim <- do.call(out.FUN, as.list(out.FUN.args))
      gof.FUN.args <- modifyList(gof.FUN.args, list(sim = sim))
      
      if (verbose) 
            message("[ 4) Computing the goodness of fit ...    ]")
      if (verbose) 
            message("===========================================")
      gof.FUN.argsDefaults <- formals(gof.FUN)
      gof.FUN.args <- modifyList(gof.FUN.argsDefaults, gof.FUN.args) 
      gof.FUN.args <- modifyList(gof.FUN.args, list(sim=sim, obs=obs))
      gof.value <- do.call(gof.FUN, as.list(gof.FUN.args))
      if (verbose) message("[", gof.name, "= ", round(gof.value,3), "]")
      
      out <- list(2)
      out[[1]] <- sim
      out[[2]] <- gof.value
      names(out) <- c("sim", "GoF")
      return(out)
}

# End of function