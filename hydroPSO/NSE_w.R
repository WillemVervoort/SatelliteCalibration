# ###################################################################
# "weighted" versions of KGE and NSE and the option to summarise the
# data to a single value (via "combine") so it can be used as a goF in
# hydroPSO for calibrating on different inputs
# Willem Vervoort willemvervoort@gmail.com
# Based on KGE and NSE in hydroGOF by Mauricio Zambrano-Bigiarini (2012)
# 
# ---------------------------------------------------------------

NSE_w <-function(sim, obs, ...) UseMethod("NSE_w")


NSE_w.matrix <- function (sim, obs, weights=NULL, 
                          na.rm=TRUE, FUN=NULL, 
                          epsilon=c(0, "Pushpalatha2012", "other"), 
                          epsilon.value=NA,
                          combine=FALSE, ...){ 
  # check if weights exist
  if (length(weights)==0)  {
    warning("No weights provided using KGE.matrix")
    NSE.matrix(obs=obs,pred=pred,s=s, na.rm=na.rm, 
               method=method, 
               out.type=out.type,...)
  } else {
    # use weights
    
    # Checking that 'sim' and 'obs' have the same dimensions
    if ( all.equal(dim(sim), dim(obs)) != TRUE )
      stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
                  paste(dim(sim), collapse=" "), "] != [", 
                  paste(dim(obs), collapse=" "), "] )", sep="") )
    
    # recycle weights, this makes sure weights are same length as columns
    if (length(weights) < ncol(obs)) {
      warning("vector weights is < ncol(obs), recycling values")
    }
    weights_ext <- weights*(rep(1,ncol(obs)))
    # check if weights sum to 1
    if (sum(weights_ext != 1)) {
      weights_ext <- weights_ext/sum(weights_ext)
    }
    
    NS <- rep(NA, ncol(obs))       
    
    NS <- sapply(1:ncol(obs), function(i,x,y) { 
      NS[i] <- weights_ext[i]*NSE.default( x[,i], y[,i], na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
    }, x=sim, y=obs )    
    if (combine=TRUE)  {
      NS <- sum(NS) 
    } else {
      names(NS) <- colnames(NS)  
    }
  }    
  return(NS)
  
} # 'NSE.matrix' end


NSE_w.data.frame <- function (sim, obs, weights=NULL,
                              na.rm=TRUE, FUN=NULL, 
                              epsilon=c(0, "Pushpalatha2012", "other"),
                              epsilon.value=NA,
                              combine=FALSE, ...){ 
  
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  NSE_w.matrix(sim, obs, weights=weights, na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, combine=combine, ...)
  
} # 'NSE_w.data.frame' end


################################################################################
# Original author: Mauricio Zambrano-Bigiarini                                #
# Version with Weights: Willem Vervoort
# includes weights and combine as additional inputs
################################################################################
# 2017-07-27                                                       #
################################################################################
NSE_w.zoo <- function(sim, obs, weights=weights,
                      na.rm=TRUE, FUN=NULL, 
                      epsilon=c(0, "Pushpalatha2012", "other"), 
                      epsilon.value=NA, 
                      combine=FALSE, ...){
  
  sim <- zoo::coredata(sim)
  if (is.zoo(obs)) obs <- zoo::coredata(obs)
  
  if (is.matrix(sim) | is.data.frame(sim)) {
    NSE_w.matrix(sim, obs, weights=weights, na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, combine=combine, ...)
  } else NextMethod(sim, obs, weights = weights, na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value,combine=combine, ...)
  
} # 'NSE_w.zoo' end

