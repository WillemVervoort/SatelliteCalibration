# Objective functions for hydroPSO to match ET data and flow data
# "weigthed" versions of KGE and NSE
# Willem Vervoort
# Based on KGE and NSE in hydroGOF by Mauricio Zambrano-Bigiarini (2012)
# July 2017
# ---------------------------------------------------------------

require(hydroGOF)

KGE_w <- function(sim, obs, ...) UseMethod("KGE_w")

################################################################################
# Original Author : Mauricio Zambrano-Bigiarini                                #
# This version: Willem Vervoort
################################################################################
# 2017-07-27
################################################################################

KGE_w.matrix <- function(obs, sim, weights = NULL,
                         s=c(1,1,1), na.rm=TRUE, 
                         method=c("2009", "2012"), 
                         out.type=c("single", "full"),
                         combine = FALSE, ...) {
  
  # check if weights exist
  if (length(weights)==0)  {
    warning("No weights provided using KGE.matrix")
    KGE.matrix(obs=obs,pred=pred,s=s, na.rm=na.rm, 
               method=method, 
               out.type=out.type,...)
  } else {
    # use weights


    # Checking that 'sim' and 'obs' have the same dimensions
    if ( all.equal(dim(sim), dim(obs)) != TRUE )
      stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
                  paste(dim(sim), collapse=" "), "] != [", 
                  paste(dim(obs), collapse=" "), "] )", sep="") )
    
    # If the user provided a value for 's'
    if (!all.equal(s, c(1,1,1)) )  {
      if ( length(s) != 3 ) stop("Invalid argument: lenght(s) must be equal to 3 !")
      if ( sum(s) != 1 )    stop("Invalid argument: sum(s) must be equal to 1.0 !")
    } # IF end
    
    method   <- match.arg(method)
    out.type <- match.arg(out.type) 
    
    ifelse(method=="2012", vr.stg <- "Gamma", vr.stg <- "Alpha")
    
    KGE                <- rep(NA, ncol(obs))       
    elements           <- matrix(NA, nrow=3, ncol=ncol(obs))
    rownames(elements) <- c("r", "Beta", vr.stg)
    colnames(elements) <- colnames(obs)
    
    # recycle weights, this makes sure weights are same length as columns
    if (length(weights) < ncol(obs)) {
      warning("vector weights is < ncol(obs), recycling values")
    }
    weights_ext <- weights*(rep(1,ncol(obs)))
    
    
    if (out.type=="single") {
      out <- sapply(1:ncol(obs), function(i,x,y) { 
        KGE[i] <- weights[i]*KGE.default( x[,i], y[,i], s=s, na.rm=na.rm, method=method, out.type=out.type, ... )
      }, x=sim, y=obs ) 
      if (combine=TRUE)  {
        out <- sum(out) 
      } else {
        names(out) <- colnames(obs)  
      }
    } else { out <- lapply(1:ncol(obs), function(i,x,y) { 
        weights[i]*KGE.default( x[,i], y[,i], s=s, na.rm=na.rm, method=method, out.type=out.type, ... )
    }, x=sim, y=obs ) 
    for (i in 1:length(out) ) {
      KGE[i] <- out[[i]][[1]]
      elements[,i] <- as.numeric(out[[i]][[2]])
    } # FOR end
    if (combine=TRUE)  {
      KGE <- sum(KGE) 
    } 
    out <- list(KGE.value=KGE, KGE.elements=elements)
    } # ELSE end                     
    
    return(out)  
  }  
}


# testing
data1 <- matrix(rnorm(100),5,4)
data2 <- matrix(rnorm(100),5,4)
w <- c(0.1,0.3,0.2,0.4)

KGE_w(data1,data2,weights=w)


################################################################################
# Original Author : Mauricio Zambrano-Bigiarini                                #
# This version: Willem Vervoort
################################################################################
# 2017-07-27
################################################################################
KGE_w.data.frame <- function (sim, obs, weights = NULL, 
                            s=c(1,1,1), na.rm=TRUE, 
                            method=c("2009", "2012"), 
                            out.type=c("single", "full"),
                            combine=FALSE, ...){ 
  
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  method   <- match.arg(method)
  out.type <- match.arg(out.type) 
  
  KGE_w.matrix(sim, obs, weights=weights, s=s, na.rm=na.rm, method=method, out.type=out.type, combine = combine, ...)
  
} # 'KGE.data.frame' end


################################################################################
# Original author: Mauricio Zambrano-Bigiarini                                #
# Version with Weights: Willem Vervoort
################################################################################
# 2017-07-27                                                       #
################################################################################
KGE_w.zoo <- function(sim, obs, weights=NULL, 
                      s=c(1,1,1), na.rm=TRUE, 
                    method=c("2009", "2012"), 
                    out.type=c("single", "full"), 
                    combine = FALSE, ...){
  
  sim <- zoo::coredata(sim)
  if (is.zoo(obs)) obs <- zoo::coredata(obs)
  
  if (is.matrix(sim) | is.data.frame(sim)) {
    KGE_w.matrix(sim, obs, weight=weights, s=s, na.rm=na.rm, method=method, out.type=out.type, combine = combine, ...)
  } else NextMethod(sim, obs, weights=weights, s=s, na.rm=na.rm, method=method, out.type=out.type, combine = combine, ...)
  
} # 'KGE.zoo' end