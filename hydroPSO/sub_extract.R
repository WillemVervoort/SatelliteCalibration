# Function to extract sub-basin data
# Loading required packages
if(require(data.table)==F) library(data.table)
if(require(tidyverse)==F) library(tidyverse)
if(require(zoo)==F) library(zoo)

SUB_extract <- function(filename, sb = 1,
                        d.ini, d.end, 
                        verbose=verbose,
                        ALL = FALSE) {
  # sb is the subbasin
  
  # Reading the file
  df<- data.table::fread(filename,skip = 9,sep = "\n",
                         header = F, data.table = F, verbose = F)

  # Extracting required data information
  Subbasin <- as.numeric(apply(df,2,substr,7,10))
  Area     <- as.numeric(apply(df,2,substr,25,34))
  Pcp      <- as.numeric(apply(df,2,substr,36,44))
  PET      <- as.numeric(apply(df,2,substr,56,64))
  ET       <- as.numeric(apply(df,2,substr,66,74))
  SWc      <- as.numeric(apply(df,2,substr,76,84))
  Surf.Q   <- as.numeric(apply(df,2,substr,96,104))
  GW.Q     <- as.numeric(apply(df,2,substr,106,114))
  Water.yield <- as.numeric(apply(df,2,substr,116,124))

  if (verbose) 
    message("[ Running SUB_extract     ]")
  if (verbose) 
    message("===========================================")
  
  # Clearing up the memory
  rm(df)
  
  # creating the data.frame and producing output.
  df <- tbl_df(data.frame(Subbasin, Area, Pcp, 
                          ET, PET, SWc, Surf.Q, 
                          GW.Q, Water.yield))
  if (ALL == FALSE) {
    df <- filter(df, Subbasin %in% sb)
  }

  return(as.data.frame(df))

}
  
RCH_extract <- function(filename, rch, d.ini, d.end, verbose=verbose){

  #read the file
  flow.mod <- data.table::fread(filename, skip = 9, header= F, sep = "\n",data.table = F, verbose = F)
  flow.mod <- data.table(
    RCH = as.numeric(apply(flow.mod,2,substr,6,18)),
    Flowout = as.numeric(apply(flow.mod,2,substr,50,61))
  )
  flow.mod <- dplyr::tbl_df(flow.mod)
  # Filter the data with the selected HRU
  sub.flow <- filter(flow.mod, RCH == rch)
  rm(flow.mod)
  if (verbose) 
    message("[ Running RCH_extract     ]")
  if (verbose) 
    message("===========================================")
  return(zoo(as.numeric(sub.flow$Flowout), seq(as.Date(d.ini), as.Date(d.end), "day")))
}


Out_extract <- function(file_sub = "output.sub", 
                        file_rch = NULL, sb = 1,
                        d.ini, d.end, 
                       FLOW=FALSE, what = "ET",
                       ALL = TRUE,
                       verbose=verbose){

  if (FLOW == TRUE) {
    flow_out <- RCH_extract(file_rch, rch=sb, d.ini = d.ini,
                            d.end = d.end, verbose = verbose)
  }
  
  sub_out <- SUB_extract(file_sub, sb=NULL, d.ini = d.ini,
                         d.end = d.end, verbose = verbose,
                         ALL = ALL)
  #browser()
  sub_out <- sub_out[order(sub_out$Subbasin),]
  sb_t <- length(unique(sub_out$Subbasin))
  foo <- list()
  for (i in 1:sb_t) {
    foo[[i]] <- sub_out[sub_out$Subbasin==i,what]
  }
  sub_out2 <- do.call(cbind,foo)
  # sub_out %>% select(Subbasin, what) %>% 
  #   group_by(Subbasin) %>%
  #   mutate(row = 1:n()) %>%
  #   spread(Subbasin, what)
  rm(foo)
  sub_z <- zoo(sub_out2, order.by = seq.Date(as.Date(d.ini), as.Date(d.end), "day"))
  
  if (what == "ET") {

    # sum ET to 8 days
    r.dates <- seq(as.Date(d.ini), as.Date(d.end), by=8)
    date.sum <- zoo(rep(r.dates, c(8,diff(r.dates))),
                    order.by= seq.Date(as.Date(d.ini),
                                       as.Date(d.end),
                                         by="day"))
    #browser()
    ET_out <- merge(sub_z,date.sum)
    ET_sum <- aggregate(ET_out,list(ETdate = date.sum), sum)
    ET_sum <- zoo(ET_sum[,1:sb_t],order.by=r.dates)
    # merge with flow data
    out <- merge(flow_out,ET_sum, all=T)
  } else {
    out <- merge(flow_out,sub_z)
  }
  return(out)
}

# testing
# # packages
# require(zoo)
# require(tidyverse)
# require(data.table)

# # setwd()
# setwd("R:/PRJ-HPWC/SWAT_ETCalibration/PSO_Cotter")
# 
# 
# test <- Out_extract(file_sub = "output.sub",
#                         file_rch = "output.rch", sb = 1,
#                         d.ini="2009-01-01", d.end="2012-12-31",
#                         FLOW=TRUE, what = "ET",
#                         ALL = TRUE,
#                         verbose=TRUE)


  
