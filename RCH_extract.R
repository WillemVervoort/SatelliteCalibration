# Function to extract reach values in R:
RCH_extract <- function(filename, rch, d.ini, d.end, verbose=verbose){
  # loading the package required
  
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




