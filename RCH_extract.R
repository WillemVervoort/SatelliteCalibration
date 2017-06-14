# Function to extract reach values in R:

RCH_extract <- function(wd, rch, d.ini, d.end){
  # loading the package required
  
  #read the file
  flow.mod <- data.table::fread(paste(wd,"output.rch",sep="/"), skip = 9, header= F, sep = "\n",data.table = F, verbose = F)
  flow.mod <- data.table(
  RCH = as.numeric(apply(flow.mod,2,substr,6,18)),
  Flowout = as.numeric(apply(flow.mod,2,substr,50,61))
  )
  flow.mod <- dplyr::tbl_df(flow.mod)
  # Filter the data with the selected HRU
  sub.flow <- filter(flow.mod, RCH == rch)
  rm(flow.mod)
  return(zoo(as.numeric(sub.flow$Flowout), seq(as.Date(d.ini), as.Date(d.end), "day")))
}




