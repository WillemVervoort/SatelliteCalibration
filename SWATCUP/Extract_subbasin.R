# Function to extract sub-basin data

extract.sub <- function(wd,sb=1){
  # wd is the working directory
  # sb is the subbasin
  # Loading required packages
  require(data.table)
  require(dplyr)

  # Reading the file
  df<- data.table::fread(paste(wd,"/output.sub",sep = ""),skip = 9,sep = "\n",
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
  
  # Clearing up the memory
  rm(df)
  
  # creating the data.frame and producing output.
  df <- tbl_df(data.frame(Subbasin, Area, Pcp, ET, PET, SWc, Surf.Q, GW.Q, Water.yield))
  df <- filter(df, Subbasin %in% sb)
  
  return(df)
  
  }