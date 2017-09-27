
#########################################################################################################################
## This R script for creating ParamFiles.txt and ParamRanges-sens.txt files for the Latin Hypercube one-at-time anaysis #
## Script created by Dipangkar Kundu  
## kdip9946@uni.sydney.edu.au
# # adjusted Willem Vervoort willem.vervoort@sydney.edu.au 
#########################################################################################################################
# update: 2014-01-29 09:27:49 EST
#         2014-01-31 14:09:30 EST 
#         2014-02-10 09:08:29 EST
#         2017-06-09 15:07:26 AEST
#         2017-08-04 00:55:27 AEST To include also soil and landuse splits
##################################

################################
# Auxillary functions
###############################

# -----------------------------------------
# Function to create the parameter list
# Changed this so it can work from the master parameter file and extension list
# if a column "Split" exists
# need to strip landuse or soils from Parameter names in f[,1]
create_parlist <- function(foo) {

  if (any(grepl("Split",colnames(foo)))==T) {
    p <- nchar(foo[,1])
    r <- nchar(foo[,8])
    basename <- ifelse(foo[,8]=="ALL",as.character(foo[,1]),
                       substr(as.character(foo[,1]),1,(p-(r+1))))
    #basename
    
    # Match parameters with extensions
    parlist <- Masterparlist[pmatch(basename,Inparlist[,1],duplicates=T)]
    # add landuses to parlist
    parlist <- data.frame(parlist,foo$Split)
    p <- (foo$Split != "ALL")
    # grab the extenstions
    ext <- sapply(strsplit(as.character(parlist[p,1]),
                           "[.]"),"[",2)
    ext_name <- paste(basename[p],"_",foo$Split[p],".",ext,sep="")
    levels(parlist[,1]) <- c(levels(parlist[,1]),ext_name)
    parlist[p,1] <- ext_name
  } else {
    parlist <- Masterparlist[pmatch(foo[,1],Inparlist[,1],duplicates=T)]
  }
  return(parlist)  
}
# ---------------------------------

# ------------------------------------------------------------------
# function to remove redundant files if landuse and soil specific
# parameters exist
split_fun <- function(try,parlist_in,split) {
  # select only the specific landuses and soils
  parlist_sp <- parlist_in[parlist_in[,2] != "ALL",]
  for (i in 1:nrow(parlist_sp)) {
    # catch both landuse and soil
    # LU
    table_LU <- split[split$LU == 
                        as.character(parlist_sp[i,2]),]
#    browser()
    # soil
    table_soil <- split[split$Soil == 
                          as.character(parlist_sp[i,2]),]
    par_base <- strsplit(as.character(parlist_sp[i,1]),"[.]")[[1]][1]
    #browser()
    # remove redundant soil files for each soil based par
    if (nrow(table_soil) > 0) {
      sol_files <- paste(substr(table_soil$files,1,10),"sol",sep="")
      q <-  !(as.character(try$filename) %in% sol_files) &
        as.character(try$parname) == strsplit(as.character(parlist_sp[i,1]),"[.]")[[1]][1]
      try <- try[!q,]
    }
    # remove redundant mgt and hru files for each LU based par
    if (nrow(table_LU) > 0) {
      if (strsplit(as.character(parlist_sp[i,1]),"[.]")[[1]][2] =="mgt") {
        p <-  !(as.character(try$filename) %in% as.character(table_LU$files)) &
        as.character(try$parname) == strsplit(as.character(parlist_sp[i,1]),"[.]")[[1]][1]
        try <- try[!p,]
      }
      # also catch hru files for landuses
      if (strsplit(as.character(parlist_sp[i,1]),"[.]")[[1]][2] =="hru") {
        hru_files <- paste(substr(table_LU$files,1,10),"hru",sep="")
        q <- !(as.character(try$filename) %in% hru_files) &
          as.character(try$parname) == strsplit(as.character(parlist_sp[i,1]),"[.]")[[1]][1]
        try <- try[!q,]
        
      }
    }
  }
  return(try)
}
#--------------------------------------

## -----------------
# Function to write all filenames to try vector
f.name <- function(type,serial,name,rnum,
                   colst,coled,dpla, 
                   in_dir=txtinoutdir){
  filename <- data.frame(filename = list.files(path = in_dir, 
                                               pattern = type),
                         ser =serial,parname = name,
                         Rownum = rnum,colstart= colst,
                         colend=coled,decimal = dpla)
  return(filename)
}
#-----------------------------------------------

# -------------------------------------------------------
### Overall function for file preparation
# added the option to specify the directory
# added option to have split landuse and soil parameters
file.prep<- function(parlist_in,list,split = NULL, txtinoutdir){
  # split is a dataframe with filenames of mgt files and columns of 
  # associated landuse and soil, colnames should be:
  # c(files, LU, Soil)
  if (length(split)>0) colnames(split) <- c("files","LU","Soil")
  #browser()
  f.ext<- data.frame(first =sapply(strsplit(as.character(parlist_in[,1]),
                                            "[.]"),"[",1))
  f.ext$last <-sapply(strsplit(as.character(parlist[,1]),"[.]"),"[",2)

  df <- data.frame(par = f.ext[,1])
  df$num <- seq(1,nrow(df),1)
  
  for ( i in 1:length(f.ext$first)){
    f.ext$num[i] <- df$num[which(df$par == f.ext$first[i])[1]]
  }

  try <- vector()
  #browser()
  for( i in 1: length(f.ext$last)){
    try <- rbind(try,f.name(f.ext[i,2],f.ext[i,3],f.ext[i,1],
                            list[i,1],list[i,2],list[i,3],list[i,4],
                            in_dir = txtinoutdir))
  }
  # if there are separate soil and landuse based parameters
  if(length(split)>0) {
    # remove redunddant files out of try
    # only keep files with specific LU or soil
    out <- split_fun(try,parlist_in,split)
    
  }
  #browser()
  # remove output or dat files that might have been read in by accident
  toMatch <- c("output","dat")
  n1 <- grep(toMatch[1],out[,1])
  # if statements included in case there are no "output" or "dat"
  if (length(n1) > 0) p <- as.data.frame(out[-n1,]) else p <- out
  n2 <- grep(toMatch[2],p[,1])
  if (length(n2) > 0) p1 <- as.data.frame(p[-n2,]) else p1 <- p
  return (p1)
  
}
#-----------------------------------------------------


#####################################################################
# ## testing
# # change your working dir to SWAT TxtInOut of your model
# #setwd("R:/PRJ-HPWC/SWAT_ETCalibration/PSO_Goodra")
# setwd("C:/users/rver4657/owncloud/working/pso_Goodra")
# 
# # Reads the file based on the list of fileextensions in Chapter 1 from SWAT Manual
# Inparlist <- read.csv("../Inputdata/SWATParamFileExtensions.csv")
# # Changed this so it can work from the master parameter file and extension list
# Masterparlist <- paste(Inparlist[,1],Inparlist[,2],sep="")
# 
# 
# 
# ###########################################################################
# ## Testing Goodradigbee example
# 
# # if you have parameters separated by landuse or soil you
# # need to first have a list of file names and the associated landuse and soil
# # For this example, I used mgtfileLUandSoil.R to generate a list
# 
# # read in the dataframe with filenames, landuse and soil
# LU_soil <- read.table("PSO.in/LUSoilmgtfiles.txt", header=T)
# 
# # writing ParameterRanges file
# f <- read.table("../Inputdata/parameterfiles/parfile_goodra.txt",
#                 header = TRUE, colClasses=c("character",
#                                             rep("numeric",6),
#                                             "character"))
# pm.file <- f[!duplicated(f$Parametername),]
# pm.file <- pm.file[with(pm.file,order(Parametername)),] # ordering the data to work with the function
# pm.file <- data.frame(ParameterNmbr = c(1:nrow(pm.file)),Parametername = pm.file$Parametername, MinValue = pm.file$MinValue,
#                       Maxvalue = pm.file$MaxValue)  # Just formating the table
# write.table(pm.file, "PSO.in/ParamRanges.txt", sep = "\t",quote = FALSE, row.names = FALSE)
# 
# 
# ###########################################################################
# # processing the parameter name in file
# f <- f[with(f,order(Parametername)),]
# # separate out the relevant columns with locations in files
# f1 <- f[,c(2:5)]
# 
# parlist <- create_parlist(f)
# # test
# #nrow(parlist) == nrow(f1)
# 
# ###########################################################################
# 
# ## Running the function
# # This takes some time as there are many files
# test <- file.prep(parlist,f1,split=LU_soil,txtinoutdir = getwd())
# # reorganise columns
# test <- test[,c(2,3,1,4:7)]
# colnames(test)<- c("ParameterNmbr", "ParameterName","Filename","Row.Number", "Col.Start", "Col.End", "DecimalPlaces")
# 
# 
# 
# # Now write the file
# write.table(test, "../Inputdata/ParamFiles.txt", sep = "\t",quote = FALSE, row.names = FALSE)
