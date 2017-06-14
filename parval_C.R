
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
##################################

### function for file preperation
# added the option to specify the directory
file.prep<- function(parlist,list,txtinoutdir){
  f.ext<- data.frame(first =sapply(strsplit(parlist,"[.]"),"[",1))
  f.ext$last <-sapply(strsplit(parlist,"[.]"),"[",2)
  df <- data.frame(par = unique(f.ext[,1]))
  df$num <- seq(1,nrow(df),1)
  
  for ( i in 1:length(f.ext$first)){
    f.ext$num[i]<- df$num[which(df$par == f.ext$first[i])]
  }
  f.name <- function(type,serial,name,rnum,colst,coled,dpla){
    filename <- data.frame(filename = list.files(path = txtinoutdir, 
                                                 pattern = type),
                           ser =rep(serial),parname = rep(name),
                           Rownum = rep(rnum),colstart= rep(colst),
                           colend=rep(coled),decimal = rep(dpla))
    return(filename)
  }
  try <- vector()
  #browser()
  for( i in 1: length(f.ext$last)){
    try <- rbind(try,f.name(f.ext[i,2],f.ext[i,3],f.ext[i,1],
                            list[i,1],list[i,2],list[i,3],list[i,4]))
  }
  #browser()
  toMatch <- c("output","dat")
  n1 <- grep(toMatch[1],try[,1])
  # if statements included in case there are no "output" or "dat"
  if (length(n1) > 0) p <- as.data.frame(try[-n1,]) else p <- try
  n2 <- grep(toMatch[2],p[,1])
  if (length(n2) > 0) p1 <- as.data.frame(p[-n2,]) else p1 <- p
  return (p1)
  
}

### change your working dir to SWAT TxtInOut of your model

## Sorting the list of parameter that need to be included
#parlist<-sort(c("CN2.mgt","SOL_BD.sol","SOL_BD.sol","SOL_AWC.sol","SOL_AWC.sol","SOL_AWC.sol"))
# this now reads the file that has the list from Chapter 1 from SWAT Manual
Inparlist <- read.csv("../Inputdata/SWATParamFileExtensions.csv")
# Changed this so it can work from the master parameter file and extension list
Masterparlist <- paste(Inparlist[,1],Inparlist[,2],sep="")



###########################################################################

# preparing ParameterRanges file
f <- read.table("../Inputdata/parfile_alt.txt", header = TRUE)
pm.file <- f[!duplicated(f$Parametername),]
pm.file <- pm.file[with(pm.file,order(Parametername)),] # ordering the data to work with the function
pm.file <- data.frame(ParameterNmbr = c(1:nrow(pm.file)),Parametername = pm.file$Parametername, MinValue = pm.file$MinValue,
                      Maxvalue = pm.file$MaxValue)  # Just formating the table
write.table(pm.file, "../PSO_Cotter/PSO.in/ParamRanges.txt", sep = "\t",quote = FALSE, row.names = FALSE)


###########################################################################
# processing the parameter name in file
f <- f[with(f,order(Parametername)),]
f1 <- f[,c(2:5)]
# Changed this so it can work from the master parameter file and extension list
parlist <- Masterparlist[pmatch(f[,1],Inparlist[,1],duplicates=T)]

# test
# length(parlist) == nrow(f1)
###########################################################################

## Running the function

test <- file.prep(sort(parlist),f1,txtinoutdir = "../PSO_Cotter")
# reorganise columns
test <- test[,c(2,3,1,4:7)]
colnames(test)<- c("ParameterNmbr", "ParameterName","Filename","Row.Number", "Col.Start", "Col.End", "DecimalPlaces")



# Now write the file
write.table(test, "../Inputdata/ParamFiles.txt", sep = "\t",quote = FALSE, row.names = FALSE)
