
## READING SWAT OUTPUT.HRU FILES IN R
##http://santiago.begueria.es/2013/10/reading-swat-output-hru-files-in-r/ 
#Here is the function 
#need to verify name of the column ArcSwat/QSwat => ok done
# use read_fwf() in tidyverse as this is faster.
require(tidyverse)

swat_readOutputhru <- function(file,col=NULL,hru=NULL,year=NULL,lulc=NULL,ver=2012) {
  # year is only needed for monthly and annual output
  # Output.hru does not list years for daily output!
  # format of the .hru file (SWAT 2012)
  fmt=list(var=c('LULC','HRU','GIS','SUB','MGT','MON','AREA','PRECIP','SNOFALL','SNOMELT','IRR',
                 'PET','ET','SW_INIT','SW_END','PERC','GW_RCHG','DA_RCHG','REVAP','SA_IRR','DA_IRR','SA_ST',
                 'DA_ST','SURQ_GEN','SURQ_CNT','TLOSS','LATQ','GW_Q','WYLD','DAILYCN','TMP_AV','TMP_MX',
                 'TMP_MN','SOL_TMP','SOLAR','SYLD','USLE','N_APP','P_APP','NAUTO','PAUTO','NGRZ','PGRZ',
                 'NCFRT','PCFRT','NRAIN','NFIX','F-MN','A-MN','A-SN','F-MP','AO-LP','L-AP','A-SP','DNIT',
                 'NUP','PUP','ORGN','ORGP','SEDP','NSURQ','NLATQ','NO3L','NO3GW','SOLP','P_GW','W_STRS',
                 'TMP_STRS','N_STRS','P_STRS','BIOM','LAI','YLD','BACTP','BACTLP',
                 'WTAB','SOL','SNO','CMUP','CMTOT','QTILE','TNO3','LNO3','GW_Q_D','LATQCNT','TVAP'),
           col=c(4,5,10,5,5,5,rep(10,79)),
           c_col=cumsum(c(1,4,5,10,5,5,5,rep(10,78))))
  if (ver==2009) {
    fmt$var <- fmt$var[1:80]
    fmt$col <- fmt$col[1:80]
    fmt$c_col <- fmt$c_col[1:80]
  }
  if (ver==2005) {
    fmt$var <- fmt$var[1:75]
    fmt$col <- fmt$col[1:75]
    fmt$c_col <- fmt$c_col[1:75]
  }
  if (class(w)=='numeric') {
    col <- fmt$var[w]
  }
  
  # select columns
  if (!is.null(col)) {
    if (!('MON' %in% col)) {
      col <- c('MON',col)
    }
    if (!('LULC' %in% col)) {
      col <- c('LULC',col)
    }
    if (!('HRU' %in% col)) {
      col <- c('HRU',col)
    }
    w <- fmt$var %in% col
    fmt$var <- fmt$var[w]
    ending <- fmt$c_col[w] + fmt$col[w]-1
    starting <- fmt$c_col[w]
  }
  #browser()
  # read file, rearrange table
  res <- read_fwf(file,fwf_positions(start=starting,
                                     end=ending,
                                     col_names=fmt$var),
                  skip=9)
  res <- res[order(res$HRU),]
  
  # select hrus by number or by lulc
  if (!is.null(hru)) {
    res <- res[res$HRU>=min(hru) & res$HRU<=max(hru),]
  }
  if (!is.null(lulc)) {
    res <- res[res$LULC==lulc,]
  }
  
  # monthly and annual tables
  # This probably only works if you actually have monthly and annual output
  # need to check first, do you have daily output?
  # max(MON) is 366 OR 365
  # browser()
  if (max(res$MON,na.rm=TRUE)==366 | max(res$MON,na.rm=TRUE)==365) {
    mon <- NULL
    anu <- NULL
  } else {
    mon <- res[res$MON<=12,]
    anu <- na.omit(res[res$MON>12,])
    colnames(anu) <- sub('MON','YEA',colnames(anu))
    w <- which(mon$HRU==mon$HRU[1] & mon$MON==mon$MON[1])
    ww <- c((w-1)[-1],nrow(mon))
    years <- min(anu$YEA):max(anu$YEA)
    mon$YEA <- NA
    for (i in 1:length(w)) {
      mon[w[i]:ww[i],][,'YEA'] <- years[i]
    }
    
    # select years
    if (!is.null(year)) {
      mon <- mon[mon$YEA>=min(year) & mon$YEA<=max(year),]
      anu <- anu[anu$YEA>=min(year) & anu$YEA<=max(year),]
    }
    
    # rearrange
    rownames(mon) <- rownames(anu) <- NULL
    w <- which(colnames(mon)=='MON')
    ww <- which(colnames(mon)=='YEA')
    mon <- mon[,c(colnames(mon)[c(1:w)],'YEA',colnames(mon)[-c(1:w,ww)])]
  } # end if else daily data
  # go
  return(list(res=res,mon=mon,anu=anu))
}


#examples

#setwd("C:/SWAT project/SantaLucia")
setwd("c:/users/rver4657/owncloud/southsouthnetwork/resources/uruguayswat")
#file <- './Scenarios/Sim1-2000-2016/TxtInOut/output.hru'
file <- './santaluciaswat/Scenarios/default/TxtInOut/output.hru'


w <- c('PRECIP','SNOFALL','SNOMELT','IRR','ET',
       'SW_END','SA_ST','DA_ST','WYLD','REVAP')
# the following also works:
# w <- c(1,2,6,8,9,10,11,13,15,22,23,25,26,27,28,29)

# read the whole file (daily data, ARCSWAT output)
r <- swat_readOutputhru(file,col=w)

# read just one hru (daily data)
r <- swat_readOutputhru(file,col=w,hru=5)

# you will have to manually put in the years

plot(r$res$ET, type="l")
r$mon
r$anu

## read in the monthly data (QSWAT output from Flora)
# I have taken out the 13.0 rows, which are the summary monthly rows
fileQ <- "output_QSWAT.hru"

r <- swat_readOutputhru(fileQ,col=w)
head(r$mon)
r$anu
