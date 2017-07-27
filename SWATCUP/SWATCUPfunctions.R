# SWAT-CUP input preparation functions
# Willem Vervoort and Dipangkar Kundu
# version 2017-07-21
# source() this file to load functions
# for a demo see: CreatingSWATCUPobservedData.pdf

# read in modis data
MODIS_ts <- function(MODISdir="MODIS",patt=".asc"){
  
  # read in all the file names
  x1 <- list.files(path=MODISdir, pattern=patt)
  
  # each "asc" file stores all the values in time for 1 location
  # the number of rows is important as this is all the time steps
  # divide nrows by 2 as second part is QC data
  n <- nrow(read.csv(paste(MODISdir,x1[1],sep="/"),header=F))/2
  # Create storage for the data, Jdate is Julian date
  Store <- data.frame(Year = numeric(length=n),
                      JDay = numeric(length=n),
                      ET = numeric(length=n),
                      Point = numeric(length = n))
  
  # Create a list to store the different pixels (each with a Store)
  Store1 <- list()
  # run a loop over the list of file names
  for (i in 1:length(x1)) {
    Mdata <- read.csv(paste(MODISdir,x1[i],sep="/"),header=F)
    # do some substringing
    Store[,1] <- as.numeric(substr(Mdata[1:n,8],2,5))
    Store[,2] <- as.numeric(substr(Mdata[1:n,8],6,8))
    Store[,3] <- Mdata[1:n,11]/10 
    # 0.1 scaling factor (see MODIS read_me)
    Store[,4] <- i
    Store1[[i]] <- Store
    
  }
  # converting from list back to a data.frame
  ts.data <- do.call(rbind,Store1) 
  # Now make the date from the Year and Jdate
  ts.data$Date <- as.Date(paste(ts.data$Year,ts.data$JDay, 
                                sep = "/"), "%Y/%j")
  
  return(ts.data)
}


# auxillary functions
# 1. readfun
readfun <- function(filename, n=n) {
  foo <- file(filename,"r+")
  foo_bar <- readLines(foo, n = n)
  close(foo)
  return(foo_bar)
}

# organiseFun and organiseFlow
organiseFun <- function(df_in, st.date, end.date) {
  # simple downscale the ET, can be improved
  difdays <- diff(df_in$JDay)
  difdays <- replace(difdays,difdays==-360,5)
  df_in$ET <- df_in$ET/c(5,difdays)
  # generate the SWAT_CUP id
  df_in$cup_ID <- paste("ET_", df_in$Point[1], "_", 
                        df_in$JDay, "_", df_in$Year, sep = "")
  # generate the full date series
  full_dates <- seq.Date(as.Date(st.date), as.Date(end.date), by = 1)
  full_dates <- data.frame(n = 1:length(full_dates), Dates = full_dates)
  # subsetting the data
  df_in2 <- subset(df_in, Date >= as.Date(st.date) & Date <= as.Date(end.date))
  # match full_dates with df_in2
  serial <- full_dates[full_dates[,2] %in% df_in2$Date,]
  
  # re arranging the dataframe
  df_out <- data.frame(Serial = serial$n,
                       cup_ID = df_in2$cup_ID, ET = round(df_in2$ET,3))
  return(df_out)
}

organiseFlow <- function(df_in, st.date, end.date) {
  
  df_in$JDay <- format.Date(df_in$Date, "%j")
  df_in$Year <- format.Date(df_in$Date, "%Y")
  df_in$Date <- as.Date(df_in$Date)
  df_in$cup_ID <- paste("FLOW_OUT_",df_in$JDay,"_",df_in$Year, sep = "")
  
  # subsetting the data
  df.sub <- subset(df_in, Date >= as.Date(st.date) & 
                     Date <= as.Date(end.date))
  df.sub$Serial <- 1:nrow(df.sub)
  # Removing the NA values as SWAT-CUP does not deal with NA values
  df.sub <- na.omit(df.sub)
  
  # re arranging the dataframe
  df.sub <- data.frame(Serial = df.sub$Serial,
                       cup_ID = df.sub$cup_ID, Flow = df.sub[,2])
  return(df.sub)
}

# write fun
writeFun <- function(outfile, df_write, header = header, 
                     Flow = FALSE,
                     np = NULL, i = NULL, weight = 0.5) {
  browser()
  p <- grep("subbasin number",header)
  r <- grep("of data points", header, ignore.case=T)
  
  # writing the rch file
  if (regexpr("observed_rch.txt",outfile, fixed = T)[[1]]>0) {
    header[p] <- paste("FLOW_OUT_1","     ", 
                       substr(header[p], 11, nchar(header[p])),sep="")
    header[r] <- paste(nrow(df_write),substr(header[r], 6,
                                             nchar(header[r])),
                       sep = "   ")
  }
  
  # writing the observed.txt file
  if (regexpr("observed.txt",outfile, fixed=T)[[1]]>0 & Flow == TRUE) {
    header[p] <- paste("FLOW_OUT_1", "    ", 
                       substr(header[p], 11, nchar(header[p])),sep="")
    header[p + 1] <- paste(ifelse(length(weight) > 1,weight[1],weight),
                           "      ",substr(header[p + 1], 7, 
                                         nchar(header[p + 1])),sep="")
    header[r] <- paste(nrow(df_write),substr(header[r], 6,
                                             nchar(header[r])),
                       sep = "   ")
  } else {
    if (regexpr("observed.txt",outfile, fixed=T)[[1]]>0 & Flow == FALSE) {
      if (length(weight) > 1) w <- weight[i+1] else w <- (1-weight)/np
      header[p] <- paste("ET_", i,"     ", 
                         substr(header[p], 11, nchar(header[p])),sep="")
      header[p + 1] <- paste(round(w,4), "    ", 
                             substr(header[p + 1], 9, 
                                    nchar(header[p + 1])),
                             sep="   ")
      header[r] <- paste(nrow(df_write),substr(header[r], 6,
                                               nchar(header[r])),
                         sep = "     ")
    } 
  }
  # writing observed_sub.txt
  if (regexpr("observed_sub.txt",outfile, fixed=T)[[1]]>0) {
    header[p] <- paste("ET_", i, "    ", 
                       substr(header[p], 11, nchar(header[p])),sep="")
    header[r] <- paste(nrow(df_write),substr(header[r], 6,
                                             nchar(header[r])),
                       sep = "     ")
  }
  # write to a file, but with a header
  write(header[(p-1):(r+2)], file = outfile, append = T)
  write.table(df_write,file=outfile, sep = "   ",row.names = F, 
              col.names = F, quote = F,
              append = T)
}

## Final overall function
swatcup_ETformat <- function(df, df_flow = NULL, 
                             date.format = "%Y-%m-%d", 
                             st.date, end.date, 
                             outfile ,infile, nlines, 
                             Flow = FALSE, 
                             weight = 0.5){
  # colnames should be c("Year","JDay", "ET", "Point", "Date")
  # Formating to a date format
  df$Date <- as.Date(df$Date, format = date.format)
  # read in the header from the file
  header <- readfun(infile, nlines)
  # write the number of observed variables to the top
  header[1] <- paste(ifelse(Flow==FALSE ,
                            length(unique(df$Point)),
                            length(unique(df$Point)) + 1),
                     "     : number of observed variables")
  write(header[1:(grep("subbasin number",header) - 2)],
        file = outfile)
  # prepare the flow data
  if (Flow == TRUE) {
    # use organiseflow
    if (length(df_flow)>0) df_input <- df_flow else df_input <- df
    df_in2 <- organiseFlow(df_in = df_input,
                           st.date, end.date)
    # use writeFun
    writeFun(outfile = outfile,
             df_write = df_in2, header = header, Flow = Flow, 
             np = NULL,
             weight = weight)
  }
  
  # running a loop through the number of points
  if (length(df_flow) > 0 | Flow == FALSE) {
    for (i in 1:length(unique(df$Point))) {
      df_sub <- df[df$Point==i,]
      
      df_in2 <- organiseFun(df_sub, st.date, end.date)
      
      # write the data and header
      writeFun(outfile = outfile,
               df_write = df_in2, header = header, Flow = FALSE, 
               np = length(unique(df$Point)), i = i, 
               weight=weight)
    }
  }
}
