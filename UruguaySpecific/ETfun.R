
## +++++++++++++++++++++++++++++++++++++++++++++++
#
#  ET function to calculate PM, PT and Hargreaves ET
# rewritten to take a zoo dataframe as inout

# Create a function to calculate and plot the different potential ET estimates
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ET.fun <- function(data, elev,lat, plotit=c(1,2,3), panE = F) {
  #browser()
  names(data) <- c("MaxT", "MinT", "MaxRelH", "MinRelH", 
                   "Solar.Rad", "Windspeed")
  # the zoo data contains 6 named columns:
  # MaxT = maximum daily temperature
  # MinT = minium relative humidity
  # MaxRelH = maximum relative humidty on the day
  # MinRelH = minimum relative humidity on the day
  # Solar.Rad = average solar radiation for the day MJ/m2
  # Windspeed = the average daily windspeed
  # lat is the latitude of the location in degrees,
  # needed to calculate the solar inclination in the PM equation
  # elev is the elevation of the weather station in m
  # plotit is a routine to plot the different components
  # panE identifies whether there is an optional column named PanE that needs to be plotted
  
  # set up plotting
  if (length(plotit)>=1 & is.numeric(plotit)==T) {
    numplot=plotit; 
    plotit=T 
  } else  {numplot <- c(1,2,3)}
  #print(plotit)
  
  #------------------------
  # skip over missing data
  #-----------------------
  bad <- is.na(data$Solar.Rad) 
  data[bad,"Solar.Rad"] <- 0
  
  #browser()
  # ---------------------
  # start preliminary calculations
  # ----------------------
  # T mean
  Tmean <- (data$MaxT + data$MinT)/2
  
  # delta v
  deltav <- (4098*(0.611*exp((17.27*Tmean)/(Tmean + 237.3)))/
               (Tmean+237.3)^2)
  
  # lambda V
  lambda.w <- 10^3*(2501-2.36*Tmean)
  
  # Pair
  Pair <- 101.3*((Tmean+273-0.0065*elev)/(Tmean+273))^5.256
  
  # gamma air
  gamma.air <- 0.00163*Pair/lambda.w
  
  #Cair
  Cair <- 622*gamma.air*lambda.w/Pair
  
  # esat
  esat <- (0.611*exp(17.27*data$MaxT/(data$MaxT + 237.3)) + 0.611*exp(17.27*data$MinT/(data$MinT + 237.3)))/2
  
  # eact
  eact <- (0.611*exp(17.27*data$MaxT/(data$MaxT + 237.3))*data$MaxRelH/100 +
             0.611*exp(17.27*data$MinT/(data$MinT + 237.3))*data$MinRelH/100)/2
  
  # VPD
  VPD <- esat - eact
  
  # rho.air
  rho.air <- 3.486E-3*Pair*(1-0.378*eact/Pair)/(Tmean + 273)
  
  # calculate dr omega, delta , Ra and Nrel
  # find the julian days
  J <- as.numeric(substr(format(as.Date(time(ETinput)),"%Y-%j"),6,8))
  dr <- 1 + 0.033*cos(2*pi/365*J)
  # solar declination
  delta <- 0.409*sin(2*pi/365*J-1.39)
  # sunset hour
  omega <- acos(-tan(pi*lat/180 )*tan(delta))
  # extra terrestrial radiation
  Ra <- 118.08/pi*dr*(omega*sin(pi*lat/180)*sin(delta)+cos(pi*lat/180)*cos(delta)*sin(omega))
  Nrel <- data$Solar.Rad/(0.5*Ra)-0.25
  
  #Rnl
  Rnl <- 4.903*10^-3*(((data$MaxT+273)^4+(data$MinT+273)^4)/2)*
    (0.34-0.14*sqrt(eact))*(0.1+0.9*Nrel)
  
  # Rn
  Rn <- (1-0.23)*data$Solar.Rad*10^6-Rnl
  
  # ----------------------------
  # Calculating ET
  # --------------------------
  
  # calculate Penman Ep
  data$PM.Ep <- (deltav/lambda.w*Rn + (86400*rho.air*Cair/lambda.w*(esat-eact)/(208/data$Windspeed)))/
    (deltav+gamma.air*(1 + 70/(208/data$Windspeed)))
  
  # Calculate Priestley Taylor Ep
  data$PT.Ep <- 1.26/lambda.w*deltav/(deltav+rho.air)*Rn
  
  
  # Calculate Hargreaves Ep:
  data$HG.Ep <-0.0023*1/lambda.w*Ra*10^6*(Tmean + 17.8)*sqrt(data$MaxT-data$MinT) 

  # put NA values back
  data[bad,"Solar.Rad"] <- NA
  data[bad,"PM.Ep"] <- NA
  data[bad,"PT.Ep"] <- NA
  data[bad,"HG.Ep"] <- NA
  
  
  # ---------------------------------
  # Plotting bit more complex, need to allow for different functions to be plotted
  # --------------------------------
#  browser()
  # create a dataframe with just the plotting data
  plotwhat <- cbind(data$PM.Ep,data$PT.Ep,data$HG.Ep)
  #print(head(plotwhat))
  # PM data
  if(plotit==T) {
      plot(as.Date(time(plotwhat)),plotwhat[,numplot[1]], pch=1, col="red",
             # put in axis labels
             xlab="Date",ylab="Potential evaporation (mm/day)",
             # make sure axis covers all data
             ylim=c(0,10))
        # PT data
        if (length(numplot)>1)
        {points(as.Date(time(plotwhat)),plotwhat[,numplot[2]] , pch=2, col="blue")}
        # HG data
        if(length(numplot)==3) {
          #  print(TRUE)
          points(as.Date(time(plotwhat)),plotwhat[,numplot[3]], pch=2, col="green")}
        if (panE == F) {
          # legend
          # define the legend text
          lgd.txt <- c( "Penman M", "Priestley T", "Hargreaves")
          # define the legend see ?legend. 
          # First part is "position" next is "text" next is line and point types etc.
          legend("top", lgd.txt[numplot], pch=c(1,2,2)[numplot], col=c("red", "blue","green")[1:length(numplot)])
        } else {
          # plot panE
          points(as.Date(time(plotwhat)),data$PanE, pch=3, col="purple")
      
          # legend
          # define the legend text
          lgd.txt <- c( "Penman M", "Priestley T", "Hargreaves","PanE")
          # define the legend see ?legend. 
          # First part is "position" next is "text" next is line and point types etc.
          legend("top", lgd.txt[c(numplot,4)], pch=c(1,2,2,3)[c(numplot,4)], col=c("red", "blue","green","purple")[c(1:length(numplot),4)])
        }
  }

  
  
  return(data)
} # end of function
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
