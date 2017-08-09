# Calculating daily radiation from scratch
# willem vervoort 2017-08-03

Latitude <- 33.9519 # Athens GA
# dates from 1 Jan 2015 to today
Dates <- seq.date("2015-01-01", Sys.Date(),by=1)

# you also need daily maximum and minimum temperature
MinT <-
MaxT <- 

#Using FAO: http://www.fao.org/docrep/X0490E/x0490e07.htm#radiation

J <- as.numeric(format(Dates,"%Y-%j"))
dr <- 1 + 0.033*cos(2*pi/365*J)
# solar declination
delta <- 0.409*sin(2*pi/365*J-1.39)
# sunset hour
omega <- acos(-tan(pi*lat/180 )*tan(delta))
# extra terrestrial radiation
Ra <- 118.08/pi*dr*(omega*sin(pi*lat/180)*sin(delta)+cos(pi*lat/180)*cos(delta)*sin(omega))

# Number of sunshine hours (equation 34)
N <- 24/pi*omega

# Calculate Rs (equation 35)
# using standard values for a_s and b_s
# you can calibrate a_s and b_s for Athens if you have older solar data
a_s <- 0.25
b_s <- 0.5

# also n/N is unknown as you don't have hours of sunshine.
# this is difficult as this requires the number of sunshine hours.
# use n/N = 0.75 as a start 
Rs <- (a_s + b_s*(0.75))*Ra 

#Rnl
Rnl <- 4.903*10^-3*(((MaxT+273)^4+(MinT+273)^4)/2)*
  (0.34-0.14*sqrt(eact))*(0.1+0.9*Rs)

# Rn
Rn <- (1-0.23)*Rs*10^6-Rnl
