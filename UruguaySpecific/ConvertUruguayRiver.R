# reformat data from the Concordia station Uruguay river in Argentina
# now also does the Parana river in Argentina
# Data from : https://nelson.wisc.edu/sage/data-and-models/datasets.php

require(tidyverse)

setwd("S:/PRJ-HPWC/SWAT_ETCalibration/InputData")

UR <- read.table("Uruguay_Condordia_data.txt", skip = 15,
                 nrows = 11, sep = "\t", header = T)

UR_new <- UR %>%
      gather(`Jan`:`Dec`, key = Month, value = Flow) 

UR_new <- UR_new[,-2]

UR_new$M <- rep(1:12,each=11)
UR_new <- UR_new[,-2]
names(UR_new)[3] <- "Month"
UR_new <- UR_new %>%
      arrange(Year, Month) %>%
      select(Year,Month,Flow)

head(UR_new)

write.csv(UR_new, "UruguayRiver_ConcordiaSt.csv", row.names = F)

PR <- read.table("Parana_Corrientes_data.txt", skip = 15,
                 nrows = 11, sep = "\t", header = T)

PR_new <- PR %>%
  gather(`Jan`:`Dec`, key = Month, value = Flow) 

PR_new <- PR_new[,-2]

PR_new$M <- rep(1:12,each=11)
PR_new <- PR_new[,-2]
names(PR_new)[3] <- "Month"
PR_new <- PR_new %>%
  arrange(Year, Month) %>%
  select(Year,Month,Flow)

head(PR_new)

write.csv(PR_new, "Parana_CorrientesSt.csv", row.names = F)
