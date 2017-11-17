
setwd("c:/users/rver4657/owncloud/southsouthnetwork/satellitecalibration")

require(tidyverse)

chirpsSL <- readRDS("data/CHIRPS/output.RDS")

# convert prcp from list into a dataframe
# first extract the prcp elements into a list
SLprcp <- chirpsSL %>% map("prcp")

is.list(SLprcp)

SLprcp_df <- as.data.frame(do.call(cbind,SLprcp))
head(SLprcp_df)
