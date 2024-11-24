#===== Note
# This script provides functions to run sector_analytics.R by mpi.
#===== Libraries
library(Rmpi)
library(doMPI)
#===== Run by mpi
cl <- startMPIcluster(count=96-1)
registerDoMPI(cl)
source("src/sector_analytics.R")
closeCluster(cl)
