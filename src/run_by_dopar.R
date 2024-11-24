#===== Note
# This script provides functions to run sector_analytics.R by dopar.
#===== Run by dopar
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)
source("src/sector_analytics.R")
stopCluster(cl)

