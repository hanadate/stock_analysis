#===== Note
# This script provides functions to run sector_analytics.R by dopar.
#===== Libraries
library(tidyverse)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(fredr)
library(caret)
library(xgboost)
library(doParallel)
library(pROC)

#===== Setting
# Set your path
starttime <- now()
cut_date <- 0
workdir <- "G:/My Drive/stock_analysis"
result_file <- "doc/today_rate.txt"
setwd(workdir)
print(paste0(today()," START."))
fromdate <- "2000-01-01"
pred_days <- c(2,3)
sectors <- c(
  # 9 Basic Sectors + TLT
  "XLK", "XLF", "XLE",
  "XLB", "XLI", "XLY",
  "XLV", "XLP", "XLU", "TLT"
)
# TRAIN Params
# Note:
# https://topepo.github.io/caret/model-training-and-tuning.html
# For too manu features, colsample_bytree works.
# Not use PCA as feature compression by PCA is based on normal distribution.
# Set nrounds as many as possible.
# tolerance takes the simplest model that is within a percent tolerance of the empirically optimal mode
# Under CV, best model could have small generalization error.
# raytrek
trControl <- trainControl(method = "repeatedcv", # method="LOOCV" is bad for large dataset.
                          number = 5, # 5
                          repeats = 5, # 5
                          classProbs = TRUE,
                          summaryFunction = mnLogLoss,
                          search = "random",
                          verboseIter=TRUE,
                          selectionFunction="tolerance")
tuneGrid <- expand.grid(nrounds = 200,
                        max_depth = c(3,5), #3~8
                        eta = .1, 
                        gamma = .1, # higher is useful for overfit  .1~.4
                        colsample_bytree = .7, #.5~.8
                        min_child_weight = c(2,4), # higher is useful for overfit 2,4,8,16,32
                        subsample = 1) #.5~.8

#===== Run sector_analytics
cores.tmp <- 16 #16
cores <- ifelse(detectCores(logical=TRUE)<cores.tmp, detectCores(logical=TRUE), cores.tmp)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
source("src/sector_analytics.R")
# source("src/price_buy_sell.R")
source("src/stats_return.R")
stopCluster(cl)

