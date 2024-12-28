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
pred_days <- c(1,2,3)
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
trControl <- trainControl(method = "repeatedcv", # method="LOOCV" is bad for large dataset.
                          number = 5, # Try in short time with setting 1.
                          repeats = 2, # Try in short time with setting 1.
                          classProbs = TRUE,
                          summaryFunction = mnLogLoss,
                          search = "random",
                          verboseIter=TRUE,
                          selectionFunction="tolerance")
tuneGrid <- expand.grid(nrounds = 100,
                        max_depth = c(4,6,8),
                        eta = .05,
                        gamma = 0,
                        colsample_bytree = .4,
                        min_child_weight = 1,
                        subsample = 1)

#===== Run sector_analytics
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)
source("src/sector_analytics.R")
stopCluster(cl)

