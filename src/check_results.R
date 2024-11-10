#===== Note
# This script provides functions to analyze results by sector_analytics.R.
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

#===== settings
pred_days <- c(10,20)
i <- pred_days[2]
sectors <- c(
  # 9 Basic Sectors
  "XLK", "XLF", "XLE",
  "XLB", "XLI", "XLY",
  "XLV", "XLP", "XLU"
)
#===== read rds and csv for replication
# list of files under doc folder
list.files("doc")
# read ground truth and independent variables
winner_x <- read_csv(paste0("doc/winner_x_", i,".csv")) %>%
  dplyr::mutate_at(c("month_of_year", "day_of_week",
                     "nth_wday_month", "month_of_year",
                     "day_of_week","week_of_month"),as.factor)

#===== check best hyper params
modelFit$bestTune
modelFit$results

modelFit$results %>% 
  dplyr::select(max_depth, colsample_bytree, logLoss) %>% 
  tidyr::pivot_wider(names_from=max_depth, values_from=logLoss)

# ROC
res_roc <- pROC::multiclass.roc(winner_x$max_idx, predict(modelFit,winner_x,type="prob"))
res_roc$auc

# add pred col to winner_x
winner_x_pred <- winner_x %>% 
  dplyr::mutate(pred=res_roc$response,
                accuracy=(max_idx==pred)) %>% 
  dplyr::select(actual_date,max_idx,pred,accuracy)
summary(winner_x_pred)

#===== for debugging
# find duplicated date
length(winner_x$actual_date)==
  length(unique(winner_x$actual_date))
