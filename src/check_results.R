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
pred_days <- c(2,3,4)
# select prediction date length
pd <- pred_days[3]
sectors <- c(
  # 9 Basic Sectors
  "XLK", "XLF", "XLE",
  "XLB", "XLI", "XLY",
  "XLV", "XLP", "XLU", "TLT"
)
#===== read rds and csv for replication
# list of files under doc folder
list.files("doc")
# read ground truth and independent variables
winner_x <- read_csv(paste0("doc/winner_x_", pd,".csv")) %>%
  dplyr::mutate_at(c("month_of_year", "day_of_week",
                     "nth_wday_month", "month_of_year",
                     "day_of_week","week_of_month"),as.factor)
# number of winning by each sector.
table(winner_x$max_idx)

# date that all sectors lose 
winner_x %>% 
  dplyr::filter(max_idx=="zero") %>% 
  dplyr::pull(actual_date) %>% 
  tail(100)

# trained model
modelFit <- readRDS(paste0("doc/modelFit_", pd,".rds"))
# prices
prices_ohlc <- readRDS("doc/sectors_prices.rds")
adjusted_prices <- lapply(prices_ohlc, function(x){(Cl(adjustOHLC(x,use.Adjusted=TRUE)))})

#===== check best hyper params
modelFit$bestTune
modelFit$results

modelFit$results %>% 
  dplyr::select(max_depth, colsample_bytree, logLoss) %>% 
  tidyr::pivot_wider(names_from=max_depth, values_from=logLoss)


#===== ROC
res_roc <- pROC::multiclass.roc(winner_x$max_idx, predict(modelFit,winner_x,type="prob"))
res_roc$auc

#===== Accuracy
cm <- caret::confusionMatrix(data=predict(modelFit,winner_x,type="raw"), reference=as.factor(winner_x$max_idx))
cm

#===== read pred_prob_dump_
pred_prob <- read_csv(paste0("doc/pred_prob_dump_", pd,".csv"), col_select = -1)
glimpse(tail(pred_prob,1))

#===== add pred col to winner_x
#===== check fail date
winner_x_pred <- winner_x %>% 
  dplyr::mutate(pred=predict(modelFit,winner_x,type="raw"),
                accuracy=(max_idx==pred)) %>% 
  dplyr::select(actual_date,max_idx,pred,accuracy)
summary(winner_x_pred)
winner_x_pred %>% 
  dplyr::filter(accuracy==FALSE)

#===== is max_idx the most profitable?
winner_x %>% 
  dplyr::select(actual_date, max_idx) %>% 
  tail(10)

#===== for debugging
# find duplicated date
length(winner_x$actual_date)==
  length(unique(winner_x$actual_date))

#===== Easy Backtest
# Note: This backtest is rough and optimistic. 
#       Move window for date of train data every oneday to perform legit backtest.
#===
# セクターごとのOutperform確率

drs <- foreach(i=c(sectors)) %do% { 
  dr <- dailyReturn(adjustOHLC(prices_ohlc[[i]], use.Adjusted=TRUE))
  names(dr) <- i
  return(dr)
}
drs.xts <- do.call(merge, c(drs, all=TRUE))

invest_rate <- pred_prob %>%
  dplyr::select(-actual_date) %>% 
  # map predicted probability to ticker
  dplyr::mutate(PRED = names(.)[max.col(.)]) %>% 
  dplyr::mutate(actual_date=pred_prob$actual_date) %>%
  # full invest to most profitable ticker.
  # if zero has highest prob, set position 0.
  tidyr::pivot_longer(cols=all_of(c(sectors,"zero")),
                      names_to="ticker") %>% 
  dplyr::mutate(value=if_else(PRED==ticker,1,0)) %>% 
  dplyr::filter(ticker!="zero") %>% 
  tidyr::pivot_wider(names_from=ticker, values_from=value) %>% 
  dplyr::select(-PRED)
invest_rate %>% glimpse
invest_rate %>% tail
invest_rate %>% summary
non_position_date <- pred_prob %>% 
  dplyr::select(-actual_date) %>% 
  # map predicted probability to ticker
  dplyr::mutate(PRED = names(.)[max.col(.)]) %>% 
  dplyr::mutate(actual_date=pred_prob$actual_date) %>%
  # full invest to most profitable ticker.
  # if zero has highest prob, set position 0.
  tidyr::pivot_longer(cols=all_of(c(sectors,"zero")),
                      names_to="ticker") %>% 
  dplyr::filter((PRED==ticker)&(PRED=="zero")) %>% 
  dplyr::pull(actual_date)

invest_rate.xts <- xts(select(invest_rate, -actual_date), order.by=invest_rate$actual_date)
drs.xts
invest_rate.xts

png(filename=paste0("doc/return_portfolio_1year_",pd,".png"), width=480,height=300)
pdlen <- 200
return_mystrategy <- Return.portfolio(R=tail(drs.xts, pdlen), weights=invest_rate.xts, wealth.index=FALSE, contribution=TRUE, 
                 rebalance_on="days")
chart.CumReturns(return_mystrategy$portfolio.returns)
dev.off()
