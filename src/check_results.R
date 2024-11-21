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
pred_days <- c(1,3,5)
# select prediction date length
pd <- pred_days[2]
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

# date inverse of average of all sectors won 
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

#===== read pred_prob_dump_
pred_prob <- read_csv(paste0("doc/pred_prob_dump_", pd,".csv"), col_select = -1)
glimpse(tail(pred_prob,1))

#===== add pred col to winner_x
winner_x_pred <- winner_x %>% 
  dplyr::mutate(pred=res_roc$response,
                accuracy=(max_idx==pred)) %>% 
  dplyr::select(actual_date,max_idx,pred,accuracy)
summary(winner_x_pred)

#===== cor SPX and TLT

#===== for debugging
# find duplicated date
length(winner_x$actual_date)==
  length(unique(winner_x$actual_date))

#===== Easy Backtest
# Note: This backtest is rough and optimistic. 
#       Move window for date of train data every oneday to perform legit backtest.
#===
# セクターごとのOutperform確率

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
non_position <- pred_prob %>% 
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
  

# ETF価格
adjusted_prices_df <- as.data.frame(do.call(cbind, adjusted_prices)) %>%
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
# ETF価格とOutperform確率
invest_rate_prices <- invest_rate %>%
  dplyr::inner_join(., adjusted_prices_df, by="actual_date")
# ETF価格と投資配分をactual_dateでjoin
# セクターごとのdata.frameにして，ひとつのListにまとめる
ticker_c <- sectors
invest_rate_prices_list <- foreach(i=1:length(ticker_c), .combine="rbind") %do% {
  target_ticker <- ticker_c[i]
  target_invest_rate_prices <- invest_rate_prices %>%
    dplyr::select(actual_date, starts_with(target_ticker)) %>%
    dplyr::rename(invest_rate := !!target_ticker,
                  price := !!paste0(target_ticker, ".Close")) %>%
    dplyr::mutate(ticker=target_ticker)
}
leverage_invest_rate_prices_list <- invest_rate_prices_list %>%
  dplyr::mutate(invest_rate=case_when(
    ticker %in% c("XLE") ~ invest_rate*2,
    ticker %in% c("XLF","XLK","XLV","TLT") ~ invest_rate*3,
    .default = invest_rate
  ))
# 本戦略のリターン
# レバレッジの場合は、
# エネ ERX = XLE x2
# 金融 FAS = XLF x3
# 技術 TECL = XLK x3
# 長期債 TMF = TLT x3
# ヘルス CURE = XLV x3
leverage_on_c <- c(FALSE, TRUE)
comp_ticker_c <- c("SPY", "SPXL")
comp_duration_c <- c(1000,2000)

for(i in seq(length(leverage_on_c))) {for(j in seq(length(comp_duration_c))) {
  leverage_on <- leverage_on_c[i]
  comp_ticker <- comp_ticker_c[i]
  comp_duration <- comp_duration_c[j]
  print(paste0("lev:",leverage_on,", comp:",comp_ticker, " dur:",comp_duration))

  performance_daily_return <- foreach(k=seq(length(ticker_c)), .combine = "+") %do% {
    target_ticker <- ticker_c[k]
    target_df <- if(leverage_on==TRUE){leverage_invest_rate_prices_list}else{invest_rate_prices_list}
    target_invest_rate_prices <- target_df %>%
      dplyr::filter(ticker %in% target_ticker) %>%
      dplyr::select(actual_date, price) %>%
      as.xts %>%
      dailyReturn(., type="arithmetic")
    target_rate <- target_df %>%
      dplyr::filter(ticker %in% target_ticker) %>%
      dplyr::select(actual_date, invest_rate) %>%
      as.xts
    target_daily_return <- target_invest_rate_prices*target_rate
  } %>%
    setNames(., paste0("mystrategy","_lev_",leverage_on))
  # PerformanceAnalytics::charts.PerformanceSummary(performance_daily_return)

  comp_value_list <- new.env()
  getSymbols(comp_ticker, from = "2006-03-01", to = today(),
             env = comp_value_list,  src = "yahoo", warnings = FALSE)
  comp_value <- as.list(comp_value_list)[[1]]

  SPY_dailyreturn <- comp_value %>%
    adjustOHLC(., use.Adjusted = TRUE) %>%
    dailyReturn(., type="arithmetic") %>%
    setNames(., comp_ticker)
  mystrategy_spy <- merge.xts(performance_daily_return, SPY_dailyreturn)
  png(filename=paste0("doc/vs",comp_ticker,"_",comp_duration,"_lev_",leverage_on,"_pd_",pd,".png"),width = 480, height = 480)
  # for loop内ではprintしないといけない
  print(chart.CumReturns(tail(mystrategy_spy,comp_duration), legend.loc = "bottomright", wealth.index = TRUE))
  dev.off()
}}
#========

