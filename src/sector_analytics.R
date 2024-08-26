library(tidyverse)
# library(riingo)
library(quantmod)
library(PerformanceAnalytics)
# library(slurmR)
library(lubridate)
library(caret)
library(doParallel)
library(xgboost)
library(fredr)

# Set your path
workdir <- "G:/My Drive/stock_analysis"
setwd(workdir)

print(paste0(today()," START."))
# # 取得可能リスト
# tickers <- supported_tickers() # for tiingo
# saveRDS(tickers, file="doc/tiingo_tickers_20231219.rds")
# tickers <- readRDS("doc/tiingo_tickers_20231219.rds")
# tickers %>%
#   dplyr::filter(ticker %in% c("IAU","XHB","TLT","SPY","^VIX", 
#                               "^IRX","^FVX","^TNX","^TYX",
#                               "XLK","XLF","XLRE",
#                               "XLB","XLI","XLY",
#                               "XLE",
#                               "XLC", "XLV", "XLP", "XLU")) %>%
#   dplyr::arrange(desc(startDate))
# 学習用データ取得
# XLC, XLREは歴史が浅いため除去
prices_ohlc <- new.env()
getSymbols(c("IAU", "SLV", "XHB", "TLT", "SPY", "^VIX",
             "^IRX","^FVX","^TNX","^TYX",
             "XLK", "XLF",
             "XLB", "XLI", "XLY",
             "XLE",
             "XLV", "XLP", "XLU"),
           env = prices_ohlc,
           src = "yahoo", from = "2006-03-01", to = today(), warnings = FALSE)
saveRDS(prices_ohlc, file="doc/sectors_prices.rds")
prices_ohlc <- readRDS("doc/sectors_prices.rds")

# AdjustしてCloseを抽出する、あとna.omitしておく
adjusted_prices <- lapply(prices_ohlc, function(x){na.omit(Cl(adjustOHLC(x,use.Adjusted=TRUE)))})

# 最古日付を検査
each_min_date <- lapply(adjusted_prices, function(x){min(index(x))}) %>% 
  do.call(c, .)
unique(each_min_date) # 2006-03-01ならOK

# データ取得状況、最新日付を検査
each_max_date <- lapply(adjusted_prices, function(x){max(index(x))}) %>% 
  do.call(c, .)
unique(each_max_date)
status_get_data <- if(length(unique(each_max_date))==1){"OK"}else{"ERR"}

# Y: 日毎のリターンのrs.par日合計をセクター間比較し，最も大きいセクター
# つまり，次のrs.par日で最もOutperformするセクターを当てる
return_prices <- lapply(adjusted_prices, function(x){
  setNames(
    dailyReturn(x,type="arithmetic"), str_replace(paste0(colnames(x), "_return"), ".Close","")
  )
})
#### Settings
rs.par <- 20
weight20 <- 1.05
weight200 <- 1.1
############
return_rs_prices <- lapply(return_prices, function(x){
  tmp.x <- runSum(x,n=rs.par) 
  colnames(tmp.x)<-str_replace(paste0(colnames(x), "_return_rs"), "_return","")
  return(tmp.x)
})

mergeddf <- as.data.frame(do.call(cbind, return_rs_prices))
# mergeddf %>% glimpse()

# # rs.par日間合計リターンで全てのセクターがマイナスになる確率
# mergeddf %>% 
#   drop_na() %>% 
#   rowwise() %>% 
#   mutate(max = max(across(where(is.numeric)))) %>% 
#   ungroup %>% 
#   round(3) %>% 
#   mutate(positive=if_else(max>0,1,0)) %>% 
#   summarise(mean(positive))
# # 100%の確率でいずれかのセクターは0より大きい


winner <- mergeddf %>% 
  # 投資対象候補セクターに絞る
  dplyr::select(-SPY_return_rs, -IAU_return_rs,
                -SLV_return_rs,
                -XHB_return_rs, -VIX_return_rs,
                -IRX_return_rs, -FVX_return_rs,
                -TNX_return_rs, -TYX_return_rs
  ) %>% 
  dplyr::mutate(date=row.names(.)) %>%
  tidyr::pivot_longer(-date) %>% 
  dplyr::filter(!is.na(value)) %>% 
  group_by(date) %>%
  mutate(
    max_idx=str_replace(.[which.max(value),]$name, "_return_rs", ""),
    max_val=max(value)
  ) %>% 
  ungroup() %>% 
  tidyr::pivot_wider(names_from=name, values_from=value) %>% 
  dplyr::select(date, max_idx) %>% 
  dplyr::mutate(actual_date=lubridate::ymd(date))
# dplyr::mutate(actual_date=lubridate::ymd(date)-lubridate::days(10))
# winner %>% glimpse
# rs.par日間で最もOutperformしたセクター
table(winner$max_idx)
round(table(winner$max_idx)/nrow(winner)*100,1)

# RSI14で各セクターの過熱感
rsi.par<-14
rsi14_prices <- lapply(adjusted_prices, function(x){
  tmp.x <- RSI(x, rsi.par)
  colnames(tmp.x) <- str_replace(paste0(colnames(x), "_rsi14"), ".Close","")
  return(tmp.x)
})
rsi14_merged_prices <- as.data.frame(do.call(cbind, rsi14_prices)) %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
# rsi14_merged_prices %>% glimpse

# ゴールデンクロス、デッドクロス
sma.long.par<-200
sma.short.par<-50
sma_cross_prices <- lapply(adjusted_prices, function(x){
  setNames(
    SMA(x, sma.long.par)/SMA(x, sma.short.par), str_replace(paste0(colnames(x), "_smacross"), ".Close","")
  )
})
sma_cross_merged_prices <- as.data.frame(do.call(cbind, sma_cross_prices)) %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
# sma_cross_merged_prices %>% glimpse

# 金と銀のRSIを比較
gold_rsi14_prices <- rsi14_prices$IAU
slv_rsi14_prices <- rsi14_prices$SLV
gold_slv_rsi14_ratio <- (gold_rsi14_prices / slv_rsi14_prices) %>% 
  setNames(., "gold_slv_rsi14_ratio") %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
# gold_slv_rsi14_ratio %>% glimpse

# 金とホームビルダーのRSIを比較し，景況感を数量化
# 景気が良いと家が建つ，景気が悪いと金に逃げる
gold_rsi14_prices <- rsi14_prices$IAU
hb_rsi14_prices <- rsi14_prices$XHB
gold_hb_rsi14_ratio <- (gold_rsi14_prices / hb_rsi14_prices) %>% 
  setNames(., "gold_hb_rsi14_ratio") %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
# gold_hb_rsi14_ratio %>% glimpse

# SP500と長期債券のRSIを比較し，金利を評価する
# 金利が低いと債券が上がる，金利が高いと株が上がる
spy_rsi14_prices <- rsi14_prices$SPY
tlt_rsi14_prices <- rsi14_prices$TLT
spy_tlt_rsi14_ratio <- (spy_rsi14_prices / tlt_rsi14_prices) %>% 
  setNames(., "spy_tlt_rsi14_ratio") %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
# spy_tlt_rsi14_ratio %>% glimpse

# VIXはそのまま入れる
vix_value <- adjusted_prices$VIX
vix_value_df <- vix_value %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
# vix_value_df %>% glimpse

# 米国債金利
irx_rate <- adjusted_prices$IRX %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
fvx_rate <- adjusted_prices$FVX %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
tnx_rate <- adjusted_prices$TNX %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
tyx_rate <- adjusted_prices$TYX %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
treasuries <- irx_rate %>% 
  dplyr::inner_join(., fvx_rate, by="actual_date") %>% 
  dplyr::inner_join(., tnx_rate, by="actual_date") %>% 
  dplyr::inner_join(., tyx_rate, by="actual_date") %>% 
  dplyr::mutate(
    irx_fvx = IRX.Close / FVX.Close,
    irx_tnx = IRX.Close / TNX.Close,
    irx_tyx = IRX.Close / TYX.Close,
    fvx_tnx = FVX.Close / TNX.Close,
    fvx_tyx = FVX.Close / TYX.Close,
    tnx_tyx = TNX.Close / TYX.Close
  )
# treasuries %>% glimpse

# UTIL/SP500 ratio
xlu_value <- adjusted_prices$XLU %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
spy_value <- adjusted_prices$SPY %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
xlu_spy_ratio <- xlu_value %>% 
  dplyr::inner_join(., spy_value, by="actual_date") %>%
  dplyr::mutate(xlu_spy_ratio = XLU.Close / SPY.Close)

# 金とホームビルダーそのまま比較も追加
xhb_value <- adjusted_prices$XHB %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
iau_value <- adjusted_prices$IAU %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
xhb_iau_ratio <- xhb_value %>% 
  dplyr::inner_join(., iau_value, by="actual_date") %>%
  dplyr::mutate(xhb_iau_ratio = XHB.Close / IAU.Close)

# economic data from FRED


# 特徴量と教師データをJOIN
winner_x <- winner %>% 
  dplyr::full_join(., gold_slv_rsi14_ratio, by="actual_date") %>% 
  dplyr::full_join(., gold_hb_rsi14_ratio, by="actual_date") %>% 
  dplyr::full_join(., spy_tlt_rsi14_ratio, by="actual_date") %>% 
  dplyr::full_join(., rsi14_merged_prices, by="actual_date") %>% 
  dplyr::full_join(., sma_cross_merged_prices, by="actual_date") %>% 
  dplyr::full_join(., vix_value_df, by="actual_date") %>% 
  dplyr::full_join(., treasuries, by="actual_date") %>% 
  dplyr::full_join(., xlu_spy_ratio, by="actual_date") %>% 
  dplyr::full_join(., xhb_iau_ratio, by="actual_date") %>% 
  # rsで最もOutperformするセクターの情報をrs.par日前の変数と同行にずらす
  dplyr::mutate(date=lead(date,n=rs.par), max_idx=lead(max_idx,n=rs.par)) %>% 
  tidyr::drop_na() %>% 
  dplyr::select(-date) %>% 
  dplyr::mutate(max_idx=as.character(max_idx)) #%>% 
  # # weights 直近20日の罰則を1倍、200日を1倍
  # dplyr::mutate(wts=case_when(
  #   rank(actual_date) %in% (max(rank(actual_date))-20):(max(rank(actual_date))) ~ weight20,
  #   rank(actual_date) %in% (max(rank(actual_date))-200):(max(rank(actual_date))-21) ~ weight200,
  #   .default = 1
  # ))

#===== ML
# probに応じて按分比を変えていく 
# Openで成り行きbuy or sell

# all data train
winner_x %>% glimpse

# class weight for imbalancing multi class
class_weight <- ((nrow(winner_x)/length(unique(winner_x$max_idx))) / table(winner_x$max_idx)) %>% 
  as.data.frame() %>% 
  dplyr::rename(wts=Freq) %>% 
  dplyr::left_join(winner_x,. , by=join_by(max_idx==Var1)) %>% 
  dplyr::pull(wts)

# モデルの設定

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          classProbs = TRUE,
                          summaryFunction = mnLogLoss,
                          search = "random")
tuneGrid <- expand.grid(nrounds = 100,
                        max_depth = seq(4,6,1),
                        eta = seq(.01,.03,.01),
                        gamma = 0,
                        colsample_bytree = 1,
                        min_child_weight = 1,
                        subsample = 1)
# モデルのトレーニング
(starttime <- now())
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)
modelFit <- train(max_idx ~ . -actual_date, data = winner_x,
                  weights = class_weight,
                  method = "xgbTree", metric="logLoss",
                  trControl = trControl, tuneGrid = tuneGrid)
stopCluster(cl)
(endtime <- now())
(proc.time <- difftime(endtime, starttime))
saveRDS(modelFit, file="doc/modelFit.rds")
print("TRAINING FINISHED.")
# 140min
modelFit <- readRDS("doc/modelFit.rds")
importance_matrix <- xgb.importance(modelFit$finalModel$feature_names, model = modelFit$finalModel) %>% 
  dplyr::mutate_if(.predicate = is.numeric, .funs = ~ round(.,3))
write.table(importance_matrix, "doc/importance_matrix.txt")
# Today's rate
today_x <- 
  gold_slv_rsi14_ratio %>% 
  dplyr::full_join(., gold_hb_rsi14_ratio, by="actual_date") %>%
  dplyr::full_join(., spy_tlt_rsi14_ratio, by="actual_date") %>% 
  dplyr::full_join(., rsi14_merged_prices, by="actual_date") %>% 
  dplyr::full_join(., sma_cross_merged_prices, by="actual_date") %>%
  dplyr::full_join(., vix_value_df, by="actual_date") %>% 
  dplyr::full_join(., treasuries, by="actual_date") %>% 
  dplyr::full_join(., xlu_spy_ratio, by="actual_date") %>% 
  dplyr::full_join(., xhb_iau_ratio, by="actual_date") %>% 
  tidyr::drop_na() #%>% 
  # # weights 直近20日の罰則を1倍、200日を1倍
  # dplyr::mutate(wts=case_when(
  #   rank(actual_date) %in% (max(rank(actual_date))-20):(max(rank(actual_date))) ~ weight20,
  #   rank(actual_date) %in% (max(rank(actual_date))-200):(max(rank(actual_date))-21) ~ weight200,
  #   .default = 1
  # ))
actual_date_c <- pull(today_x, actual_date)
today_rate <- today_x %>% 
  # 次のn日間リターンの最大セクターを予測
  predict(modelFit, ., type="prob") %>% 
  round(2) %>% 
  dplyr::mutate(actual_date=actual_date_c) %>% 
  dplyr::select("actual_date","XLK","XLF","XLI","XLB","XLY","XLE","XLV","XLU","XLP","TLT")
today_rate %>% tail(1)

#===== Backtest
# セクターごとのOutperform確率
invest_rate <- today_rate %>% 
  dplyr::select(-actual_date) %>% 
  # positive threshold +.3= >=.2でpositive
  +.3 %>%
  round(0) %>%
  dplyr::mutate(actual_date=today_rate$actual_date) %>%
  # 合計値で配分比
  dplyr::mutate(tot=XLK+XLF+XLI+XLB+XLY+XLE+XLV+XLU+XLP+TLT) %>%
  # 合計値で割る
  dplyr::mutate(across(c(XLK,XLF,XLI,XLB,XLY,XLE,XLV,XLU,XLP,TLT), ~.x/tot)) %>%
  # nanを0で補完
  dplyr::mutate(across(everything(), ~replace_na(.x,0)))
invest_rate %>% tail
invest_rate %>% summary

# ETF価格
adjusted_prices_df <- as.data.frame(do.call(cbind, adjusted_prices)) %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
# ETF価格とOutperform確率
invest_rate_prices <- invest_rate %>% 
  dplyr::inner_join(., adjusted_prices_df, by="actual_date")
# ETF価格と投資配分をactual_dateでjoin
# セクターごとのdata.frameにして，ひとつのListにまとめる
ticker_c <- c("XLK","XLF","XLB","XLI","XLY","XLE","XLV","XLP","XLU","TLT")
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
comp_duration_c <- c(20,200,1000,2000)

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
  png(filename=paste0("doc/vs",comp_ticker,"_",comp_duration,"_lev_",leverage_on,".png"),width = 480, height = 480)
  # for loop内ではprintしないといけない
  print(chart.CumReturns(tail(mystrategy_spy,comp_duration), legend.loc = "bottomright", wealth.index = TRUE))
  dev.off()
}}

#=====Result summary
write.table(t(format(tail(today_rate,1), nsmall=2)), "doc/today_rate.txt")
write_lines("
>.2 is valid.
---景気強/金利低---
技術 XLK x3= TECL
金融 XLF x3= FAS
---景気強/金利高---
資本 XLI
素材 XLB
消費 XLY
---景気弱/金利高---
エネ XLE x2= ERX
---景気弱/金利低---
医療 XLV x3= CURE
公共 XLU
必需 XLP
長債 TLT x3= TMF
", file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("START: ",round(starttime)), file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("END: ",round(endtime)), file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("MIN DATE: ",round(unique(each_min_date))), file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("MAX DATE: ",round(unique(each_max_date))), file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("DATA STATUS: ",status_get_data), file="doc/today_rate.txt", append=TRUE)

write.csv(today_rate, file="doc/today_rate_dump.csv")
print("ALL DONE.")
