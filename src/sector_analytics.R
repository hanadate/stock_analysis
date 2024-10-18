#===== Note
# 9 BASIC SECTORS from Super Sectors (Nyaradi, 2010) is:
# XLK, XLI, XLY, XLB, XLE, XLP, XLV, XLU, XLF
# Take advantage of Interpolation, not Extrapolation.
# THERE'S ALWAYS A BULL MARKET SOMEWHERE.
# Short is risker than long. https://www.investopedia.com/terms/s/shortselling.asp
#===== Libraries
library(tidyverse)
library(lubridate)
# library(rlist)
# library(forecast)
# library(riingo)
library(quantmod)
library(PerformanceAnalytics)
library(fredr)
# library(slurmR)
library(caret)
library(xgboost)
library(doParallel)

#===== Setting
# Set your path
starttime <- now()
workdir <- "G:/My Drive/stock_analysis"
setwd(workdir)
print(paste0(today()," START."))
fromdate <- "2000-01-01"

#===== Get prices
prices_ohlc <- new.env()
getSymbols(c(
  # Commodities Futures: 
  "GC=F", "SI=F", "PL=F", "HG=F", "PA=F", # Metals
  "CL=F", "HO=F", "NG=F", "RB=F", # Oil
  "ZC=F", "ZO=F", "KE=F", "ZR=F", "ZM=F", "ZL=F", "ZS=F", # Grains
  "GF=F", "HE=F", "LE=F", # Livestocks
  "CC=F", "KC=F", "CT=F", "OJ=F", "SB=F", # nonessential groceries
  # "LBS=F", # Lumber
  # Gayed (2015) said movement from Lumber to Gold is a leading economic indicator.
  # However, LB1 comdty and LBS=F are not available steadily. I use XHB instead.
  "XHB", # Housing instead of lumber.
  # Treasury Bond
  "TLT", "TLH", "IEF", "IEI", "SHY", "SHV",
  "^IRX","^FVX","^TNX","^TYX",
  # Indices
  "^GSPC", "^DJI", "^IXIC", "^NYA", "^XAX", 
  "BUK100P", "^RUT","^VIX", "^FTSE", "^GDAXI",
  "^FCHI", "^STOXX50E", "^N100", "^BFX", "MOEX.ME", "^HSI",
  "^STI", "^AXJO", "^AORD", "^BSESN", "^JKSE",
  "^KLSE", "^NZ50", "^KS11", "^TWII", "^GSPTSE",
  "^BVSP", "^MXX", "^IPSA", "^MERV", "^TA125.TA",
  "^CASE30", "^JN0U.JO", "DX-Y.NYB", "^125904-USD-STRD",
  "^XDB", "^XDE", "000001.SS", "^N225", "^XDN", "^XDA",
  # 
  # 9 Basic Sectors
  "XLK", "XLF", "XLE",
  "XLB", "XLI", "XLY",
  "XLV", "XLP", "XLU"),
  env = prices_ohlc,
  src = "yahoo", from = fromdate, to = today(), warnings = FALSE)
saveRDS(prices_ohlc, file="doc/sectors_prices.rds")
prices_ohlc <- readRDS("doc/sectors_prices.rds")

#===== Get FRED data
# Popular series https://fred.stlouisfed.org/tags/series?ob=pv
fredr_set_key(read_file("apikey/fred_api.txt"))
unrate_df <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date(fromdate),
  observation_end = today()
) %>% 
  dplyr::select(date, value) %>% 
  dplyr::rename(actual_date=date, unrate=value) %>% 
  dplyr::mutate(unrate_change=unrate/lag(unrate)) %>% 
  # Date of FRED data is actual previous date. Change it announcement date.
  # monthly 1st Friday.
  dplyr::mutate(actual_date=actual_date+months(1))

mortgage_df <- fredr(
  series_id = "MORTGAGE30US",
  observation_start = as.Date(fromdate),
  observation_end = today()
) %>% 
  dplyr::select(date, value) %>% 
  dplyr::rename(actual_date=date, mortgage=value) %>% 
  dplyr::mutate(mortgage_change=mortgage/lag(mortgage))
# weekly ending Thursday. No adjustment.

m2_df <- fredr(
  series_id = "M2SL",
  observation_start = as.Date(fromdate),
  observation_end = today()
) %>% 
  dplyr::select(date, value) %>% 
  dplyr::rename(actual_date=date, m2=value) %>% 
  dplyr::mutate(m2_change=m2/lag(m2)) %>% 
  # Date of FRED data is actual previous date. Change it announcement date.
  # monthly final Tuesday.
  dplyr::mutate(actual_date=actual_date+months(1))

#===== Tidy up data
# Adjust prices to extract Close price 
adjusted_prices <- lapply(prices_ohlc, function(x){(Cl(adjustOHLC(x,use.Adjusted=TRUE)))})

# Check Missing Rate for each ticker
missing_rate <- lapply(adjusted_prices, function(x){mean(is.na(x))}) %>% 
  do.call(c,.) %>% 
  sort()

# Delete tickers have >.05 missing.
ok_tickers <- names(missing_rate[missing_rate < .05])
adjusted_prices <- adjusted_prices[ok_tickers]
# Impute the last available data to missing aka last observation carried forward (locf).
adjusted_prices <- lapply(adjusted_prices, function(x){na.locf(x)})

# Check the oldest date for each ticker
each_min_date <- lapply(adjusted_prices, function(x){min(index(x))}) %>% 
  do.call(c, .) %>% 
  sort()
# each_min_date
# Delete tickers are newer than XHB (2006-02-06).
ok_tickers <- names(each_min_date[each_min_date <= each_min_date["XHB"]])
adjusted_prices <- adjusted_prices[ok_tickers]
# Trim older date than the inception date of XHB.
adjusted_prices <- adjusted_prices %>% 
  map(\(x) x[index(x)>=each_min_date["XHB"]])

# Check the latest date for each ticker.
each_max_date <- lapply(adjusted_prices, function(x){max(index(x))}) %>% 
  do.call(c, .) %>% 
  sort()
# each_max_date
# unique(each_max_date)
# Check lengths
# lapply(adjusted_prices, function(x){length(x)}) %>% unlist %>% sort
# Use XHB as standard and impute by locf to even out.

#====== Features
#=== RSI14
rsi.par<-14
rsi14_prices <- lapply(adjusted_prices, function(x){
  tmp.x <- RSI(x, rsi.par)
  colnames(tmp.x) <- str_replace(paste0(colnames(x), "_rsi14"), ".Close","")
  return(tmp.x)
})
rsi14_merged_prices <- as.data.frame(do.call(cbind, rsi14_prices)) %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))

#=== Compare with each ticker
ticker_pairs <- combn(names(adjusted_prices),2)
compare_price <- function(t1, t2){
  res <- (adjusted_prices[[t1]] / adjusted_prices[[t2]]) %>% 
    setNames(., paste0(t1,"_",t2,"_price_ratio")) %>% 
    as.data.frame() %>% 
    dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
  return(res)
}
t <- now()
print("Comparing all prices...")
compared_price_df <- 
  foreach(i=seq(ncol(ticker_pairs)), .combine="full_join",
          .packages="tidyverse", .errorhandling="remove") %do% {
            res <- compare_price(ticker_pairs[1,i], ticker_pairs[2,i])
            return(res)
          }
print(paste0("Proc time is ", now() - t))
compared_price_df <- compared_price_df %>% 
  dplyr::arrange(actual_date) %>% 
  # na.locf
  tidyr::fill(everything(), .direction="down")

#=== Compare with each RSI
compare_rsi <- function(t1, t2){
  res <- (rsi14_prices[[t1]] / rsi14_prices[[t2]]) %>% 
    setNames(., paste0(t1,"_",t2,"_rsi14_ratio")) %>% 
    as.data.frame() %>% 
    dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
  return(res)
}
t <- now()
print("Comparing all RSIs...")
compared_rsi_df <- 
  foreach(i=seq(ncol(ticker_pairs)), .combine="full_join",
          .packages="tidyverse", .errorhandling="remove") %do% {
            res <- compare_rsi(ticker_pairs[1,i], ticker_pairs[2,i])
            return(res)
          }
print(paste0("Proc time is ", now() - t))
compared_rsi_df <- compared_rsi_df %>% 
  dplyr::arrange(actual_date) %>% 
  # na.locf
  tidyr::fill(everything(), .direction="down")

#=== Golden cross, death cross.
sma.long.par<-200
sma.short.par<-50
sma_cross_prices <- lapply(adjusted_prices, function(x){
  setNames(
    SMA(x, sma.long.par)/SMA(x, sma.short.par), str_replace(paste0(colnames(x), "_smacross"), ".Close","")
  )
})
sma_cross_merged_prices <- as.data.frame(do.call(cbind, sma_cross_prices)) %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.))) %>% 
  tidyr::fill(everything(), .direction="down")

#=== VIX
vix_value <- adjusted_prices$VIX
vix_value_df <- vix_value %>% 
  as.data.frame() %>% 
  dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))

#=== Treasury yield
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

#===== Iteration for each prediction range
# Y: The most profitable sector is calculated by comparing with sum of daily return in rs.par days
# In other words, the objective of this problem is prediction for the most outpeformed sector in next rs.par days. 
return_prices <- lapply(adjusted_prices[c("XLK", "XLF", "XLE",
                                          "XLB", "XLI", "XLY",
                                          "XLV", "XLP", "XLU")], function(x){
                                            setNames(
                                              dailyReturn(x,type="arithmetic"), str_replace(paste0(colnames(x), "_return"), ".Close","")
                                            )
                                          })

today_rate_combined <- foreach(i=c(5,10), .combine="rbind") %do% {
  print(paste0("Start ",i," days forecast at ",lubridate::now()))
  rs.par <- i
  return_rs_prices <- lapply(return_prices, function(x){
    tmp.x <- runSum(x,n=rs.par) 
    colnames(tmp.x)<-str_replace(paste0(colnames(x), "_return_rs"), "_return","")
    return(tmp.x)
  })
  # as.data.frame
  mergeddf <- as.data.frame(do.call(cbind, return_rs_prices))
  
  winner <- mergeddf %>% 
    # Select focal Sectors
    dplyr::select(starts_with(c("XLK", "XLF", "XLE",
                                "XLB", "XLI", "XLY",
                                "XLV", "XLP", "XLU"))
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
  # Number and ratio of win for each sector in rs.par days.
  table(winner$max_idx)
  round(table(winner$max_idx)/nrow(winner)*100,1)
  
  # JOIN winners and features
  winner_x <- winner %>% 
    # Firts, winner sector (max_idx) cell in rs.par days move to rs.par days before
    dplyr::arrange(actual_date) %>% 
    dplyr::mutate(max_idx=lead(max_idx,n=rs.par)) %>% 
    dplyr::select(-date) %>% 
    # Join features
    dplyr::left_join(., rsi14_merged_prices, by="actual_date") %>% 
    dplyr::left_join(., sma_cross_merged_prices, by="actual_date") %>% 
    dplyr::left_join(., vix_value_df, by="actual_date") %>% 
    dplyr::left_join(., treasuries, by="actual_date") %>% 
    dplyr::left_join(., compared_rsi_df, by="actual_date") %>%
    dplyr::left_join(., compared_price_df, by="actual_date") %>%
    dplyr::left_join(., unrate_df, by="actual_date") %>%
    dplyr::left_join(., mortgage_df, by="actual_date") %>% 
    dplyr::left_join(., m2_df, by="actual_date") %>%
    dplyr::arrange(actual_date) %>% 
    # Fill NA by last obs except max_idx.
    tidyr::fill(-one_of("max_idx"),.direction="down") %>%
    tidyr::drop_na(-one_of("max_idx")) %>%
    dplyr::mutate(max_idx=as.character(max_idx)) %>% 
    # Trim to start from Dec. 2006.
    dplyr::filter(actual_date>=ymd("2006-12-01")) %>% 
    # Make Calendar Features
    dplyr::mutate(year = lubridate::year(actual_date),
                  month_of_year = lubridate::month(actual_date),
                  # For check, set label TRUE.
                  day_of_week = lubridate::wday(actual_date,label=FALSE)) %>% 
    dplyr::group_by(year, month_of_year, day_of_week) %>% 
    dplyr::mutate(week_of_month=row_number()) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(nth_wday_month=as.factor(paste0(week_of_month,"th_",day_of_week,"_of_",month_of_year)),
                  year=as.factor(year),
                  month_of_year=as.factor(month_of_year),
                  day_of_week=as.factor(day_of_week)) %>% 
    dplyr::select(-week_of_month) %>% 
    # Delete Saturday & Sunday.
    # Sort by actual_date
    dplyr::arrange(actual_date)
  # This dataset includes NAs in object variable.
  # Thus, pass the parameter na.action=na.omit
  # check train data 
  # winner_x %>% dim
  write_csv(winner_x, file=paste0("doc/winner_x_", i,".csv"))
  
  #===== ML
  # class weight for imbalancing multi class
  class_weight <- ((nrow(winner_x)/length(unique(winner_x$max_idx))) / table(winner_x$max_idx)) %>% 
    as.data.frame() %>% 
    dplyr::rename(wts=Freq) %>% 
    dplyr::left_join(winner_x,. , by=join_by(max_idx==Var1)) %>% 
    dplyr::pull(wts)
  
  # Params
  # Note:
  # Not much different from 3 and 9 in number of CV (Nti et al, 2021).
  # For too manu features, colsample_bytree works. 
  # Not use PCA as feature compression by PCA is based on normal distribution.
  # Set nrounds as many as possible.  
  trControl <- trainControl(method = "repeatedcv", # method="LOOCV" is bad for large dataset.
                            number = 10, # Try in short time with setting 1. (10)
                            repeats = 2, # Try in short time with setting 1. (2)
                            classProbs = TRUE,
                            summaryFunction = mnLogLoss,
                            search = "random",
                            verboseIter=TRUE)
  tuneGrid <- expand.grid(nrounds = c(100,1000),
                          max_depth = 6,
                          eta = .3,
                          gamma = 0,
                          colsample_bytree = c(.1,.4,.7),
                          min_child_weight = 1,
                          subsample = 1)
  # Train
  print(paste0("Start train at ", now()))
  cl <- makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  modelFit <- train(max_idx ~ . -actual_date, data = winner_x,
                    weights = class_weight,
                    method = "xgbTree", metric="logLoss", 
                    na.action=na.omit,
                    trControl = trControl, tuneGrid = tuneGrid)
  stopCluster(cl)
  saveRDS(modelFit, file=paste0("doc/modelFit_", i,".rds"))
  print(paste0("TRAINING FINISHED at ", now()))
  #=== End of training.
  modelFit <- readRDS(paste0("doc/modelFit_", i,".rds"))
  importance_matrix <- xgb.importance(modelFit$finalModel$feature_names, model = modelFit$finalModel) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = ~ round(.,3))
  write.table(importance_matrix, paste0("doc/importance_matrix_", i,".txt"))
  
  # predict the most profitable sector as probability.
  today_x <- winner_x
  actual_date_c <- pull(today_x, actual_date)
  pred_prob <- today_x %>% 
    predict(modelFit, ., type="prob") %>% 
    round(2) %>% 
    dplyr::mutate(actual_date=actual_date_c) %>% 
    dplyr::select("actual_date","XLK","XLF","XLI","XLB","XLY","XLE","XLV","XLU","XLP")
  write.csv(pred_prob, file=paste0("doc/pred_prob_dump_", i,".csv"))
  today_rate <- tail(pred_prob,1) %>% 
    dplyr::mutate(actual_date=as.integer(i)) %>%
    dplyr::rename(predict_date=actual_date)
  write.table(today_rate, paste0("doc/today_rate_dump", i,".txt"))
  return(today_rate)
}
endtime <- now()

#=====Result summary
today_rate_combined %>% 
  format(., nsmall=2) %>% 
  t(.) %>% 
  write.table(., "doc/today_rate.txt")
write_lines("
Tech XLK x3= TECL
Fin  XLF x3= FAS
Ind  XLI
Bas  XLB
Cons XLY
Ene  XLE x2= ERX
Heal XLV x3= CURE
Util XLU
Stap XLP
# Note: Sell fast Leveraged ETFs. It must decay in a long term.
", file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("START: ",round(starttime)), file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("END: ",round(endtime)), file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("MIN DATE: ",min(winner_x$actual_date)), file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("MAX DATE: ",max(winner_x$actual_date)), file="doc/today_rate.txt", append=TRUE)
write_lines(paste0("DIM: ",nrow(winner_x), "x",ncol(winner_x)), file="doc/today_rate.txt", append=TRUE)
write_lines("===trainControl===", file="doc/today_rate.txt", append=TRUE)
write_lines(trControl, file="doc/today_rate.txt", append=TRUE)
write_lines("===tuneGrid===", file="doc/today_rate.txt", append=TRUE)
write_lines(tuneGrid, file="doc/today_rate.txt", append=TRUE)
print("PREDICT FINISHED.")


# #=====Backtest
# print("Start Backtest")
# print(now())
# # Check exist predictions
# predictions <- paste0("doc/",dir("doc")[str_starts(dir("doc"),"pred_prob_dump")])
# 
# # セクターごとのOutperform確率
# today_rate <- read_csv(predictions[1])
# invest_rate <- today_rate %>%
#   dplyr::select(-actual_date) %>%
#   # positive threshold +.3= >=.2でpositive
#   +.3 %>%
#   round(0) %>%
#   dplyr::mutate(actual_date=today_rate$actual_date) %>%
#   # 合計値で配分比
#   dplyr::mutate(tot=XLK+XLF+XLI+XLB+XLY+XLE+XLV+XLU+XLP+TLT) %>%
#   # 合計値で割る
#   dplyr::mutate(across(c(XLK,XLF,XLI,XLB,XLY,XLE,XLV,XLU,XLP,TLT), ~.x/tot)) %>%
#   # nanを0で補完
#   dplyr::mutate(across(everything(), ~replace_na(.x,0)))
# invest_rate %>% tail
# invest_rate %>% summary
# 
# # ETF価格
# adjusted_prices_df <- as.data.frame(do.call(cbind, adjusted_prices)) %>%
#   dplyr::mutate(actual_date=lubridate::ymd(row.names(.)))
# # ETF価格とOutperform確率
# invest_rate_prices <- invest_rate %>%
#   dplyr::inner_join(., adjusted_prices_df, by="actual_date")
# # ETF価格と投資配分をactual_dateでjoin
# # セクターごとのdata.frameにして，ひとつのListにまとめる
# ticker_c <- c("XLK","XLF","XLB","XLI","XLY","XLE","XLV","XLP","XLU","TLT")
# invest_rate_prices_list <- foreach(i=1:length(ticker_c), .combine="rbind") %do% {
#   target_ticker <- ticker_c[i]
#   target_invest_rate_prices <- invest_rate_prices %>%
#     dplyr::select(actual_date, starts_with(target_ticker)) %>%
#     dplyr::rename(invest_rate := !!target_ticker,
#                   price := !!paste0(target_ticker, ".Close")) %>%
#     dplyr::mutate(ticker=target_ticker)
# }
# leverage_invest_rate_prices_list <- invest_rate_prices_list %>%
#   dplyr::mutate(invest_rate=case_when(
#     ticker %in% c("XLE") ~ invest_rate*2,
#     ticker %in% c("XLF","XLK","XLV","TLT") ~ invest_rate*3,
#     .default = invest_rate
#   ))
# # 本戦略のリターン
# # レバレッジの場合は、
# # エネ ERX = XLE x2
# # 金融 FAS = XLF x3
# # 技術 TECL = XLK x3
# # 長期債 TMF = TLT x3
# # ヘルス CURE = XLV x3
# leverage_on_c <- c(FALSE, TRUE)
# comp_ticker_c <- c("SPY", "SPXL")
# comp_duration_c <- c(20,200,1000,2000)
# 
# for(i in seq(length(leverage_on_c))) {for(j in seq(length(comp_duration_c))) {
#   leverage_on <- leverage_on_c[i]
#   comp_ticker <- comp_ticker_c[i]
#   comp_duration <- comp_duration_c[j]
#   print(paste0("lev:",leverage_on,", comp:",comp_ticker, " dur:",comp_duration))
# 
#   performance_daily_return <- foreach(k=seq(length(ticker_c)), .combine = "+") %do% {
#     target_ticker <- ticker_c[k]
#     target_df <- if(leverage_on==TRUE){leverage_invest_rate_prices_list}else{invest_rate_prices_list}
#     target_invest_rate_prices <- target_df %>%
#       dplyr::filter(ticker %in% target_ticker) %>%
#       dplyr::select(actual_date, price) %>%
#       as.xts %>%
#       dailyReturn(., type="arithmetic")
#     target_rate <- target_df %>%
#       dplyr::filter(ticker %in% target_ticker) %>%
#       dplyr::select(actual_date, invest_rate) %>%
#       as.xts
#     target_daily_return <- target_invest_rate_prices*target_rate
#   } %>%
#     setNames(., paste0("mystrategy","_lev_",leverage_on))
#   # PerformanceAnalytics::charts.PerformanceSummary(performance_daily_return)
# 
#   comp_value_list <- new.env()
#   getSymbols(comp_ticker, from = "2006-03-01", to = today(),
#              env = comp_value_list,  src = "yahoo", warnings = FALSE)
#   comp_value <- as.list(comp_value_list)[[1]]
# 
#   SPY_dailyreturn <- comp_value %>%
#     adjustOHLC(., use.Adjusted = TRUE) %>%
#     dailyReturn(., type="arithmetic") %>%
#     setNames(., comp_ticker)
#   mystrategy_spy <- merge.xts(performance_daily_return, SPY_dailyreturn)
#   png(filename=paste0("doc/vs",comp_ticker,"_",comp_duration,"_lev_",leverage_on,".png"),width = 480, height = 480)
#   # for loop内ではprintしないといけない
#   print(chart.CumReturns(tail(mystrategy_spy,comp_duration), legend.loc = "bottomright", wealth.index = TRUE))
#   dev.off()
# }}
# #========
# 
