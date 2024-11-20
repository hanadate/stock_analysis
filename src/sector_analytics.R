#===== Note
# This script provides a prediction the most profitable sector for any days. 
# 9 BASIC SECTORS from Super Sectors (Nyaradi, 2010) is:
# XLK, XLI, XLY, XLB, XLE, XLP, XLV, XLU, XLF
# Take advantage of Interpolation, not Extrapolation.
# Jim Cramer frequently said THERE'S ALWAYS A BULL MARKET SOMEWHERE.
# Short is risker than long. https://www.investopedia.com/terms/s/shortselling.asp
# Add zero return into target classes to catch a DON'T LONG signal.
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
workdir <- "G:/My Drive/stock_analysis"
result_file <- "doc/today_rate.txt"
setwd(workdir)
print(paste0(today()," START."))
fromdate <- "2000-01-01"
pred_days <- c(5,10,15)
sectors <- c(
  # 9 Basic Sectors 
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
                          number = 3, # Try in short time with setting 1.
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
                        colsample_bytree = c(.4,.7),
                        min_child_weight = 1,
                        subsample = 1)

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
  sectors),
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

#=== Zero (Originally, this is inverse of average for all sectors)
zero_sectors <- lapply(adjusted_prices[c(sectors)], function(x){
  setNames(
    -dailyReturn(x,type="log"), str_replace(paste0(colnames(x), "_return"), ".Close","")
  )
}) %>% Reduce("+",.)/length(sectors) *0
names(zero_sectors) <- "zero_return"

#===== Iteration for each prediction range
# Y: The most profitable sector is calculated by comparing with sum of daily return in rs.par days
# In other words, the objective of this problem is prediction for the most outpeformed sector in next rs.par days. 
return_prices <- lapply(adjusted_prices[c(sectors)], function(x){
  setNames(
    dailyReturn(x,type="log"), str_replace(paste0(colnames(x), "_return"), ".Close","")
  )
}) %>% 
  rlist::list.append(., zero_sectors)
today_rate_combined <- foreach(i=pred_days, .combine="rbind") %do% {
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
    # dplyr::select(starts_with(c(sectors))
    # ) %>% 
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
  # Number and ratio of win for each sector for rs.par days.
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
                  week_of_month=as.factor(week_of_month),
                  day_of_week=as.factor(day_of_week)) %>%
    # Delete non-circular features
    dplyr::select(-year) %>%
    # dplyr::select(-week_of_month) %>% 
    # Delete Saturday & Sunday.
    # Sort by actual_date
    dplyr::arrange(actual_date)
  # This dataset includes NAs in object variable.
  # Thus, pass the parameter na.action=na.omit
  # check train data 
  # winner_x %>% dim
  write_csv(winner_x, file=paste0("doc/winner_x_", i,".csv"))
  # when read csv, apply factor type for calendar features.
  # winner_x <- read_csv(paste0("doc/winner_x_", i,".csv")) %>%
  #   dplyr::mutate_at(c("month_of_year", "day_of_week",
  #                      "nth_wday_month", "month_of_year",
  #                      "day_of_week","week_of_month"),as.factor)
  
  #===== ML
  # class weight for imbalancing multi class
  class_weight <- ((nrow(winner_x)/length(unique(winner_x$max_idx))) / table(winner_x$max_idx)) %>% 
    as.data.frame() %>% 
    dplyr::rename(wts=Freq) %>% 
    dplyr::left_join(winner_x,. , by=join_by(max_idx==Var1)) %>% 
    dplyr::pull(wts)
  
  # Train
  print(paste0("Start train at ", now()))
  cl <- makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  set.seed(1111)
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
  
  # importance 
  importance_matrix <- xgb.importance(modelFit$finalModel$feature_names, model = modelFit$finalModel) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = ~ round(.,3))
  write.csv(importance_matrix, paste0("doc/importance_matrix_", i,".csv"))
  
  # predict the most profitable sector as probability.
  actual_date_c <- pull(winner_x, actual_date)
  pred_prob <- winner_x %>% 
    predict(modelFit, ., type="prob") %>% 
    round(2) %>% 
    dplyr::mutate(actual_date=actual_date_c) %>% 
    dplyr::select(c("actual_date", sectors,"zero"))
  write.csv(pred_prob, file=paste0("doc/pred_prob_dump_", i,".csv"))
  # roc auc
  res_roc <- pROC::multiclass.roc(winner_x$max_idx, predict(modelFit,winner_x,type="prob"))
  saveRDS(res_roc, file=paste0("doc/res_roc_",i,".rds"))
  # make return obj.
  today_rate <- tail(pred_prob,1) %>% 
    dplyr::mutate(actual_date=as.integer(i)) %>%
    dplyr::rename(predict_date=actual_date)
  write.table(today_rate, paste0("doc/today_rate_dump", i,".txt"))
  return(today_rate)
}
saveRDS(today_rate_combined,"doc/today_rate_combined.rds")
endtime <- now()

#=====Result summary
today_rate_combined %>% 
  dplyr::rename(PD=predict_date) %>% 
  format(., nsmall=2) %>% 
  t(.) %>% 
  write.table(., result_file)
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
Trea TLT
Zero zero
# Note: Sell fast Leveraged ETFs. It must decay in a long term.
", file=result_file, append=TRUE)
write_lines(paste0("START: ",round(starttime)), file=result_file, append=TRUE)
write_lines(paste0("END: ",round(endtime)), file=result_file, append=TRUE)
write_lines(paste0("MIN DATE: ",min(winner_x$actual_date)), file=result_file, append=TRUE)
write_lines(paste0("MAX DATE: ",max(winner_x$actual_date)), file=result_file, append=TRUE)
write_lines(paste0("DIM: ",nrow(winner_x), "x",ncol(winner_x)), file=result_file, append=TRUE)
for(i in pred_days){
  write_lines(paste0("===",i," days==="), file=result_file, append = TRUE)
  res_roc <- readRDS(paste0("doc/res_roc_",i,".rds"))
  write_lines(paste0("AUC: ", res_roc$auc), file=result_file, append=TRUE)
  train_summary <- readRDS(paste0("doc/modelFit_",i,".rds"))
  write_lines(paste0(names(train_summary$results)," : ",round(train_summary$results,2)), file=result_file, append=TRUE)
  write_lines(paste0(names(train_summary$bestTune)," : ",train_summary$bestTune), file=result_file, append=TRUE)
}
write_lines("===trainControl===", file=result_file, append=TRUE)
write_lines(paste0(names(trControl)," : " , trControl), file=result_file, append=TRUE)
write_lines("===tuneGrid===", file=result_file, append=TRUE)
write_lines(paste0(names(tuneGrid)," : " , tuneGrid), file=result_file, append=TRUE)
print("PREDICT FINISHED.")
