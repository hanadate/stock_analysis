#===== Target: the most profitable sector in future 2 days
rate2 <- read.table("doc/today_rate_dump2.txt") %>% 
  select(-predict_date)
target <- colnames(rate2)[apply(rate2, 1, which.max)]
target_ohlc <- prices_ohlc[[target]] %>% 
  adjustOHLC(., use.Adjusted=TRUE)

#===== Buy: low price of target in future 1 day
target_lo <- Lo(target_ohlc)
names(target_lo) <- "target"
return_target_lo <- lapply(target_lo, function(x){
  setNames(
    dailyReturn(x,type="log"), str_replace(paste0(colnames(x), "_return"), ".Low","")
  )
})
return_target_lo_1 <- lapply(return_target_lo, function(x){
  tmp.x <- runSum(x,n=1) 
  colnames(tmp.x)<-str_replace(paste0(colnames(x), "_return_rs"), "_return","")
  return(tmp.x)
}) %>% 
  # date-1 to avoid leakage
  as.data.frame() %>% 
  rownames_to_column("actual_date") %>% 
  mutate(actual_date=ymd(actual_date)-days(1)) %>% 
  `colnames<-`(c("actual_date","lo_1"))
# load features
winner_x_1 <- read_csv(file="doc/winner_x_1.csv")
x_1 <- winner_x_1 %>% 
  select(-max_idx)
# join features and target
lo_x_1 <- x_1 %>% 
  inner_join(., return_target_lo_1, by=c("actual_date"))

# TODO
# Train
print(paste0("Start train at ", now()))
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)
set.seed(1111)
trControl2 <- trainControl(method = "repeatedcv", # method="LOOCV" is bad for large dataset.
                          number = 10, # Try in short time with setting 1.
                          repeats = 2, # Try in short time with setting 1.
                          summaryFunction = defaultSummary,
                          search = "random",
                          verboseIter=TRUE,
                          selectionFunction="tolerance")
modelFit_lo_1 <- train(lo_1 ~ . -actual_date, data = lo_x_1,
                  method = "xgbTree", metric="RMSE", 
                  na.action=na.omit,
                  trControl = trControl2, tuneGrid = tuneGrid)
# stopCluster(cl)
saveRDS(modelFit_lo_1, file=paste0("doc/modelFit_lo_1.rds"))
print(paste0("TRAINING FINISHED at ", now()))
#=== End of training.
modelFit_lo_1 <- readRDS(paste0("doc/modelFit_lo_1.rds"))

# predict the buy price.
actual_date_c <- pull(lo_x_1, actual_date)
pred_lo_1 <- lo_x_1 %>% 
  predict(modelFit_lo_1, .)
write.csv(pred_lo_1, file=paste0("doc/pred_lo_1.csv"))
cutsize <- 200
pred_lo_1_price <- data.frame(lo=tail(target_lo,cutsize), pred_lo_logreturn=tail(pred_lo_1,cutsize)) %>% 
  mutate(pred_lo_return=exp(pred_lo_logreturn),
         pred_lo_price=target*pred_lo_return) %>% 
  .$pred_lo_price %>% 
  tail(.,1)


#===== Sell: high price of taarget in future 2 days
target_hi <- Hi(target_ohlc)
names(target_hi) <- "target"
return_target_hi <- lapply(target_hi, function(x){
  setNames(
    dailyReturn(x,type="log"), str_replace(paste0(colnames(x), "_return"), ".High","")
  )
})
return_target_hi_2 <- lapply(return_target_hi, function(x){
  tmp.x <- runSum(x,n=2) 
  colnames(tmp.x)<-str_replace(paste0(colnames(x), "_return_rs"), "_return","")
  return(tmp.x)
}) %>% 
  # date-1 to avoid leakage
  as.data.frame() %>% 
  rownames_to_column("actual_date") %>% 
  mutate(actual_date=ymd(actual_date)-days(2)) %>% 
  `colnames<-`(c("actual_date","hi_2"))
# load features
winner_x_2 <- read_csv(file="doc/winner_x_2.csv")
x_2 <- winner_x_2 %>% 
  select(-max_idx)
# join features and target
hi_x_2 <- x_2 %>% 
  inner_join(., return_target_hi_2, by=c("actual_date"))

# TODO
# Train
print(paste0("Start train at ", now()))
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)
set.seed(1111)
trControl2
modelFit_hi_2 <- train(hi_2 ~ . -actual_date, data = hi_x_2,
                       method = "xgbTree", metric="RMSE", 
                       na.action=na.omit,
                       trControl = trControl2, tuneGrid = tuneGrid)
# stopCluster(cl)
saveRDS(modelFit_hi_2, file=paste0("doc/modelFit_hi_2.rds"))
print(paste0("TRAINING FINISHED at ", now()))
#=== End of training.
modelFit_hi_2 <- readRDS(paste0("doc/modelFit_hi_2.rds"))

# predict the buy price.
actual_date_c <- pull(hi_x_2, actual_date)
pred_hi_2 <- hi_x_2 %>% 
  predict(modelFit_hi_2, .)
write.csv(pred_hi_2, file=paste0("doc/pred_hi_2.csv"))
cutsize <- 200
pred_hi_2_price <- data.frame(hi=tail(target_hi,cutsize), pred_hi_logreturn=tail(pred_hi_2,cutsize)) %>% 
  mutate(pred_hi_return=exp(pred_hi_logreturn),
         pred_hi_price=target*pred_hi_return) %>% 
  .$pred_hi_price %>% 
  tail(.,1)


# write results
write_lines(paste0("BUY ", target, " at ", pred_lo_1_price), file=result_file, append=TRUE)
write_lines(paste0("SELL ", target, " at ", pred_hi_2_price), file=result_file, append=TRUE)
