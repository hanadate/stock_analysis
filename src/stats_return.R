return_rs_prices <- readRDS("doc/return_rs_prices_2.rds")
return_quantile <- as.data.frame(do.call(cbind, return_rs_prices)) %>% 
  rownames_to_column(var="date") %>% 
  pivot_longer(., cols=!date, names_to="sector", values_to="return") %>% 
  drop_na() %>% 
  group_by(date) %>% 
  mutate(max_return=max(return, na.rm=TRUE)) %>% 
  filter(return>=max_return) %>% 
  group_by(sector) %>% 
  summarise(
    Q1 = as.character(round(exp(quantile(return, 0.25)),3)),
    Q2 = as.character(round(exp(median(return)),3)),
    Q3 = as.character(round(exp(quantile(return, 0.75)),3))
  ) %>% 
  mutate(sector=str_remove(sector, "_return_rs"))
return_quantile

# write results
write_lines("===Quantile of winner's return===", file=result_file, append=TRUE)
write.table(return_quantile, result_file, row.names=FALSE, append=TRUE)

