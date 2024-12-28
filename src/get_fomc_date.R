library(rvest)
library(stringr)
library(lubridate)
url <- paste0(
  "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"
)
fomc_html <- read_html(url)

fomc_html %>% 
  html_elements(".panel-default")

fomc_year <- fomc_html %>%
  html_elements(".panel-default") %>% 
  html_elements(".panel-heading") %>% 
  html_text2() %>% 
  .[str_detect(.,"FOMC Meetings")] %>% 
  str_extract("\\d+")

fomc_month <- fomc_html %>% 
  html_elements(".fomc-meeting") %>% 
  html_elements(".fomc-meeting__month") %>% 
  html_text2() %>% 
  .[str_detect(.,"Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec")] %>% 
  ifelse(str_detect(.,"/"),str_extract(.,"(?<=/).*"),.)
# fomc_month %>% length 
fomc_ym <- paste(fomc_year[cumsum(str_detect(fomc_month, "Jan"))], fomc_month)

fomc_date <- fomc_html %>% 
  html_elements(".fomc-meeting") %>% 
  html_elements(".fomc-meeting__date") %>%
  html_text2() %>% 
  ifelse(str_detect(.,"-"),str_extract(.,"(?<=-).*"),.) %>% 
  str_extract("\\d+")
# fomc_date %>% length() 

fomc_meetings <- as_date(paste(fomc_ym, fomc_date))
