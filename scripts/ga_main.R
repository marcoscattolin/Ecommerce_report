library(tidyverse)              #version 1.1.1
library(readxl)                 #version 1.0.0
library(lubridate)              #version 1.6.0
library(RGoogleAnalytics)       #version 0.1.1
library(stringr)                #version 1.2.0



temp <- ga_get_views(brand = "Prada", start_date = ymd("20170501"), end_date = ymd("20170503"))


temp %>% 
        group_by(custom_gouping) %>% 
        summarise_at(vars(sessions:pageviews),sum) %>% 
        arrange(desc(sessions))
