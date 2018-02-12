library(tidyverse)              #version 1.1.1
library(readxl)                 #version 1.0.0
library(lubridate)              #version 1.6.0
library(magick)                 #version 1.2
library(RGoogleAnalytics)       #version 0.1.1
library(stringr)                #version 1.2.0
library(ISOweek)
library(curl)


message("\n\n\n\n\n\n\n-------- STARTING SCRIPT --------")


source("scripts/helpers/reporting_functions.R")
source("scripts/helpers/sku_utils.R")
source("scripts/helpers/ga_visits_v2.R")

# INPUT -------------------------------------------------------------------
cat("Insert reference date (format yyyy-mm-dd): ")
ref_day <- ymd(readLines(file("stdin"),1))
#ref_day <- ymd("20180211")

# MAKE OVERVIEW -----------------------------------------------------------
message("------ PROCESSING OVERVIEW DATA -------\n")
load_retail(ref_day)
load_ecommerce(ref_day)
add_ecommerce_flag()
add_classe()
add_exchange_rates()
add_totals()
add_sku_metadata()
add_special_flags()
save_datasets()
message("------ PROCESSING OVERVIEW DATA: DONE -------\n\n\n")


# MAKE BEST SELLERS -------------------------------------------------------
message("------ PROCESSING BESTSELLERS DATA -------\n")
bs_clean_data()
bs_get_images(ref_day,top_n = 12)
bs_save_dataset()
message("------ PROCESSING BESTSELLERS DATA: DONE -------\n\n\n")



# GET GOOGLE VISITS DATA ---------------------------------------------------------
message("------ PROCESSING GOOGLE ANALYTICS VISITS DATA -------\n")
visits <- ga_get_views(brand = "Prada",ref_day = ref_day, lookback_weeks = 4, split_daywise = T)
visits <- ga_get_views(brand = "Miu Miu",ref_day = ref_day, lookback_weeks = 4, split_daywise = T) %>% bind_rows(visits)
visits <- visits %>% mutate(ref_day = ref_day) %>% mutate(transactions = case_when(country == "China" ~ 0, TRUE ~ transactions))
ga_views_save_dataset()
message("------ PROCESSING GOOGLE ANALYTICS VISITS DATA: DONE -------\n\n\n")

# GET GOOGLE VISITS DATA V2 ---------------------------------------------------------
message("------ PROCESSING GOOGLE ANALYTICS VISITS DATA V2 -------\n")
visits_v2 <- ga_get_views_v2(brand = "P",ref_day = ref_day, split_daywise = T)
visits_v2 <- ga_get_views_v2(brand = "M",ref_day = ref_day, split_daywise = T) %>% bind_rows(visits_v2)
visits_v2 <- ga_get_views_v2(brand = "MA",ref_day = ref_day, split_daywise = T) %>% bind_rows(visits_v2)
visits_v2 <- ga_get_views_v2(brand = "KS",ref_day = ref_day, split_daywise = T, use_carshoe_raw = T) %>% bind_rows(visits_v2)
visits_v2 <- visits_v2 %>% mutate(ref_day = ref_day) %>% mutate(transactions = case_when(country == "China" ~ 0, TRUE ~ transactions))
ga_views_save_dataset_v2()
message("------ PROCESSING GOOGLE ANALYTICS VISITS DATA V2: DONE -------\n\n\n")

# GET GOOGLE MOST VIEWED DATA ---------------------------------------------------------
message("------ PROCESSING GOOGLE ANALYTICS MOST VIEWED DATA -------\n")
most_viewed <- ga_get_most_viewed(ref_day, "Prada", paginate_query = T)
most_viewed <- ga_get_most_viewed(ref_day, "Miu Miu", paginate_query = T, use_miumiu_mirror = F) %>% bind_rows(most_viewed)
most_viewed_reshape()
most_viewed <- ga_get_most_viewed_china(ref_day, "Prada", paginate_query = F, use_miumiu_mirror = F) %>% bind_rows(most_viewed)
most_viewed_enrich()
most_viewed_add_marketplaces()
most_viewed_get_images(top_n = 12)
most_viewed_save_dataset()
message("------ PROCESSING GOOGLE ANALYTICS MOST VIEWED DATA: DONE -------\n\n\n")


# GET OUT OF STOCK DATA ---------------------------------------------------------
message("------ PROCESSING GOOGLE ANALYTICS OUT OF STOCK DATA -------\n")
out_of_stock <- ga_get_out_of_stock(ref_day, "Prada", paginate_query = T)
out_of_stock <- ga_get_out_of_stock(ref_day, "Miu Miu", paginate_query = T, use_miumiu_mirror = F) %>% bind_rows(out_of_stock)
ga_out_of_stock_enrich()
out_of_stock_save_dataset()
message("------ PROCESSING GOOGLE ANALYTICS OUT OF STOCK DATA: DONE -------\n\n\n")


# ANOMALY DETECTION -------------------------------------------------------
message("------ EVALUATING LAST WEEK ANOMALIES -------\n")
make_weekly_data()
low_hi_sales_anomalies(ref_day)
no_sales_anomalies(ref_day)
price_anomalies(ref_day)
message("------ EVALUATING LAST WEEK ANOMALIES: DONE -------\n")





# END OF SCRIPT -----------------------------------------------------------
cat("Script completed, hit Return to finish...")
a <- readLines(file("stdin"),1)
